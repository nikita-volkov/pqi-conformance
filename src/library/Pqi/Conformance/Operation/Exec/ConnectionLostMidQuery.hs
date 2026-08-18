-- | Coverage for a connection reset while a query response is in flight -
-- after a valid handshake, not during one (contrast
-- 'Pqi.Conformance.Operation.Connectdb.HandshakeReset|). A hand-rolled
-- server completes the startup handshake normally (@AuthenticationOk@ then
-- @ReadyForQuery@), reads the client's @Query@ message, then resets the
-- connection - via @SO_LINGER@ with a zero timeout, so the close sends a
-- TCP @RST@ - instead of ever sending a result.
--
-- Found in @pqi-native@: none of 'Pqi.Native.Query.exec',
-- @execParams@, @prepare@, @execPrepared@, @describePrepared@, or
-- @describePortal@ wrap their read loop in any exception handler at all -
-- unlike 'Pqi.Native.Connection.establish', which at least classifies a
-- broken read into a 'Pqi.errorMessage' (see 'HandshakeReset' and
-- 'Pqi.Conformance.Operation.Connectdb.UnresolvableHost' for cases where
-- that classification itself is wrong). A connection lost mid-query
-- instead escapes 'Pqi.exec' as a raw, uncaught 'System.IO.Error.IOException',
-- crashing the caller's thread outright rather than surfacing as any kind
-- of classified failure. libpq itself never throws here: @PQexec@ returns a
-- result with 'Pqi.FatalError' status and its own \"server closed the
-- connection unexpectedly\" message, the same wording it uses for a reset
-- during the handshake.
--
-- Deliberately left failing, same as
-- 'Pqi.Conformance.Operation.Connectdb.HandshakeReset': the mismatch it
-- demonstrates - a classified failure on one side, an uncaught exception on
-- the other - is the point.
module Pqi.Conformance.Operation.Exec.ConnectionLostMidQuery
  ( spec,
  )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, try)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString
import qualified Pqi
import Pqi.Conformance.Prelude
import qualified Pqi.Conformance.Reference as Reference
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "exec" do
    describe "a connection reset while a query response is in flight" do
      it "the candidate reports a classified failed result like the reference, instead of throwing" \_ ->
        -- Both attempts share one listener (and so one port): the failure
        -- message embeds the port number, and the candidate and reference
        -- would otherwise always disagree on that one detail despite
        -- matching in every way that matters.
        withHandshakingThenResettingServer \port -> do
          candidate <- attempt adapter port
          reference <- attempt Reference.adapter port
          candidate `shouldBe` reference

-- | Connect, run one query against the given port (see
-- 'withHandshakingThenResettingServer'), and report the resulting result's
-- status and error message, or the exception's 'Show'n form if one escaped
-- 'Pqi.exec' - which is exactly what should never happen.
--
-- @sslmode=disable@ keeps this comparable across adapters: libpq negotiates
-- SSL before the startup packet by default, so without it the exchange
-- would never reach the fake server's plain-protocol handshake, which
-- @pqi-native@ (which never attempts SSL) does not even send. The host is
-- given as a literal IP rather than a name so a failure message doesn't
-- gain a resolved-IP parenthetical the candidate would then also have to
-- reproduce.
attempt :: Pqi.Adapter -> Socket.PortNumber -> IO (Either String (Maybe (Pqi.ExecStatus, Maybe ByteString)))
attempt adapter port = do
  let conninfo =
        "host=127.0.0.1 port="
          <> ByteString.Char8.pack (show port)
          <> " dbname=x user=x sslmode=disable"
  bracket (Pqi.connectdb adapter conninfo) Pqi.finish \connection -> do
    outcome <- try @SomeException (Pqi.exec connection "select 1")
    case outcome of
      Left err -> pure (Left (show err))
      Right mResult -> do
        observed <- traverse (\r -> (,) <$> Pqi.resultStatus r <*> Pqi.resultErrorMessage r) mResult
        pure (Right observed)

-- | Bind a loopback listener on an ephemeral port and hand its port number
-- to the action, while a background thread serves every connection made to
-- it in turn: complete a minimal but valid startup handshake
-- (@AuthenticationOk@ then @ReadyForQuery@), read whatever the client sends
-- next (its @Query@ message), then reset the connection - via @SO_LINGER@
-- with a zero timeout, which makes the kernel send a @RST@ instead of the
-- usual @FIN@ on close - rather than ever answering it.
withHandshakingThenResettingServer :: (Socket.PortNumber -> IO a) -> IO a
withHandshakingThenResettingServer action =
  bracket open Socket.close \listener -> do
    port <- Socket.socketPort listener
    _ <- forkIO (try @SomeException (forever (serveOneQueryThenReset listener)) >> pure ())
    action port
  where
    open = do
      address : _ <-
        Socket.getAddrInfo
          (Just Socket.defaultHints {Socket.addrSocketType = Socket.Stream})
          (Just "127.0.0.1")
          (Just "0")
      sock <- Socket.socket (Socket.addrFamily address) (Socket.addrSocketType address) (Socket.addrProtocol address)
      Socket.bind sock (Socket.addrAddress address)
      Socket.listen sock 8
      pure sock

serveOneQueryThenReset :: Socket.Socket -> IO ()
serveOneQueryThenReset listener = do
  (conn, _) <- Socket.accept listener
  _ <- Socket.ByteString.recv conn 4096 -- the startup packet
  Socket.ByteString.sendAll conn authenticationOk
  Socket.ByteString.sendAll conn readyForQuery
  _ <- Socket.ByteString.recv conn 4096 -- the Query message
  Socket.setSockOptValue conn Socket.Linger (Socket.SockOptValue (Socket.StructLinger 1 0))
  Socket.close conn
  where
    -- 'R', length 8 (self-inclusive), auth type 0 (Ok).
    authenticationOk = ByteString.pack [0x52, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00]
    -- 'Z', length 5 (self-inclusive), status 'I' (idle).
    readyForQuery = ByteString.pack [0x5A, 0x00, 0x00, 0x00, 0x05, 0x49]
