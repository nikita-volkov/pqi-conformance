-- | Coverage for a handshake-time failure that is a hard reset
-- (@ECONNRESET@) rather than a clean EOF: the server accepts the TCP
-- connection, reads the startup packet, then tears the socket down with
-- @SO_LINGER@ set to a zero timeout so the close sends a TCP @RST@ instead
-- of the usual @FIN@.
--
-- Unlike a graceful close (see
-- 'Pqi.Conformance.Operation.Connectdb.Rejection', which truncates an
-- @ErrorResponse@ frame and closes normally), a reset is a genuinely
-- different I\/O failure shape, and was not routed through the same code
-- path in @pqi-native@:
-- 'Pqi.Native.Connection.handshakeFailureMessage' special-cased an EOF
-- (\"server closed the connection unexpectedly\", matching libpq's own
-- wording) but fell back to the raw 'Show'n 'System.IO.Error.IOException'
-- for anything else - and @ECONNRESET@ landed in that \"anything else\"
-- branch.
--
-- Was found in @pqi-native@
-- (<https://github.com/nikita-volkov/hasql/issues/329>): the candidate's
-- 'Pqi.errorMessage' for this failure was e.g. @Network.Socket.recvBuf:
-- resource vanished (Connection reset by peer)@, entirely unlike libpq's
-- own \"server closed the connection unexpectedly\" sentence for the same
-- underlying reset. Fixed by widening
-- 'Pqi.Native.Connection.handshakeFailureMessage's classification to
-- also recognize a hard reset (@ioe_type == ResourceVanished@), not just
-- a clean EOF.
module Pqi.Conformance.Operation.Connectdb.HandshakeReset
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
  describe "connectdb" do
    describe "a handshake-time connection reset (not a clean EOF)" do
      it "the candidate reports a failure message shaped like the reference's" \_ ->
        -- Both attempts share one listener (and so one port): the failure
        -- message embeds the port number, and the candidate and reference
        -- would otherwise always disagree on that one detail despite
        -- matching in every way that matters.
        withResettingServer \port -> do
          candidate <- attempt adapter port
          reference <- attempt Reference.adapter port
          candidate `shouldBe` reference

-- | Run 'Pqi.connectdb' against the given port (see 'withResettingServer')
-- and report the resulting status and error message, or the exception's
-- 'Show'n form if one escaped - which is exactly what should never happen.
--
-- @sslmode=disable@ keeps this comparable across adapters: libpq negotiates
-- SSL before the startup packet by default, so without it the reset would
-- land during a preamble @pqi-native@ (which never attempts SSL) does not
-- even send. The host is given as a literal IP rather than a name so
-- libpq's failure message doesn't gain a resolved-IP parenthetical the
-- candidate would then also have to reproduce.
attempt :: Pqi.Adapter -> Socket.PortNumber -> IO (Either String (Pqi.ConnStatus, Maybe ByteString))
attempt adapter port = do
  let conninfo =
        "host=127.0.0.1 port="
          <> ByteString.Char8.pack (show port)
          <> " dbname=x user=x sslmode=disable"
  result <- try @SomeException (Pqi.connectdb adapter conninfo)
  case result of
    Left err -> pure (Left (show err))
    Right connection -> do
      observedStatus <- Pqi.status connection
      observedError <- Pqi.errorMessage connection
      Pqi.finish connection
      pure (Right (observedStatus, observedError))

-- | Bind a loopback listener on an ephemeral port and hand its port number
-- to the action, while a background thread serves every connection made to
-- it in turn: read whatever the client has sent so far (the startup
-- packet), then reset the connection - via @SO_LINGER@ with a zero timeout,
-- which makes the kernel send a @RST@ instead of the usual @FIN@ on close -
-- rather than closing it gracefully.
withResettingServer :: (Socket.PortNumber -> IO a) -> IO a
withResettingServer action =
  bracket open Socket.close \listener -> do
    port <- Socket.socketPort listener
    _ <- forkIO (try @SomeException (forever (serveOneReset listener)) >> pure ())
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

serveOneReset :: Socket.Socket -> IO ()
serveOneReset listener = do
  (conn, _) <- Socket.accept listener
  _ <- Socket.ByteString.recv conn 4096
  Socket.setSockOptValue conn Socket.Linger (Socket.SockOptValue (Socket.StructLinger 1 0))
  Socket.close conn
