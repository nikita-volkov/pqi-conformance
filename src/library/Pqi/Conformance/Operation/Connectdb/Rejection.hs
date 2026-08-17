-- | Coverage for a mid-handshake server rejection: the server accepts the TCP
-- connection, starts writing an error frame back (mirroring how it sheds load
-- with e.g. \"sorry, too many clients already\"), then closes the socket
-- before the frame is complete. A sound adapter reports this as a classified,
-- \'ConnectionBad\' failure - the same way it reports any other rejected
-- handshake - instead of letting the underlying I\/O exception escape
-- 'Pqi.connectdb'.
--
-- Found in @pqi-native@ (<https://github.com/nikita-volkov/pqi-native/issues/8>):
-- 'establish' only wrapped the initial TCP connect in an exception handler,
-- leaving the handshake read that follows it able to throw an uncaught
-- 'System.IO.Error.IOException' whenever the server's rejection didn't arrive
-- as a complete frame.
--
-- Unlike most operation specs, this one drives a raw listener instead of the
-- shared PostgreSQL container: the failure is about how an adapter reacts to
-- a truncated read, which a real server only triggers racily (e.g. under
-- @max_connections@ pressure). A hand-rolled listener reproduces the exact
-- byte pattern deterministically.
module Pqi.Conformance.Operation.Connectdb.Rejection
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
    describe "a mid-handshake server rejection" do
      it "the candidate reports a classified error like the reference, instead of throwing" \_ -> do
        candidate <- attempt adapter
        reference <- attempt Reference.adapter
        candidate `shouldBe` reference

-- | Run 'Pqi.connectdb' against 'withRejectingServer' and report the
-- resulting status, or the exception's 'Show'n form if one escaped - which is
-- exactly what should never happen.
attempt :: Pqi.Adapter -> IO (Either String Pqi.ConnStatus)
attempt adapter =
  withRejectingServer \port -> do
    let conninfo =
          "host=127.0.0.1 port=" <> ByteString.Char8.pack (show port) <> " dbname=x user=x"
    result <- try @SomeException (Pqi.connectdb adapter conninfo)
    case result of
      Left err -> pure (Left (show err))
      Right connection -> do
        observedStatus <- Pqi.status connection
        Pqi.finish connection
        pure (Right observedStatus)

-- | Bind a loopback listener on an ephemeral port and hand its port number to
-- the action, while a background thread accepts a single connection on it,
-- reads whatever the client has sent so far, writes the first three bytes of
-- an \'E\'rrorResponse frame (a type byte and two of its four length bytes)
-- and closes - never completing the frame.
--
-- The truncated write is safe to race against the client: the kernel queues
-- the connection at 'Socket.listen' time, so the client's own 'connect' and
-- initial 'send' succeed regardless of whether the server thread has reached
-- 'Socket.accept' yet, and the client only blocks once it starts reading the
-- (never-completed) response.
withRejectingServer :: (Socket.PortNumber -> IO a) -> IO a
withRejectingServer action =
  bracket open Socket.close \listener -> do
    port <- Socket.socketPort listener
    _ <- forkIO (try @SomeException (serveOneRejection listener) >> pure ())
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
      Socket.listen sock 1
      pure sock

serveOneRejection :: Socket.Socket -> IO ()
serveOneRejection listener = do
  (conn, _) <- Socket.accept listener
  _ <- Socket.ByteString.recv conn 4096
  Socket.ByteString.sendAll conn (ByteString.pack [0x45, 0x00, 0x00])
  Socket.close conn
