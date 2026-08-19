-- | Coverage for a send onto a connection whose write side is already gone -
-- contrast 'Pqi.Conformance.Operation.Exec.ConnectionLostMidQuery' (the
-- connection dies while a result is in flight, so the /read/ side fails) and
-- 'Pqi.Conformance.Operation.Connectdb.HandshakeReset' (the connection dies
-- during the handshake). The connection is opened normally, then its own
-- socket is shut down on the write side directly (@shutdown(fd, SHUT_WR)@) -
-- a portable, deterministic way to make the next send fail with @EPIPE@
-- without depending on a peer's timing. A peer-initiated reset was tried
-- first and found unreliable for this: a single write issued well after the
-- peer had already reset the connection was still silently accepted into
-- the local kernel send buffer (observed on this platform even a full
-- second later), so there was no window in which a write was guaranteed to
-- fail. Shutting down the write side locally leaves no such window.
--
-- The shutdown call goes straight through a raw FFI @shutdown(2)@ on the
-- bare descriptor, deliberately never constructing a @Network.Socket@
-- 'Socket' value over it. An earlier version wrapped the connection's own
-- live descriptor with 'Network.Socket.mkSocket' to call
-- 'Network.Socket.shutdown' the ordinary way, and that made some later,
-- unrelated spec hang - reliably, and only when this spec ran at all -
-- while this spec itself always passed in isolation. The leading theory is
-- that constructing a second 'Socket' over an fd the connection already
-- owns perturbs GHC's I\/O-manager registration for it (kqueue on this
-- platform), and the damage doesn't surface until that fd number is later
-- reused by an unrelated connection. A raw syscall on the 'System.Posix.Types.Fd'
-- touches only kernel-level socket state, nothing GHC-side.
--
-- The scenario compares the connection's status and error message after
-- sending and draining, not the full shape of what came back: libpq does
-- not necessarily flush a query this small onto the wire inside
-- @PQsendQuery@ itself - it can queue it and report success optimistically,
-- deferring the actual write to the next call that needs to wait on the
-- server, i.e. @PQgetResult@ (observed directly below - the reference's
-- @sendQuery@ reports success, and the failure surfaces as a single
-- @FatalError@ result from the drain instead). @pqi-native@ has no such
-- buffering - every send is an immediate, real write - so its failure
-- surfaces one step earlier, at @sendQuery@ itself, with nothing left to
-- drain. That difference in *when* the failure is discovered is a benign
-- consequence of two legitimately different I\/O strategies, not something
-- this fix should or could paper over. What must still agree, and is what
-- 'Hasql.Comms.Roundtrip.runSend' actually inspects to decide
-- @connectionLost@, is the connection's final state: marked bad, with
-- libpq's own wording for the loss.
--
-- Was found in @pqi-native@: 'Pqi.Native.Transport.send' is
-- @Network.Socket.ByteString.sendAll@, which throws an 'System.IO.Error.IOException'
-- on @EPIPE@\/@ECONNRESET@ rather than reporting the failure through a
-- return value. 'Pqi.Native.Query.sendAsync' - the function every one of
-- 'Pqi.sendQuery', @sendQueryParams@, @sendPrepare@, @sendQueryPrepared@,
-- @sendDescribePrepared@ and @sendDescribePortal@ is built on - called it
-- with no exception handler at all, and only updated the connection's
-- bookkeeping (marking it usable, recording the pending command) on the
-- path where sending never throws. A dead socket therefore made
-- 'Pqi.sendQuery' throw a raw 'System.IO.Error.IOException' instead of
-- returning 'False', unlike libpq's @PQsendQuery@, which never throws: a
-- fatal send marks @PQstatus@ @CONNECTION_BAD@ and returns @0@, leaving
-- @PQerrorMessage@ to carry the same "server closed the connection
-- unexpectedly" wording it uses for a connection lost while reading.
--
-- This is also the entry point 'Hasql.Session' actually exercises for
-- every ordinary (non-pipelined) statement - @Hasql.Comms.Send@ calls
-- 'Pqi.sendQuery' directly, never the synchronous 'Pqi.exec' family - so an
-- uncaught exception here is what let a plain network drop during a send
-- escape 'Hasql.Connection.use's @try \@SomeException@ as an
-- "interruption" instead of a classified error, triggering interruption
-- cleanup against an already-dead connection.
--
-- Fixed by catching 'System.IO.Error.IOException' around the send in
-- 'Pqi.Native.Query.sendAsync' (and the sibling direct sends in
-- 'Pqi.Native.pipelineSync'\/'sendFlushRequest') and converting it into the
-- same connection-bad classification
-- 'Pqi.Native.Connection.markConnectionLost' already gave the read side.
module Pqi.Conformance.Operation.SendQuery.ConnectionLostBeforeSend
  ( spec,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import System.Posix.Types (Fd (..))
import Test.Hspec

foreign import ccall unsafe "shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

-- | @shutdown(fd, SHUT_WR)@ on the raw descriptor. @1@ is @SHUT_WR@\/@SD_SEND@,
-- a POSIX-standard value shared by Linux, the BSDs (so macOS) and Winsock
-- alike - stable enough to hardcode rather than pull in a C header binding
-- for one constant.
shutdownWriteSide :: Fd -> IO ()
shutdownWriteSide (Fd fd) = throwErrnoIfMinus1_ "shutdown" (c_shutdown fd 1)

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "sendQuery" do
    describe "a connection whose write side is already gone" do
      it "the candidate ends up marked bad like the reference, instead of throwing" \conninfo ->
        differential adapter conninfo scenario

-- | Shut down the connection's own socket on the write side, then send a
-- query - draining its results only if the send itself was accepted, since
-- @pqi-native@ always leaves nothing pending when it wasn't - and report
-- the connection's resulting status and error message, or the exception's
-- 'Show'n form if one escaped, which is exactly what should never happen.
scenario :: Pqi.Connection -> IO (Either String (Pqi.ConnStatus, Maybe ByteString))
scenario connection = do
  mFd <- Pqi.socket connection
  case mFd of
    Nothing -> error "pqi-conformance: connected connection reported no socket"
    Just fd -> shutdownWriteSide fd
  outcome <- try @SomeException do
    sent <- Pqi.sendQuery connection "select 1"
    when sent (void (drainResults connection))
  case outcome of
    Left err -> pure (Left (show err))
    Right () -> do
      status <- Pqi.status connection
      errorMessage <- Pqi.errorMessage connection
      pure (Right (status, errorMessage))
