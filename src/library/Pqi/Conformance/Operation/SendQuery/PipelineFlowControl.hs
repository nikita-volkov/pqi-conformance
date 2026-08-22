-- | Coverage for pipeline flow control: a burst of pipelined commands larger
-- than the socket buffers along the path can absorb, submitted back-to-back
-- before a single result is read, where every command produces output of its
-- own. Submitting such a burst must not deadlock: a client that still has
-- bytes to write while the server streams unread results back has to keep
-- /reading/ the socket while sending, so the server's output path drains and
-- the server keeps consuming the client's commands.
--
-- The divergence: @pqi-native@'s 'Pqi.Native.Transport.send' was a bare
-- blocking @sendAll@, which never reads while sending. Once the server has
-- filled its send buffer and the client's receive buffer with results, it
-- blocks in @ClientWrite@ and stops reading the client's commands; the
-- client, with bytes still unsent, blocks in @send@. Neither side can make
-- progress again. The real @libpq@ survives the same scenario:
-- @pqSendSome@ in @fe-misc.c@, on an incomplete send, calls @pqReadData()@
-- to absorb incoming data and then @pqWait(true, true, …)@ for
-- read-or-write readiness, looping until its output is fully sent - the
-- comment above that loop describes this exact deadlock.
--
-- Where it surfaced: hasql's @manyLargeResultsViaPipeline@ benchmark hung
-- (non-deterministically across runs, but always within the 100-command
-- pipeline) from @pqi-native@ 1.0.1.4 onward, after its default host
-- resolution moved from TCP @localhost@ to the Unix-domain socket (@\/tmp@).
-- The deadlock was always there; the Unix socket merely exposed it, because
-- its default 8KB+8KB buffers saturate on a ~10-16KB command burst, where
-- TCP's 128KB+128KB absorb the same burst without the client's send ever
-- blocking. This suite reaches its container over TCP, so the scenario
-- scales the burst until the TCP path saturates the same way: 1000
-- statements of ~2KB each (~2.1MB of commands) against the ~300-400KB of
-- combined kernel buffering on the way to the container. Each statement
-- returns an ~8KB result, so after a few dozen statements the server is
-- blocked writing while the client still has megabytes unsent - the deadlock
-- is forced by buffer arithmetic, not by timing.
--
-- Why this observation: the failure mode is a hang, not a wrong value, and a
-- hung candidate must fail the spec rather than stall the whole suite. The
-- scenario therefore runs under a deadline and the /bounded outcome/ is what
-- gets compared: the reference completes within seconds and reports the
-- aggregate shape of the pipeline, while the deadlocked candidate never
-- returns and is reported as a deadline miss. When the deadline fires, the
-- worker thread is abandoned rather than killed - it is blocked inside a
-- foreign call no async exception can interrupt - and the harness's bracket
-- closes the connection, so the server sees EOF and exits. The results are
-- aggregated to counts rather than observed per-result: at this scale full
-- per-result observations would be enormous, per-statement agreement is
-- already covered by the per-operation specs, and what this scenario is
-- about is that all 1000 commands complete 'Pqi.TuplesOk' and the pipeline
-- reaches its sync.
module Pqi.Conformance.Operation.SendQuery.PipelineFlowControl
  ( spec,
  )
where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

-- | The payload each statement carries: 2KB, so a statement is ~2.1KB on the
-- wire while returning an ~8KB result - output expands ~4x over input.
statementLiteral :: ByteString
statementLiteral = ByteString.Char8.replicate 2048 'x'

statement :: ByteString
statement = "select repeat('" <> statementLiteral <> "', 4)"

-- | 1000 statements, ~2.1MB of commands in all - far more than the client's
-- send buffer and the server's receive buffer hold together (~300-400KB over
-- the container's TCP link), so the client cannot finish sending unless the
-- server keeps reading, which it cannot do unless the client keeps draining
-- results.
commandCount :: Int
commandCount = 1000

-- | The reference collects the whole pipeline in seconds. Anything that
-- cannot within a minute is not slow but deadlocked.
deadlineMicros :: Int
deadlineMicros = 60_000_000

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "sendQuery" do
    describe "pipeline flow control" do
      it "sends a burst larger than the socket buffers while results stream back, without deadlocking" \conninfo ->
        differential adapter conninfo scenario

-- | Pipeline @statement@ @commandCount@ times, sync, then collect the whole
-- pipeline: every command's result and separator, the sync result, and the
-- idle end. The observation aggregates them (how many commands, how many
-- came back 'Pqi.TuplesOk', the total tuple count, any separator that wasn't
-- the expected empty one, the sync status, whether the stream really ended).
scenario :: Pqi.Connection -> IO (Either String (Bool, Int, Bool, Int, Int, Int32, Int, Maybe Pqi.ExecStatus, Bool, Bool))
scenario connection = withDeadline deadlineMicros do
  entered <- Pqi.enterPipelineMode connection
  sent <- traverse (\_ -> Pqi.sendQuery connection statement) [1 .. commandCount]
  synced <- Pqi.pipelineSync connection
  (commands, okCount, tuples, separatorAnomalies) <- collectCommands 0 0 0 0
  syncResult <- Pqi.getResult connection
  syncStatus <- traverse Pqi.resultStatus syncResult
  idle <- Pqi.getResult connection
  exited <- Pqi.exitPipelineMode connection
  pure
    ( entered,
      length (filter id sent),
      synced,
      commands,
      okCount,
      tuples,
      separatorAnomalies,
      syncStatus,
      isNothing idle,
      exited
    )
  where
    collectCommands !commands !okCount !tuples !separatorAnomalies
      | commands >= commandCount = pure (commands, okCount, tuples, separatorAnomalies)
      | otherwise =
          Pqi.getResult connection >>= \case
            Nothing -> pure (commands, okCount, tuples, separatorAnomalies)
            Just result -> do
              status <- Pqi.resultStatus result
              rows <- Pqi.ntuples result
              separator <- Pqi.getResult connection
              collectCommands
                (commands + 1)
                (okCount + (if status == Pqi.TuplesOk then 1 else 0))
                (tuples + rows)
                (separatorAnomalies + maybe 0 (const 1) separator)

-- | Run an action under a deadline, reporting @Left@ if it does not finish
-- in time. Unlike 'System.Timeout.timeout', this /returns/ even when the
-- action is stuck inside a blocking foreign call (where @timeout@'s async
-- exception cannot be delivered and the wait itself would hang): the action
-- runs in a worker thread that is simply abandoned when the deadline passes.
-- Being abandoned is what makes a deadlocked candidate fail the spec instead
-- of freezing the whole suite.
withDeadline :: Int -> IO a -> IO (Either String a)
withDeadline micros action = do
  box <- Concurrent.newEmptyMVar
  let deliver = Concurrent.tryPutMVar box
  _ <-
    Concurrent.forkIO
      ( Exception.try @Exception.SomeException action
          >>= deliver . either (Left . show) Right
      )
  timer <-
    Concurrent.forkIO
      ( Concurrent.threadDelay micros
          >> deliver (Left ("deadline exceeded: no outcome within " <> show micros <> "us"))
      )
  outcome <- Concurrent.takeMVar box
  Concurrent.killThread timer
  pure outcome
