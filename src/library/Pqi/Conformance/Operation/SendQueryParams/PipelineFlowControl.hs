-- | Coverage for pipeline flow control: a burst of pipelined commands larger
-- than the socket buffers along the path can absorb, submitted back-to-back
-- before a single result is read, where every command produces output of its
-- own. Submitting such a burst must not deadlock: a client that still has
-- bytes to write while the server streams unread results back has to keep
-- /reading/ the socket while sending, so the server's output path drains and
-- the server keeps consuming the client's commands.
--
-- The burst is submitted through 'Pqi.sendQueryParams' because @libpq@
-- rejects @PQsendQuery@ itself in pipeline mode (\"PQsendQuery not allowed
-- in pipeline mode\") - only the extended protocol is accepted there. The
-- flow-control property being exercised is not specific to it, though: every
-- send operation goes through the same driver-level flush, which is where
-- the divergence lived.
--
-- The divergence: @pqi-native@'s 'Pqi.Native.Transport.send' was a bare
-- blocking @sendAll@, which never reads while sending. Once the server has
-- filled its output path with results, it blocks in @ClientWrite@ and stops
-- reading the client's commands; the client, with bytes still unsent,
-- blocks in @send@. Neither side can make progress again. The real @libpq@
-- survives the same scenario: @pqSendSome@ in @fe-misc.c@, on an incomplete
-- send, calls @pqReadData()@ to absorb incoming data and then
-- @pqWait(true, true, …)@ for read-or-write readiness, looping until its
-- output is fully sent - the comment above that loop describes this exact
-- deadlock.
--
-- Where it surfaced: hasql's @manyLargeResultsViaPipeline@ benchmark hung
-- (non-deterministically across runs, but always within the 100-command
-- pipeline) from @pqi-native@ 1.0.1.4 onward, after its default host
-- resolution moved from TCP @localhost@ to the Unix-domain socket (@\/tmp@).
-- The deadlock was always there; the Unix socket merely exposed it, because
-- its default 8KB+8KB buffers saturate on a ~10-16KB command burst, where
-- TCP's larger buffers absorb the same burst without the client's send ever
-- blocking.
--
-- Why the burst is as large as it is: this suite reaches its container
-- through Docker's published-port forwarding, and on a workstation that
-- path buffers far more than the raw socket buffers - measured on this
-- platform (macOS host, Docker Desktop), ~8.5MB of commands are accepted
-- with the server never reading (send-buffer autotuning plus the
-- forwarder's own buffering), and ~5.4MB of results are accepted with the
-- client never reading (receive-buffer autotuning capped at 4MB plus the
-- forwarder's buffering). A direct kernel route (a Linux CI host, a Unix
-- socket) exposes only the true socket buffers, an order of magnitude less.
-- The scenario therefore sends 1000 commands of ~32KB each - ~32MB of wire
-- traffic, more than double the sum of both directions' absorption even on
-- the forwarding path - with each command returning an ~32KB result, so
-- output expands to roughly the size of the input. By buffer arithmetic the
-- server's output path fills while it has consumed only a fraction of the
-- burst, and the client cannot finish sending unless the server consumes
-- several more MB, which it cannot do while the client reads nothing: both
-- sides are guaranteed stuck, whatever the platform's capacities are below
-- the burst's.
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
module Pqi.Conformance.Operation.SendQueryParams.PipelineFlowControl
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

-- | The payload each command carries: 32KB, so a command is ~32KB on the
-- wire (extended-protocol framing included) and returns an ~32KB result -
-- output expands to roughly the size of the input, so the server's output
-- path fills while it has consumed only a small fraction of the burst.
statementLiteral :: ByteString
statementLiteral = ByteString.Char8.replicate 32768 'x'

statement :: ByteString
statement = "select repeat('" <> statementLiteral <> "', 1)"

-- | 1000 commands, ~32MB of wire traffic in all - more than double what the
-- container's forwarding path can absorb in /both/ directions combined on a
-- workstation (measured ~8.5MB of unread input, ~5.4MB of unread output),
-- and far more than a direct kernel route can (~0.4MB). The client cannot
-- finish sending unless the server keeps reading, which it cannot do unless
-- the client keeps draining results.
commandCount :: Int
commandCount = 1000

-- | The reference collects the whole pipeline in seconds. Anything that
-- cannot within a minute is not slow but deadlocked.
deadlineMicros :: Int
deadlineMicros = 60_000_000

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "sendQueryParams" do
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
  sent <- traverse (\_ -> Pqi.sendQueryParams connection statement [] Pqi.Text) [1 .. commandCount]
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
      ( void
          ( Exception.try @Exception.SomeException action
              >>= deliver
              . either (Left . show) Right
          )
      )
  timer <-
    Concurrent.forkIO
      ( Concurrent.threadDelay micros
          >> void (deliver (Left ("deadline exceeded: no outcome within " <> show micros <> "us")))
      )
  outcome <- Concurrent.takeMVar box
  Concurrent.killThread timer
  pure outcome
