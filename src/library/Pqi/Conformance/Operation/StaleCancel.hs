-- | Deterministic regression coverage for the stale-cancel bug: a cancel
-- dispatched during pipeline clean-up must not corrupt the next command run on
-- the connection.
--
-- Root cause: 'Pqi.cancel' opens a TCP connection to the postmaster, sends the
-- @CancelRequest@, and — in the buggy implementation — immediately closes its
-- end.  The request can then sit unread in the postmaster's socket buffer while
-- the client races ahead and issues its next query.  The postmaster eventually
-- reads the request and delivers @SIGINT@ to the backend, which by then is
-- executing the /next/ statement, cancelling it with SQLSTATE @57014@.
--
-- The race is reliably opened by the pipeline clean-up sequence that
-- @hasql@ runs after a timeout fires mid-read: once the pipeline is aborted,
-- 'Pqi.getResult' returns synthetic results for the outstanding commands
-- __without blocking on the wire__ (see @getNextResult@ in
-- @Pqi.Native.Query@).  That lets the client reach its follow-up query before
-- the postmaster has processed the cancel.
--
-- The fix mirrors libpq's @PQcancel@: after sending the request, drain the
-- cancel socket until the server closes its end (which it only does after the
-- signal has been dispatched), so the cancel can no longer land on a later
-- command.
--
-- A single run hits the race only intermittently (the postmaster usually
-- processes the cancel in time), so this spec replays the scenario many times.
-- On the buggy implementation at least one iteration reliably loses the race
-- and reports @57014@ on the victim query; the fixed implementation passes
-- every iteration.
module Pqi.Conformance.Operation.StaleCancel
  ( spec,
  )
where

import Control.Exception (bracket)
import Pqi (ExecStatus (..), FieldCode (..), IsCancel (..), IsConnection (..))
import qualified Pqi as Lq
import Pqi.Conformance.Observation (ResultObservation (..))
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, execScenario, float8Oid)
import System.Timeout (timeout)
import Test.Hspec

-- | How many times to replay the racy scenario.  Each iteration that loses the
-- race on buggy code surfaces SQLSTATE @57014@ on the victim query; enough
-- iterations make at least one failure overwhelmingly likely (the underlying
-- per-run failure rate on buggy code is roughly 30%).
iterations :: Int
iterations = 30

spec :: forall c. (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "stale cancel" do
    it "does not corrupt the next command when a cancel is sent during pipeline clean-up" \conninfo -> do
      outcomes <- for [1 .. iterations] \i -> do
        outcome <- runScenario proxy conninfo
        pure (i, outcome)
      -- Every iteration's victim query must have succeeded.  Any iteration that
      -- did not means a stale cancel corrupted the connection (57014).
      let corrupted = [(i, outcome) | (i, Just outcome) <- outcomes]
      corrupted `shouldBe` []

-- | Run one stale-cancel scenario on a fresh connection.  Returns 'Nothing' if
-- the victim query succeeded, or @Just (status, sqlstate)@ describing how it
-- failed (e.g. @(FatalError, Just "57014")@).
runScenario ::
  forall c.
  (IsConnection c) =>
  Proxy c ->
  ByteString ->
  IO (Maybe (ExecStatus, Maybe ByteString))
runScenario _ conninfo =
  bracket (connectdb conninfo :: IO c) finish \connection -> do
    -- Build a pipeline with a fast query followed by a slow one, mirroring the
    -- sequence hasql issues.  Two prepared statements keep pendingParses in
    -- play, matching the real-world reproduction.
    _ <- enterPipelineMode connection
    _ <- sendPrepare connection "s1" "select $1::int" Nothing
    _ <- sendQueryPrepared connection "s1" [Just ("42", Lq.Text)] Lq.Text
    _ <- sendPrepare connection "s2" "select pg_sleep($1)" (Just [float8Oid])
    _ <- sendQueryPrepared connection "s2" [Just ("0.1", Lq.Text)] Lq.Text
    _ <- pipelineSync connection

    -- Interrupt the read mid-pipeline, exactly as hasql's timeout does: the
    -- slow pg_sleep is still running when the read is abandoned.
    _ <- timeout 50_000 (drainResults connection)

    -- cleanUpAfterInterruption: drain, cancel, drain.  The cancel is sent while
    -- the pipeline is mid-flight; on buggy code its SIGINT can arrive late.
    _ <- drainResults connection
    mHandle <- getCancel connection
    _ <- for mHandle cancel
    _ <- drainResults connection

    -- leavePipeline: the exact restore sequence hasql performs, including the
    -- retry it falls back to.
    statusBefore <- pipelineStatus connection
    when (statusBefore == Lq.PipelineOn) do
      _ <- pipelineSync connection
      _ <- drainResults connection
      _ <- sendFlushRequest connection
      _ <- drainResults connection
      ok <- exitPipelineMode connection
      unless ok do
        _ <- drainResults connection
        void (exitPipelineMode connection)

    -- The victim.  It must not be cancelled by the stale signal.
    execScenario "select 99" connection >>= \case
      Nothing -> pure (Just (FatalError, Just "no-result"))
      Just observation ->
        case observation.status of
          TuplesOk -> pure Nothing
          status -> pure (Just (status, fromMaybe Nothing (lookup DiagSqlstate observation.errorFields)))
