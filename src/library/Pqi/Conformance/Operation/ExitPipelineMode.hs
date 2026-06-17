-- | Coverage for 'Pqi.exitPipelineMode': leaving pipeline mode, which
-- fails while work is still pending and succeeds once the pipeline is drained.
module Pqi.Conformance.Operation.ExitPipelineMode
  ( spec,
  )
where

import Pqi (IsConnection (..))
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, execScenario, float8Oid, takeCommandResults, takeResult)
import System.Timeout (timeout)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "exitPipelineMode" do
    it "returns the connection to its non-pipeline status" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- enterPipelineMode connection
        exited <- exitPipelineMode connection
        after <- pipelineStatus connection
        pure (exited, after)

    it "fails with work pending and succeeds once drained" \conninfo ->
      differential proxy conninfo \connection -> do
        entered <- enterPipelineMode connection
        sent <- sendQueryParams connection "select 1" [] Lq.Text
        prematureExit <- exitPipelineMode connection
        synced <- pipelineSync connection
        results <- takeCommandResults connection
        syncResult <- takeResult connection
        exited <- exitPipelineMode connection
        pure (entered, sent, prematureExit, synced, results, syncResult, exited)

    -- Reproduces the cleanup sequence that hasql's cleanUpAfterInterruption +
    -- leavePipeline performs after a timeout mid-pipeline.  Two prepared
    -- statements are used so that pendingParses is tracked (matching the
    -- real-world scenario).  The slow statement is cancelled before its result
    -- is consumed, then the connection is restored via the exact drain/sync
    -- sequence that hasql uses.
    it "recovers after mid-pipeline cancel (mirrors cleanUpAfterInterruption)" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- enterPipelineMode connection
        _ <- sendPrepare connection "s1" "select 1" Nothing
        _ <- sendQueryPrepared connection "s1" [] Lq.Text
        _ <- sendPrepare connection "s2" "select pg_sleep($1)" (Just [float8Oid])
        _ <- sendQueryPrepared connection "s2" [Just ("0.5", Lq.Text)] Lq.Text
        _ <- pipelineSync connection
        _ <- sendFlushRequest connection
        -- Consume what toPipelineIO would have read before the timeout:
        -- parse1 result + separator, exec1 result + separator, parse2 result + separator.
        _ <- takeCommandResults connection
        _ <- takeCommandResults connection
        _ <- takeCommandResults connection
        -- exec2 (pg_sleep) is still running; cancel it to simulate the
        -- timeout-triggered cancel in cleanUpAfterInterruption.
        mHandle <- getCancel connection
        _ <- for mHandle Lq.cancel
        -- cleanUpAfterInterruption: drain1, then drain2 (after cancel)
        _ <- drainResults connection
        _ <- drainResults connection
        -- leavePipeline: new Sync, drain, Flush, drain
        _ <- pipelineSync connection
        _ <- drainResults connection
        _ <- sendFlushRequest connection
        _ <- drainResults connection
        exited <- exitPipelineMode connection
        pure exited

    -- Reproduces the failure seen in hasql's "Leaves the connection usable
    -- after timeout in pipeline" test.  A fast prepared statement is followed
    -- by a slow one; the whole read phase is wrapped in a short timeout so the
    -- slow statement is interrupted.  The exact cleanup sequence hasql uses is
    -- then applied, and the connection must be left out of pipeline mode and
    -- usable.
    --
    -- The reference (libpq) completes the blocked read before the async
    -- exception is delivered, so the session has already exited pipeline mode
    -- when cleanup starts.  The pqi-native adapter is interrupted mid-read and
    -- currently fails to leave pipeline mode, which is the bug this scenario
    -- captures.
    it "recovers after timeout interrupts mid-pipeline read" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- enterPipelineMode connection
        _ <- sendPrepare connection "s1" "select $1::int" Nothing
        _ <- sendQueryPrepared connection "s1" [Just ("42", Lq.Text)] Lq.Text
        _ <- sendPrepare connection "s2" "select pg_sleep($1)" (Just [float8Oid])
        _ <- sendQueryPrepared connection "s2" [Just ("0.1", Lq.Text)] Lq.Text
        _ <- pipelineSync connection
        -- Interrupt the read just like hasql's Connection.use + timeout does.
        _ <- timeout 50000 (drainResults connection)
        -- cleanUpAfterInterruption
        _ <- drainResults connection
        mHandle <- getCancel connection
        _ <- for mHandle Lq.cancel
        _ <- drainResults connection
        -- leavePipeline (including the retry that hasql performs)
        pipelineStatusBefore <- pipelineStatus connection
        exited <-
          if pipelineStatusBefore == Lq.PipelineOn
            then do
              _ <- pipelineSync connection
              _ <- drainResults connection
              _ <- sendFlushRequest connection
              _ <- drainResults connection
              ok <- exitPipelineMode connection
              if ok
                then pure True
                else do
                  _ <- drainResults connection
                  exitPipelineMode connection
            else pure True
        afterStatus <- pipelineStatus connection
        usable <- execScenario "select 99" connection
        pure (exited, afterStatus, usable)
