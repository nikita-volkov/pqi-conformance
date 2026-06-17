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
import Pqi.Conformance.Scenario (drainResults, float8Oid, takeCommandResults, takeResult)
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
