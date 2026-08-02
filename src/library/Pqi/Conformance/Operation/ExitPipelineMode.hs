-- | Coverage for 'Pqi.exitPipelineMode': leaving pipeline mode, which
-- fails while work is still pending and succeeds once the pipeline is drained.
module Pqi.Conformance.Operation.ExitPipelineMode
  ( spec,
  )
where

import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, execScenario, float8Oid, takeCommandResults, takeResult)
import System.Timeout (timeout)
import Test.Hspec

spec :: Lq.Adapter -> SpecWith ByteString
spec adapter =
  describe "exitPipelineMode" do
    it "returns the connection to its non-pipeline status" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.enterPipelineMode
        exited <- connection.exitPipelineMode
        after <- connection.pipelineStatus
        pure (exited, after)

    it "fails with work pending and succeeds once drained" \conninfo ->
      differential adapter conninfo \connection -> do
        entered <- connection.enterPipelineMode
        sent <- connection.sendQueryParams "select 1" [] Lq.Text
        prematureExit <- connection.exitPipelineMode
        synced <- connection.pipelineSync
        results <- takeCommandResults connection
        syncResult <- takeResult connection
        exited <- connection.exitPipelineMode
        pure (entered, sent, prematureExit, synced, results, syncResult, exited)

    -- Reproduces the cleanup sequence that hasql's cleanUpAfterInterruption +
    -- leavePipeline performs after a timeout mid-pipeline.  Two prepared
    -- statements are used so that pendingParses is tracked (matching the
    -- real-world scenario).  The slow statement is cancelled before its result
    -- is consumed, then the connection is restored via the exact drain/sync
    -- sequence that hasql uses.
    it "recovers after mid-pipeline cancel (mirrors cleanUpAfterInterruption)" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.enterPipelineMode
        _ <- connection.sendPrepare "s1" "select 1" Nothing
        _ <- connection.sendQueryPrepared "s1" [] Lq.Text
        _ <- connection.sendPrepare "s2" "select pg_sleep($1)" (Just [float8Oid])
        _ <- connection.sendQueryPrepared "s2" [Just ("0.5", Lq.Text)] Lq.Text
        _ <- connection.pipelineSync
        _ <- connection.sendFlushRequest
        -- Consume what toPipelineIO would have read before the timeout:
        -- parse1 result + separator, exec1 result + separator, parse2 result + separator.
        _ <- takeCommandResults connection
        _ <- takeCommandResults connection
        _ <- takeCommandResults connection
        -- exec2 (pg_sleep) is still running; cancel it to simulate the
        -- timeout-triggered cancel in cleanUpAfterInterruption.
        mHandle <- connection.getCancel
        _ <- for mHandle (.cancel)
        -- cleanUpAfterInterruption: drain1, then drain2 (after cancel)
        _ <- drainResults connection
        _ <- drainResults connection
        -- leavePipeline: new Sync, drain, Flush, drain
        _ <- connection.pipelineSync
        _ <- drainResults connection
        _ <- connection.sendFlushRequest
        _ <- drainResults connection
        exited <- connection.exitPipelineMode
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
      differential adapter conninfo \connection -> do
        _ <- connection.enterPipelineMode
        _ <- connection.sendPrepare "s1" "select $1::int" Nothing
        _ <- connection.sendQueryPrepared "s1" [Just ("42", Lq.Text)] Lq.Text
        _ <- connection.sendPrepare "s2" "select pg_sleep($1)" (Just [float8Oid])
        _ <- connection.sendQueryPrepared "s2" [Just ("0.1", Lq.Text)] Lq.Text
        _ <- connection.pipelineSync
        -- Interrupt the read just like hasql's Connection.use + timeout does.
        _ <- timeout 50000 (drainResults connection)
        -- cleanUpAfterInterruption
        _ <- drainResults connection
        mHandle <- connection.getCancel
        _ <- for mHandle (.cancel)
        _ <- drainResults connection
        -- leavePipeline (including the retry that hasql performs)
        pipelineStatusBefore <- connection.pipelineStatus
        exited <-
          if pipelineStatusBefore == Lq.PipelineOn
            then do
              _ <- connection.pipelineSync
              _ <- drainResults connection
              _ <- connection.sendFlushRequest
              _ <- drainResults connection
              ok <- connection.exitPipelineMode
              if ok
                then pure True
                else do
                  _ <- drainResults connection
                  connection.exitPipelineMode
            else pure True
        afterStatus <- connection.pipelineStatus
        usable <- execScenario "select 99" connection
        pure (exited, afterStatus, usable)
