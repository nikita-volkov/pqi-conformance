-- | Coverage for 'Pqi.sendQueryParams': the asynchronous parameterized
-- query, including a null parameter.
module Pqi.Conformance.Operation.SendQueryParams
  ( spec,
  )
where

import qualified Pqi
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, execScenario, int4Oid, takeCommandResults, takeResult)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "sendQueryParams" do
    it "sends a parameterized query and collects its result" \conninfo ->
      differential adapter conninfo \connection -> do
        sent <-
          Lq.sendQueryParams
            connection
            "select $1 :: int4 + $2 :: int4, $3 :: text"
            [Just (int4Oid, "40", Lq.Text), Just (int4Oid, "2", Lq.Text), Nothing]
            Lq.Text
        results <- drainResults connection
        pure (sent, results)

    -- 'sendQueryParams' drives the extended protocol, so it always sends an
    -- unnamed @Parse@ and therefore receives a @ParseComplete@. In pipeline
    -- mode that @ParseComplete@ must fold into the command's own result, the
    -- way it does for 'sendQueryPrepared' and the way @libpq@ always folds it -
    -- it must never terminate the result early. A subsequent 'sendPrepare' in
    -- the same pipeline raises the pending-parse counter before any result is
    -- drained, so the @ParseComplete@ of this command must not be charged
    -- against it. Doing so emits a spurious leading @CommandOk@ and shifts
    -- every later result by one.
    --
    -- This mirrors hasql's statement-cache eviction, which sends a
    -- @DEALLOCATE@ through 'sendQueryParams' immediately before a
    -- 'sendPrepare'; the eviction breaks hasql's
    -- @StatementCache.Evolution.survives evicting a statement used later in
    -- the same pipeline@. A @SELECT@ is used here (rather than a @DEALLOCATE@)
    -- so the misattribution is visible in the result sequence: a @DEALLOCATE@'s
    -- own @CommandOk@ coincidentally masks the shifted @CommandOk@ of the
    -- prepare.
    it "folds its ParseComplete into its own result when a sendPrepare follows in the same pipeline" \conninfo ->
      differential adapter conninfo \connection -> do
        entered <- Lq.enterPipelineMode connection
        sentSelect <- Lq.sendQueryParams connection "select 1 :: int4" [] Lq.Text
        sentPrepare <- Lq.sendPrepare connection "t" "select 1 :: int4" Nothing
        sentQuery <- Lq.sendQueryPrepared connection "t" [] Lq.Text
        synced <- Lq.pipelineSync connection
        results <- drainResults connection
        exited <- Lq.exitPipelineMode connection
        pure (entered, sentSelect, sentPrepare, sentQuery, synced, results, exited)

    -- @PQsendQueryParams@ rejects a parameter list longer than 65535 (the
    -- limit imposed by the wire protocol's 16-bit parameter count field)
    -- locally, without writing anything to the socket: it returns 'False'
    -- and leaves the connection exactly as usable as before the call.
    --
    -- <https://github.com/nikita-volkov/hasql/issues/326>
    it "rejects a parameter list longer than 65535 without sending anything, leaving the connection usable" \conninfo ->
      differential adapter conninfo \connection -> do
        let overflowingParams = replicate 65536 (Just (int4Oid, "1", Lq.Text))
        sent <- Lq.sendQueryParams connection "select 1 :: int4" overflowingParams Lq.Text
        followUp <- execScenario "select 2 :: int4" connection
        pure (sent, followUp)

    -- The same rejection placed in the middle of a pipeline: the commands
    -- before it must still get dispatched and the commands after it must
    -- still get queued, with no desync in the result stream and no lasting
    -- effect on the connection once the pipeline is torn down.
    it "in a pipeline, a rejected send is skipped without desyncing the surrounding commands" \conninfo ->
      differential adapter conninfo \connection -> do
        let overflowingParams = replicate 65536 (Just (int4Oid, "1", Lq.Text))
        entered <- Lq.enterPipelineMode connection
        sentBefore1 <- Lq.sendQueryParams connection "select 1 :: int4" [] Lq.Text
        sentBefore2 <- Lq.sendQueryParams connection "select 2 :: int4" [] Lq.Text
        sentBad <- Lq.sendQueryParams connection "select 3 :: int4" overflowingParams Lq.Text
        sentAfter <- Lq.sendQueryParams connection "select 4 :: int4" [] Lq.Text
        synced <- Lq.pipelineSync connection
        first <- takeCommandResults connection
        second <- takeCommandResults connection
        third <- takeCommandResults connection
        syncResult <- takeResult connection
        idle <- takeResult connection
        exited <- Lq.exitPipelineMode connection
        followUp <- execScenario "select 5 :: int4" connection
        pure (entered, sentBefore1, sentBefore2, sentBad, sentAfter, synced, first, second, third, syncResult, idle, exited, followUp)
