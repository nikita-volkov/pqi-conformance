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
import Pqi.Conformance.Scenario (drainResults, int4Oid)
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
