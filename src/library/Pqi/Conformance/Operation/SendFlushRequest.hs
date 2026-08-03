-- | Coverage for 'Pqi.sendFlushRequest': asking the server to flush its
-- output buffer so pipelined results arrive without a 'Pqi.pipelineSync'.
module Pqi.Conformance.Operation.SendFlushRequest
  ( spec,
  )
where

import qualified Pqi
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (takeCommandResults, takeResult)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "sendFlushRequest" do
    it "delivers results without a sync" \conninfo ->
      differential adapter conninfo \connection -> do
        entered <- Lq.enterPipelineMode connection
        sent <- Lq.sendQueryParams connection "select 42" [] Lq.Text
        flushRequested <- Lq.sendFlushRequest connection
        results <- takeCommandResults connection
        synced <- Lq.pipelineSync connection
        syncResult <- takeResult connection
        exited <- Lq.exitPipelineMode connection
        pure (entered, sent, flushRequested, results, synced, syncResult, exited)
