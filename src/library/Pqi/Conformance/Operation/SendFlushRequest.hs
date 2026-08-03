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
        entered <- connection . enterPipelineMode
        sent <- connection . sendQueryParams "select 42" [] Lq.Text
        flushRequested <- connection . sendFlushRequest
        results <- takeCommandResults connection
        synced <- connection . pipelineSync
        syncResult <- takeResult connection
        exited <- connection . exitPipelineMode
        pure (entered, sent, flushRequested, results, synced, syncResult, exited)
