-- | Coverage for 'Pqi.sendFlushRequest': asking the server to flush its
-- output buffer so pipelined results arrive without a 'Pqi.pipelineSync'.
module Pqi.Conformance.Operation.SendFlushRequest
  ( spec,
  )
where

import Pqi (IsConnection (..))
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (takeCommandResults, takeResult)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "sendFlushRequest" do
    it "delivers results without a sync" \conninfo ->
      differential proxy conninfo \connection -> do
        entered <- enterPipelineMode connection
        sent <- sendQueryParams connection "select 42" [] Lq.Text
        flushRequested <- sendFlushRequest connection
        results <- takeCommandResults connection
        synced <- pipelineSync connection
        syncResult <- takeResult connection
        exited <- exitPipelineMode connection
        pure (entered, sent, flushRequested, results, synced, syncResult, exited)
