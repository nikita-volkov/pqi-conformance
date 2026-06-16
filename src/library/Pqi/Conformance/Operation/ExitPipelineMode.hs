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
import Pqi.Conformance.Scenario (takeCommandResults, takeResult)
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
