-- | Coverage for 'Pqi.pipelineStatus': the pipeline-mode status,
-- including the aborted state after an in-pipeline error and recovery at the
-- sync point.
module Pqi.Conformance.Operation.PipelineStatus
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
  describe "pipelineStatus" do
    it "reports off, on, aborted, and recovery" \conninfo ->
      differential adapter conninfo \connection -> do
        off <- Lq.pipelineStatus connection
        _ <- Lq.enterPipelineMode connection
        on <- Lq.pipelineStatus connection
        _ <- traverse (\sql -> Lq.sendQueryParams connection sql [] Lq.Text) ["select 1", "select 1 / 0", "select 3"]
        _ <- Lq.pipelineSync connection
        _ <- takeCommandResults connection
        _ <- takeCommandResults connection
        aborted <- Lq.pipelineStatus connection
        _ <- takeCommandResults connection
        _ <- takeResult connection
        recovered <- Lq.pipelineStatus connection
        _ <- Lq.exitPipelineMode connection
        pure (off, on, aborted, recovered)
