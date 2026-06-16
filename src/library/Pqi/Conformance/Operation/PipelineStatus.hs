-- | Coverage for 'Pqi.pipelineStatus': the pipeline-mode status,
-- including the aborted state after an in-pipeline error and recovery at the
-- sync point.
module Pqi.Conformance.Operation.PipelineStatus
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
  describe "pipelineStatus" do
    it "reports off, on, aborted, and recovery" \conninfo ->
      differential proxy conninfo \connection -> do
        off <- pipelineStatus connection
        _ <- enterPipelineMode connection
        on <- pipelineStatus connection
        _ <- traverse (\sql -> sendQueryParams connection sql [] Lq.Text) ["select 1", "select 1 / 0", "select 3"]
        _ <- pipelineSync connection
        _ <- takeCommandResults connection
        _ <- takeCommandResults connection
        aborted <- pipelineStatus connection
        _ <- takeCommandResults connection
        _ <- takeResult connection
        recovered <- pipelineStatus connection
        _ <- exitPipelineMode connection
        pure (off, on, aborted, recovered)
