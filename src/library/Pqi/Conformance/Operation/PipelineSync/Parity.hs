-- | Reproduces the @hasql@ pipeline parity benchmark scenarios at the @pqi@
-- level: the same sequence of queries is run both sequentially and inside a
-- pipeline, and the two ways of executing them must produce identical
-- observations.
--
-- This is a scenario test for 'Pqi.pipelineSync': it marks the sync point that
-- lets the pipelined batch complete.
module Pqi.Conformance.Operation.PipelineSync.Parity
  ( spec,
  )
where

import Pqi (IsConnection (..))
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (observed, takeCommandResults, takeResult)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "parity" do
    it "manySmallResults matches sequential execution" \conninfo ->
      differential proxy conninfo \connection -> do
        let query = "SELECT 1, 2"
        sequential <- replicateM 100 (observed query [] Lq.Text connection)
        entered <- enterPipelineMode connection
        sent <- replicateM 100 (sendQueryParams connection query [] Lq.Text)
        synced <- pipelineSync connection
        pipeline <- replicateM 100 (takeCommandResults connection)
        syncResult <- takeResult connection
        trailing <- takeResult connection
        exited <- exitPipelineMode connection
        let pipelineResults = map fst pipeline
        sequential `shouldBe` pipelineResults
        pure (entered, sent, synced, pipeline, syncResult, trailing, exited, sequential)

    it "manyLargeResults matches sequential execution" \conninfo ->
      differential proxy conninfo \connection -> do
        let query = "SELECT generate_series(0,1000) as a, generate_series(1000,2000) as b"
        sequential <- replicateM 100 (observed query [] Lq.Text connection)
        entered <- enterPipelineMode connection
        sent <- replicateM 100 (sendQueryParams connection query [] Lq.Text)
        synced <- pipelineSync connection
        pipeline <- replicateM 100 (takeCommandResults connection)
        syncResult <- takeResult connection
        trailing <- takeResult connection
        exited <- exitPipelineMode connection
        let pipelineResults = map fst pipeline
        sequential `shouldBe` pipelineResults
        pure (entered, sent, synced, pipeline, syncResult, trailing, exited, sequential)
