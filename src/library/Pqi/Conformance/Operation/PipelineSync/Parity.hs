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

import qualified Pqi
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (observed, takeCommandResults, takeResult)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "parity" do
    it "manySmallResults matches sequential execution" \conninfo ->
      differential adapter conninfo \connection -> do
        let query = "SELECT 1, 2"
        sequential <- replicateM 100 (observed query [] Lq.Text connection)
        entered <- connection.enterPipelineMode
        sent <- replicateM 100 (connection.sendQueryParams query [] Lq.Text)
        synced <- connection.pipelineSync
        pipeline <- replicateM 100 (takeCommandResults connection)
        syncResult <- takeResult connection
        trailing <- takeResult connection
        exited <- connection.exitPipelineMode
        let pipelineResults = map fst pipeline
        sequential `shouldBe` pipelineResults
        pure (entered, sent, synced, pipeline, syncResult, trailing, exited, sequential)

    it "manyLargeResults matches sequential execution" \conninfo ->
      differential adapter conninfo \connection -> do
        let query = "SELECT generate_series(0,1000) as a, generate_series(1000,2000) as b"
        sequential <- replicateM 100 (observed query [] Lq.Text connection)
        entered <- connection.enterPipelineMode
        sent <- replicateM 100 (connection.sendQueryParams query [] Lq.Text)
        synced <- connection.pipelineSync
        pipeline <- replicateM 100 (takeCommandResults connection)
        syncResult <- takeResult connection
        trailing <- takeResult connection
        exited <- connection.exitPipelineMode
        let pipelineResults = map fst pipeline
        sequential `shouldBe` pipelineResults
        pure (entered, sent, synced, pipeline, syncResult, trailing, exited, sequential)
