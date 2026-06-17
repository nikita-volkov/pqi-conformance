-- | Coverage for 'Pqi.pipelineSync': marking a synchronization point that
-- batches pipelined commands, and the abort semantics when one of them fails.
module Pqi.Conformance.Operation.PipelineSync
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
  describe "pipelineSync" do
    it "collects pipelined queries per sync" \conninfo ->
      differential proxy conninfo \connection -> do
        entered <- enterPipelineMode connection
        sent <-
          traverse
            (\sql -> sendQueryParams connection sql [] Lq.Text)
            ["select 1 :: int4", "select 'two' :: text", "select 3 :: int4, 'three' :: text"]
        synced <- pipelineSync connection
        first <- takeCommandResults connection
        second <- takeCommandResults connection
        third <- takeCommandResults connection
        syncResult <- takeResult connection
        idle <- takeResult connection
        exited <- exitPipelineMode connection
        pure (entered, sent, synced, first, second, third, syncResult, idle, exited)

    it "aborts the rest of the pipeline after an error" \conninfo ->
      differential proxy conninfo \connection -> do
        entered <- enterPipelineMode connection
        sent <-
          traverse
            (\sql -> sendQueryParams connection sql [] Lq.Text)
            ["select 1", "select 1 / 0", "select 3"]
        synced <- pipelineSync connection
        first <- takeCommandResults connection
        failed <- takeCommandResults connection
        aborted <- takeCommandResults connection
        syncResult <- takeResult connection
        exited <- exitPipelineMode connection
        pure (entered, sent, synced, first, failed, aborted, syncResult, exited)

    it "returns a sync result when called without prior commands" \conninfo ->
      differential proxy conninfo \connection -> do
        entered <- enterPipelineMode connection
        synced <- pipelineSync connection
        syncResult <- takeResult connection
        trailing <- takeResult connection
        exited <- exitPipelineMode connection
        pure (entered, synced, syncResult, trailing, exited)
