-- | Coverage for 'Pqi.pipelineSync': marking a synchronization point that
-- batches pipelined commands, and the abort semantics when one of them fails.
module Pqi.Conformance.Operation.PipelineSync
  ( spec,
  )
where

import qualified Pqi
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import qualified Pqi.Conformance.Operation.PipelineSync.Parity as Parity
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (takeCommandResults, takeResult)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "pipelineSync" do
    Parity.spec adapter
    it "collects pipelined queries per sync" \conninfo ->
      differential adapter conninfo \connection -> do
        entered <- connection.enterPipelineMode
        sent <-
          traverse
            (\sql -> connection.sendQueryParams sql [] Lq.Text)
            ["select 1 :: int4", "select 'two' :: text", "select 3 :: int4, 'three' :: text"]
        synced <- connection.pipelineSync
        first <- takeCommandResults connection
        second <- takeCommandResults connection
        third <- takeCommandResults connection
        syncResult <- takeResult connection
        idle <- takeResult connection
        exited <- connection.exitPipelineMode
        pure (entered, sent, synced, first, second, third, syncResult, idle, exited)

    it "aborts the rest of the pipeline after an error" \conninfo ->
      differential adapter conninfo \connection -> do
        entered <- connection.enterPipelineMode
        sent <-
          traverse
            (\sql -> connection.sendQueryParams sql [] Lq.Text)
            ["select 1", "select 1 / 0", "select 3"]
        synced <- connection.pipelineSync
        first <- takeCommandResults connection
        failed <- takeCommandResults connection
        aborted <- takeCommandResults connection
        syncResult <- takeResult connection
        exited <- connection.exitPipelineMode
        pure (entered, sent, synced, first, failed, aborted, syncResult, exited)

    it "returns a sync result when called without prior commands" \conninfo ->
      differential adapter conninfo \connection -> do
        entered <- connection.enterPipelineMode
        synced <- connection.pipelineSync
        syncResult <- takeResult connection
        trailing <- takeResult connection
        exited <- connection.exitPipelineMode
        pure (entered, synced, syncResult, trailing, exited)
