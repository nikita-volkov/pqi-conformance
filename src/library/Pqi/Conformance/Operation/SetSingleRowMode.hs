-- | Coverage for 'Pqi.setSingleRowMode': delivering the rows of the
-- currently executing query one result at a time.
module Pqi.Conformance.Operation.SetSingleRowMode
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "setSingleRowMode" do
    it "splits a multi-row result into single-row results" \conninfo ->
      differential adapter conninfo \connection -> do
        sent <- Pqi.sendQuery connection "select i from generate_series (1, 3) as i"
        singleRow <- Pqi.setSingleRowMode connection
        results <- drainResults connection
        pure (sent, singleRow, results)

    it "handles an empty result" \conninfo ->
      differential adapter conninfo \connection -> do
        sent <- Pqi.sendQuery connection "select 1 where false"
        singleRow <- Pqi.setSingleRowMode connection
        results <- drainResults connection
        pure (sent, singleRow, results)
