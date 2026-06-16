-- | Coverage for 'Pqi.setSingleRowMode': delivering the rows of the
-- currently executing query one result at a time.
module Pqi.Conformance.Operation.SetSingleRowMode
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "setSingleRowMode" do
    it "splits a multi-row result into single-row results" \conninfo ->
      differential proxy conninfo \connection -> do
        sent <- sendQuery connection "select i from generate_series (1, 3) as i"
        singleRow <- setSingleRowMode connection
        results <- drainResults connection
        pure (sent, singleRow, results)

    it "handles an empty result" \conninfo ->
      differential proxy conninfo \connection -> do
        sent <- sendQuery connection "select 1 where false"
        singleRow <- setSingleRowMode connection
        results <- drainResults connection
        pure (sent, singleRow, results)
