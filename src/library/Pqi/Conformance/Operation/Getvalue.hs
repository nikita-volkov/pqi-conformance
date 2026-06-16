-- | Coverage for 'Pqi.getvalue': the cell value at a position, 'Nothing'
-- for SQL @NULL@, and out-of-range row and column probes.
module Pqi.Conformance.Operation.Getvalue
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "getvalue" do
    it "reads cells, nulls, and degrades out of range" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select 'hello' :: text, null :: int4"
        for result \r -> do
          present <- getvalue r 0 0
          nullCell <- getvalue r 0 1
          badRow <- getvalue r 1 0
          badColumn <- getvalue r 0 5
          pure (present, nullCell, badRow, badColumn)
