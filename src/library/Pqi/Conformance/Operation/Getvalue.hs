-- | Coverage for 'Pqi.getvalue': the cell value at a position, 'Nothing'
-- for SQL @NULL@, and out-of-range row and column probes.
module Pqi.Conformance.Operation.Getvalue
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "getvalue" do
    it "reads cells, nulls, and degrades out of range" \conninfo ->
      differential adapter conninfo \connection -> do
        result <- Pqi.exec connection "select 'hello' :: text, null :: int4"
        for result \r -> do
          present <- Pqi.getvalue r 0 0
          nullCell <- Pqi.getvalue r 0 1
          badRow <- Pqi.getvalue r 1 0
          badColumn <- Pqi.getvalue r 0 5
          pure (present, nullCell, badRow, badColumn)
