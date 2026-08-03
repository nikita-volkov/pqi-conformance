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
        result <- connection . exec "select 'hello' :: text, null :: int4"
        for result \r -> do
          present <- r . getvalue 0 0
          nullCell <- r . getvalue 0 1
          badRow <- r . getvalue 1 0
          badColumn <- r . getvalue 0 5
          pure (present, nullCell, badRow, badColumn)
