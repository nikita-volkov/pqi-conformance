-- | Coverage for 'Pqi.fname': column names by index, including an
-- out-of-range index.
module Pqi.Conformance.Operation.Fname
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "fname" do
    it "names columns and degrades out of range" \conninfo ->
      differential adapter conninfo \connection -> do
        result <- connection.exec "select 1 as foo, 2 as bar"
        for result \r -> do
          n <- r.nfields
          names <- traverse r.fname [0 .. n - 1]
          outOfRange <- r.fname 5
          pure (names, outOfRange)
