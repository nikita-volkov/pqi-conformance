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
        result <- Pqi.exec connection "select 1 as foo, 2 as bar"
        for result \r -> do
          n <- Pqi.nfields r
          names <- traverse (Pqi.fname r) [0 .. n - 1]
          outOfRange <- Pqi.fname r 5
          pure (names, outOfRange)
