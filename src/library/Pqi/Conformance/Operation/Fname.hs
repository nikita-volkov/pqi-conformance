-- | Coverage for 'Pqi.fname': column names by index, including an
-- out-of-range index.
module Pqi.Conformance.Operation.Fname
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "fname" do
    it "names columns and degrades out of range" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select 1 as foo, 2 as bar"
        for result \r -> do
          n <- nfields r
          names <- traverse (fname r) [0 .. n - 1]
          outOfRange <- fname r 5
          pure (names, outOfRange)
