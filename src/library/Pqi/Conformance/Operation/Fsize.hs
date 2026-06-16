-- | Coverage for 'Pqi.fsize': the server-side storage size of each
-- column's type (negative for variable size), including an out-of-range index.
module Pqi.Conformance.Operation.Fsize
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "fsize" do
    it "reports type sizes and degrades out of range" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select 1 :: int2, 1 :: int4, 1 :: int8, 'x' :: text, true"
        for result \r -> do
          n <- nfields r
          sizes <- traverse (fsize r) [0 .. n - 1]
          outOfRange <- fsize r 9
          pure (sizes, outOfRange)
