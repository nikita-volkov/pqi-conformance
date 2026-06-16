-- | Coverage for 'Pqi.ftype': the data-type OID of each column, including
-- an out-of-range index.
module Pqi.Conformance.Operation.Ftype
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "ftype" do
    it "reports type OIDs and degrades out of range" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select 1 :: int4, 'x' :: text, true, 1.5 :: float8, 1 :: int2"
        for result \r -> do
          n <- nfields r
          types <- traverse (ftype r) [0 .. n - 1]
          outOfRange <- ftype r 9
          pure (types, outOfRange)
