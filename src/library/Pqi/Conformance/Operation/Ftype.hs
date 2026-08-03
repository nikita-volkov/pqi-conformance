-- | Coverage for 'Pqi.ftype': the data-type OID of each column, including
-- an out-of-range index.
module Pqi.Conformance.Operation.Ftype
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "ftype" do
    it "reports type OIDs and degrades out of range" \conninfo ->
      differential adapter conninfo \connection -> do
        result <- Pqi.exec connection "select 1 :: int4, 'x' :: text, true, 1.5 :: float8, 1 :: int2"
        for result \r -> do
          n <- Pqi.nfields r
          types <- traverse (Pqi.ftype r) [0 .. n - 1]
          outOfRange <- Pqi.ftype r 9
          pure (types, outOfRange)
