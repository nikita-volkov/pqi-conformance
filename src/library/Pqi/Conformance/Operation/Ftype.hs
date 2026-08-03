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
        result <- connection . exec "select 1 :: int4, 'x' :: text, true, 1.5 :: float8, 1 :: int2"
        for result \r -> do
          n <- r . nfields
          types <- traverse r . ftype [0 .. n - 1]
          outOfRange <- r . ftype 9
          pure (types, outOfRange)
