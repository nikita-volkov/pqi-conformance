-- | Coverage for 'Pqi.fsize': the server-side storage size of each
-- column's type (negative for variable size), including an out-of-range index.
module Pqi.Conformance.Operation.Fsize
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "fsize" do
    it "reports type sizes and degrades out of range" \conninfo ->
      differential adapter conninfo \connection -> do
        result <- connection . exec "select 1 :: int2, 1 :: int4, 1 :: int8, 'x' :: text, true"
        for result \r -> do
          n <- r . nfields
          sizes <- traverse r . fsize [0 .. n - 1]
          outOfRange <- r . fsize 9
          pure (sizes, outOfRange)
