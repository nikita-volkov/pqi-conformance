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
        result <- Pqi.exec connection "select 1 :: int2, 1 :: int4, 1 :: int8, 'x' :: text, true"
        for result \r -> do
          n <- Pqi.nfields r
          sizes <- traverse (Pqi.fsize r) [0 .. n - 1]
          outOfRange <- Pqi.fsize r 9
          pure (sizes, outOfRange)
