-- | Coverage for 'Pqi.nparams': the parameter count of a
-- 'Pqi.describePrepared' result.
module Pqi.Conformance.Operation.Nparams
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "nparams" do
    it "counts the parameters of a prepared statement" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.prepare connection "conformance_nparams" "select $1 :: int4, $2 :: text" Nothing
        described <- Pqi.describePrepared connection "conformance_nparams"
        for described Pqi.nparams

    it "is zero for a parameterless statement" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.prepare connection "conformance_nparams_zero" "select 42" Nothing
        described <- Pqi.describePrepared connection "conformance_nparams_zero"
        for described Pqi.nparams
