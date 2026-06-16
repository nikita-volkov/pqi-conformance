-- | Coverage for 'Pqi.nparams': the parameter count of a
-- 'Pqi.describePrepared' result.
module Pqi.Conformance.Operation.Nparams
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "nparams" do
    it "counts the parameters of a prepared statement" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "conformance_nparams" "select $1 :: int4, $2 :: text" Nothing
        described <- describePrepared connection "conformance_nparams"
        for described nparams

    it "is zero for a parameterless statement" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "conformance_nparams_zero" "select 42" Nothing
        described <- describePrepared connection "conformance_nparams_zero"
        for described nparams
