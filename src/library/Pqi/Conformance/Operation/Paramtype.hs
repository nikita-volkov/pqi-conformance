-- | Coverage for 'Pqi.paramtype': the data-type OID of each parameter of
-- a 'Pqi.describePrepared' result, both inferred and explicitly typed.
module Pqi.Conformance.Operation.Paramtype
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (int8Oid)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "paramtype" do
    it "reports inferred parameter types" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "conformance_paramtype" "select $1 :: int4, $2 :: text" Nothing
        described <- describePrepared connection "conformance_paramtype"
        for described \r -> do
          n <- nparams r
          traverse (paramtype r) [0 .. n - 1]

    it "reports explicitly requested parameter types" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "conformance_paramtype_typed" "select $1" (Just [int8Oid])
        described <- describePrepared connection "conformance_paramtype_typed"
        for described \r -> do
          n <- nparams r
          traverse (paramtype r) [0 .. n - 1]
