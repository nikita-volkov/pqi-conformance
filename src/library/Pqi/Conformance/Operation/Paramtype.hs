-- | Coverage for 'Pqi.paramtype': the data-type OID of each parameter of
-- a 'Pqi.describePrepared' result, both inferred and explicitly typed.
module Pqi.Conformance.Operation.Paramtype
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (int8Oid)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "paramtype" do
    it "reports inferred parameter types" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.prepare "conformance_paramtype" "select $1 :: int4, $2 :: text" Nothing
        described <- connection.describePrepared "conformance_paramtype"
        for described \r -> do
          n <- r.nparams
          traverse r.paramtype [0 .. n - 1]

    it "reports explicitly requested parameter types" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.prepare "conformance_paramtype_typed" "select $1" (Just [int8Oid])
        described <- connection.describePrepared "conformance_paramtype_typed"
        for described \r -> do
          n <- r.nparams
          traverse r.paramtype [0 .. n - 1]
