-- | Coverage for 'Pqi.loCreat': creating a new large object with a
-- server-assigned OID.
--
-- The assigned OID differs between the candidate's and the reference's runs,
-- so only its presence is compared.
module Pqi.Conformance.Operation.LoCreat
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (inTransaction)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "loCreat" do
    it "creates a large object" \conninfo ->
      differential adapter conninfo \connection ->
        inTransaction connection do
          oid <- connection.loCreat
          traverse_ connection.loUnlink oid
          pure (isJust oid)
