-- | Coverage for 'Pqi.loCreat': creating a new large object with a
-- server-assigned OID.
--
-- The assigned OID differs between the candidate's and the reference's runs,
-- so only its presence is compared.
module Pqi.Conformance.Operation.LoCreat
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (inTransaction)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "loCreat" do
    it "creates a large object" \conninfo ->
      differential proxy conninfo \connection ->
        inTransaction connection do
          oid <- loCreat connection
          traverse_ (loUnlink connection) oid
          pure (isJust oid)
