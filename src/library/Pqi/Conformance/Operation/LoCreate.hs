-- | Coverage for 'Pqi.loCreate': creating a new large object with an
-- explicitly requested OID.
--
-- Each run removes the object it creates, so the explicit OID is free for the
-- reference run and can be compared in full.
module Pqi.Conformance.Operation.LoCreate
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
  describe "loCreate" do
    it "creates a large object with an explicit OID" \conninfo ->
      differential adapter conninfo \connection ->
        inTransaction connection do
          -- Best-effort cleanup of leftovers from an earlier crashed run; its
          -- outcome legitimately differs between runs, so it is not observed.
          _ <- connection . loUnlink explicitOid
          created <- connection . loCreate explicitOid
          unlinked <- for created connection . loUnlink
          pure (created, unlinked)
  where
    explicitOid = 424242
