-- | Coverage for 'Pqi.loUnlink': removing a large object, including the
-- error path for a non-existent object.
module Pqi.Conformance.Operation.LoUnlink
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
  describe "loUnlink" do
    it "removes an existing object and rejects a missing one" \conninfo ->
      differential adapter conninfo \connection ->
        inTransaction connection do
          oid <- connection . loCreat
          removed <- for oid connection . loUnlink
          missing <- connection . loUnlink 4242424
          pure (removed, missing)
