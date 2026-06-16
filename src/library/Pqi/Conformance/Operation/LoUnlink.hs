-- | Coverage for 'Pqi.loUnlink': removing a large object, including the
-- error path for a non-existent object.
module Pqi.Conformance.Operation.LoUnlink
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
  describe "loUnlink" do
    it "removes an existing object and rejects a missing one" \conninfo ->
      differential proxy conninfo \connection ->
        inTransaction connection do
          oid <- loCreat connection
          removed <- for oid (loUnlink connection)
          missing <- loUnlink connection 4242424
          pure (removed, missing)
