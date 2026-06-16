-- | Coverage for 'Pqi.unsafeFreeResult': freeing a result leaves the
-- connection usable for subsequent commands.
--
-- An observation of a freed result must not be compared: with the C-backed
-- adapter the accessors share the @PGresult@'s storage, so freeing invalidates
-- previously returned bytes. The example therefore frees and then runs a fresh
-- query, observing only the latter.
module Pqi.Conformance.Operation.UnsafeFreeResult
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (execScenario)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "unsafeFreeResult" do
    it "leaves the connection usable" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select 1"
        traverse_ unsafeFreeResult result
        execScenario "select 2" connection
