-- | Coverage for 'Pqi.status': the current connection status.
module Pqi.Conformance.Operation.Status
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "status" do
    it "reports a ready connection as OK" \conninfo ->
      differential proxy conninfo status
