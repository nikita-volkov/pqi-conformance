-- | Coverage for 'Pqi.connectionNeedsPassword': whether authentication
-- needed a password that was unavailable (False under trust auth).
module Pqi.Conformance.Operation.ConnectionNeedsPassword
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "connectionNeedsPassword" do
    it "reports whether a password was needed" \conninfo ->
      differential proxy conninfo connectionNeedsPassword
