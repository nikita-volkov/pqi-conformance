-- | Coverage for 'Pqi.connectionNeedsPassword': whether authentication
-- needed a password that was unavailable (False under trust auth).
module Pqi.Conformance.Operation.ConnectionNeedsPassword
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "connectionNeedsPassword" do
    it "reports whether a password was needed" \conninfo ->
      differential adapter conninfo (. connectionNeedsPassword)
