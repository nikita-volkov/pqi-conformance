-- | Coverage for 'Pqi.connectionUsedPassword': whether authentication
-- used a password (False under trust auth).
module Pqi.Conformance.Operation.ConnectionUsedPassword
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "connectionUsedPassword" do
    it "reports whether a password was used" \conninfo ->
      differential proxy conninfo connectionUsedPassword
