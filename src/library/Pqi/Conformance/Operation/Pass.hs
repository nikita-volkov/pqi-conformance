-- | Coverage for 'Pqi.pass': the password of the connection (absent under
-- the trust-auth conninfo).
module Pqi.Conformance.Operation.Pass
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "pass" do
    it "reports the password from the conninfo" \conninfo ->
      differential proxy conninfo pass
