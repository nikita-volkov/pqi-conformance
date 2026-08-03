-- | Coverage for 'Pqi.pass': the password of the connection (absent under
-- the trust-auth conninfo).
module Pqi.Conformance.Operation.Pass
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "pass" do
    it "reports the password from the conninfo" \conninfo ->
      differential adapter conninfo (. pass)
