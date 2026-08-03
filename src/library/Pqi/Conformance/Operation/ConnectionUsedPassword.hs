-- | Coverage for 'Pqi.connectionUsedPassword': whether authentication
-- used a password (False under trust auth).
module Pqi.Conformance.Operation.ConnectionUsedPassword
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "connectionUsedPassword" do
    it "reports whether a password was used" \conninfo ->
      differential adapter conninfo Pqi.connectionUsedPassword
