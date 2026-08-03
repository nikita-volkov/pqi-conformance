-- | Coverage for 'Pqi.user': the user name of the connection.
module Pqi.Conformance.Operation.User
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "user" do
    it "reports the user name from the conninfo" \conninfo ->
      differential adapter conninfo (. user)
