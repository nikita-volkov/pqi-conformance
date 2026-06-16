-- | Coverage for 'Pqi.user': the user name of the connection.
module Pqi.Conformance.Operation.User
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "user" do
    it "reports the user name from the conninfo" \conninfo ->
      differential proxy conninfo user
