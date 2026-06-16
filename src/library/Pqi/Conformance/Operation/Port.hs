-- | Coverage for 'Pqi.port': the port of the connection.
module Pqi.Conformance.Operation.Port
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "port" do
    it "reports the port from the conninfo" \conninfo ->
      differential proxy conninfo port
