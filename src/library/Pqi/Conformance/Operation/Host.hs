-- | Coverage for 'Pqi.host': the server host name of the connection.
module Pqi.Conformance.Operation.Host
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "host" do
    it "reports the host from the conninfo" \conninfo ->
      differential proxy conninfo host
