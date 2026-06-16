-- | Coverage for 'Pqi.serverVersion': the server version as an @MMmmpp@
-- integer.
module Pqi.Conformance.Operation.ServerVersion
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "serverVersion" do
    it "reports the server version as an integer" \conninfo ->
      differential proxy conninfo serverVersion
