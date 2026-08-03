-- | Coverage for 'Pqi.serverVersion': the server version as an @MMmmpp@
-- integer.
module Pqi.Conformance.Operation.ServerVersion
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "serverVersion" do
    it "reports the server version as an integer" \conninfo ->
      differential adapter conninfo Pqi.serverVersion
