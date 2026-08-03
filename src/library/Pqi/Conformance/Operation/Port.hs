-- | Coverage for 'Pqi.port': the port of the connection.
module Pqi.Conformance.Operation.Port
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "port" do
    it "reports the port from the conninfo" \conninfo ->
      differential adapter conninfo Pqi.port
