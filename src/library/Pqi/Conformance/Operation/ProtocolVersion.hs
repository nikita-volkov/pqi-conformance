-- | Coverage for 'Pqi.protocolVersion': the frontend/backend protocol
-- version.
module Pqi.Conformance.Operation.ProtocolVersion
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "protocolVersion" do
    it "reports the protocol version" \conninfo ->
      differential adapter conninfo (.protocolVersion)
