-- | Coverage for 'Pqi.protocolVersion': the frontend/backend protocol
-- version.
module Pqi.Conformance.Operation.ProtocolVersion
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "protocolVersion" do
    it "reports the protocol version" \conninfo ->
      differential proxy conninfo protocolVersion
