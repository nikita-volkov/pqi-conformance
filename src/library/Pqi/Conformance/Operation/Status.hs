-- | Coverage for 'Pqi.status': the current connection status.
module Pqi.Conformance.Operation.Status
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "status" do
    it "reports a ready connection as OK" \conninfo ->
      differential adapter conninfo (. status)
