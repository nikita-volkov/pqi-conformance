-- | Coverage for 'Pqi.getCancel': obtaining a cancellation handle from a
-- connection.
module Pqi.Conformance.Operation.GetCancel
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "getCancel" do
    it "produces a handle for an open connection" \conninfo ->
      differential adapter conninfo \connection ->
        isJust <$> Pqi.getCancel connection
