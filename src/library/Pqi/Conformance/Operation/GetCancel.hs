-- | Coverage for 'Pqi.getCancel': obtaining a cancellation handle from a
-- connection.
module Pqi.Conformance.Operation.GetCancel
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "getCancel" do
    it "produces a handle for an open connection" \conninfo ->
      differential proxy conninfo \connection ->
        isJust <$> getCancel connection
