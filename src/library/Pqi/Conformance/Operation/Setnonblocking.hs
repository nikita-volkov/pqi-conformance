-- | Coverage for 'Pqi.setnonblocking': toggling the connection's
-- non-blocking flag.
module Pqi.Conformance.Operation.Setnonblocking
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "setnonblocking" do
    it "turns the non-blocking flag on and off" \conninfo ->
      differential proxy conninfo \connection -> do
        setOn <- setnonblocking connection True
        nowOn <- isnonblocking connection
        setOff <- setnonblocking connection False
        nowOff <- isnonblocking connection
        pure (setOn, nowOn, setOff, nowOff)
