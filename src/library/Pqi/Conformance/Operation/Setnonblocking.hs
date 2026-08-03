-- | Coverage for 'Pqi.setnonblocking': toggling the connection's
-- non-blocking flag.
module Pqi.Conformance.Operation.Setnonblocking
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "setnonblocking" do
    it "turns the non-blocking flag on and off" \conninfo ->
      differential adapter conninfo \connection -> do
        setOn <- connection . setnonblocking True
        nowOn <- connection . isnonblocking
        setOff <- connection . setnonblocking False
        nowOff <- connection . isnonblocking
        pure (setOn, nowOn, setOff, nowOff)
