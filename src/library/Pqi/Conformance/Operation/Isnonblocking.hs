-- | Coverage for 'Pqi.isnonblocking': reporting the connection's
-- non-blocking flag, which defaults to off.
module Pqi.Conformance.Operation.Isnonblocking
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "isnonblocking" do
    it "is off initially and reflects a change" \conninfo ->
      differential proxy conninfo \connection -> do
        initially <- isnonblocking connection
        _ <- setnonblocking connection True
        afterEnable <- isnonblocking connection
        pure (initially, afterEnable)
