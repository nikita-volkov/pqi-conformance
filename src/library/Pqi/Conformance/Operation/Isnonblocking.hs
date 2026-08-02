-- | Coverage for 'Pqi.isnonblocking': reporting the connection's
-- non-blocking flag, which defaults to off.
module Pqi.Conformance.Operation.Isnonblocking
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "isnonblocking" do
    it "is off initially and reflects a change" \conninfo ->
      differential adapter conninfo \connection -> do
        initially <- connection.isnonblocking
        _ <- connection.setnonblocking True
        afterEnable <- connection.isnonblocking
        pure (initially, afterEnable)
