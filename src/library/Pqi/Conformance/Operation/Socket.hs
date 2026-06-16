-- | Coverage for 'Pqi.socket': the connection's socket file descriptor.
--
-- The descriptor number itself is per-connection identity, so only its
-- presence on an open connection is compared.
module Pqi.Conformance.Operation.Socket
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "socket" do
    it "is present on an open connection" \conninfo ->
      differential proxy conninfo \connection ->
        isJust <$> socket connection
