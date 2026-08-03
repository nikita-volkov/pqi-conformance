-- | Coverage for 'Pqi.socket': the connection's socket file descriptor.
--
-- The descriptor number itself is per-connection identity, so only its
-- presence on an open connection is compared.
module Pqi.Conformance.Operation.Socket
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "socket" do
    it "is present on an open connection" \conninfo ->
      differential adapter conninfo \connection ->
        isJust <$> connection.socket
