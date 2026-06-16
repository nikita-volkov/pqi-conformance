-- | Coverage for 'Pqi.clientEncoding': reporting the current client
-- encoding, which tracks 'Pqi.setClientEncoding' and governs how result
-- cells are re-encoded.
module Pqi.Conformance.Operation.ClientEncoding
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (execScenario)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "clientEncoding" do
    it "round-trips and governs result re-encoding" \conninfo ->
      differential proxy conninfo \connection -> do
        initial <- clientEncoding connection
        setOk <- setClientEncoding connection "LATIN1"
        switched <- clientEncoding connection
        reported <- parameterStatus connection "client_encoding"
        latinCell <- execScenario "select chr(233) as e" connection
        restoreOk <- setClientEncoding connection "UTF8"
        utfCell <- execScenario "select chr(233) as e" connection
        pure (initial, setOk, switched, reported, latinCell, restoreOk, utfCell)
