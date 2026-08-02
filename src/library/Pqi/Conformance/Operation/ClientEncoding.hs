-- | Coverage for 'Pqi.clientEncoding': reporting the current client
-- encoding, which tracks 'Pqi.setClientEncoding' and governs how result
-- cells are re-encoded.
module Pqi.Conformance.Operation.ClientEncoding
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (execScenario)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "clientEncoding" do
    it "round-trips and governs result re-encoding" \conninfo ->
      differential adapter conninfo \connection -> do
        initial <- connection.clientEncoding
        setOk <- connection.setClientEncoding "LATIN1"
        switched <- connection.clientEncoding
        reported <- connection.parameterStatus "client_encoding"
        latinCell <- execScenario "select chr(233) as e" connection
        restoreOk <- connection.setClientEncoding "UTF8"
        utfCell <- execScenario "select chr(233) as e" connection
        pure (initial, setOk, switched, reported, latinCell, restoreOk, utfCell)
