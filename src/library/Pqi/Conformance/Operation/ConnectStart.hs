-- | Coverage for 'Pqi.connectStart': beginning an asynchronous connection
-- attempt that is then driven to readiness with 'Pqi.connectPoll'.
module Pqi.Conformance.Operation.ConnectStart
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (pollUntilDone)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "connectStart" do
    it "begins an asynchronous connection that polls to readiness" \conninfo ->
      differentialConnect adapter conninfo \adapter' conninfo' -> do
        connection <- Pqi.connectStart adapter' conninfo'
        polled <- pollUntilDone (Pqi.connectPoll connection)
        observation <- observeConnection connection
        Pqi.finish connection
        pure (polled, observation)
