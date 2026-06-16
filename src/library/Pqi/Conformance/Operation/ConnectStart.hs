-- | Coverage for 'Pqi.connectStart': beginning an asynchronous connection
-- attempt that is then driven to readiness with 'Pqi.connectPoll'.
module Pqi.Conformance.Operation.ConnectStart
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (pollUntilDone)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "connectStart" do
    it "begins an asynchronous connection that polls to readiness" \conninfo ->
      differentialConnect proxy conninfo \(_ :: Proxy c) conninfo' -> do
        connection <- connectStart conninfo' :: IO c
        polled <- pollUntilDone (connectPoll connection)
        observation <- observeConnection connection
        finish connection
        pure (polled, observation)
