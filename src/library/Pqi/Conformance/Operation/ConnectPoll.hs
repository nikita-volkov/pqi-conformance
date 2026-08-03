-- | Coverage for 'Pqi.connectPoll': driving an asynchronous connection
-- attempt forward until it reports a terminal polling status.
module Pqi.Conformance.Operation.ConnectPoll
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (pollUntilDone)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "connectPoll" do
    it "reaches a terminal polling status and a ready connection" \conninfo ->
      differentialConnect adapter conninfo \adapter' conninfo' -> do
        connection <- adapter' . connectStart conninfo'
        terminal <- pollUntilDone connection . connectPoll
        connStatus <- connection . status
        connection . finish
        pure (terminal, connStatus)
