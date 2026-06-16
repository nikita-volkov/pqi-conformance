-- | Coverage for 'Pqi.connectPoll': driving an asynchronous connection
-- attempt forward until it reports a terminal polling status.
module Pqi.Conformance.Operation.ConnectPoll
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (pollUntilDone)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "connectPoll" do
    it "reaches a terminal polling status and a ready connection" \conninfo ->
      differentialConnect proxy conninfo \(_ :: Proxy c) conninfo' -> do
        connection <- connectStart conninfo' :: IO c
        terminal <- pollUntilDone (connectPoll connection)
        connStatus <- status connection
        finish connection
        pure (terminal, connStatus)
