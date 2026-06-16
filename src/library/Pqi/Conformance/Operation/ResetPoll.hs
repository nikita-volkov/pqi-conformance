-- | Coverage for 'Pqi.resetPoll': driving an asynchronous reset forward
-- until it reports a terminal polling status.
module Pqi.Conformance.Operation.ResetPoll
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
  describe "resetPoll" do
    it "reaches a terminal polling status and a ready connection" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "begin"
        started <- resetStart connection
        terminal <- pollUntilDone (resetPoll connection)
        afterStatus <- transactionStatus connection
        pure (started, terminal, afterStatus)
