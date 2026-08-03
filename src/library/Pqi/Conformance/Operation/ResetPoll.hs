-- | Coverage for 'Pqi.resetPoll': driving an asynchronous reset forward
-- until it reports a terminal polling status.
module Pqi.Conformance.Operation.ResetPoll
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
  describe "resetPoll" do
    it "reaches a terminal polling status and a ready connection" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.exec connection "begin"
        started <- Pqi.resetStart connection
        terminal <- pollUntilDone (Pqi.resetPoll connection)
        afterStatus <- Pqi.transactionStatus connection
        pure (started, terminal, afterStatus)
