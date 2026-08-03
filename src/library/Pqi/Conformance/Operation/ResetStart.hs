-- | Coverage for 'Pqi.resetStart': beginning an asynchronous reset that
-- is then driven to completion with 'Pqi.resetPoll'.
module Pqi.Conformance.Operation.ResetStart
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
  describe "resetStart" do
    it "begins an asynchronous reset that polls to a fresh session" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.exec "begin"
        started <- connection.resetStart
        polled <- pollUntilDone connection.resetPoll
        afterReset <- observeConnection connection
        pure (started, polled, afterReset)
