-- | Coverage for 'Pqi.resetStart': beginning an asynchronous reset that
-- is then driven to completion with 'Pqi.resetPoll'.
module Pqi.Conformance.Operation.ResetStart
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
  describe "resetStart" do
    it "begins an asynchronous reset that polls to a fresh session" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "begin"
        started <- resetStart connection
        polled <- pollUntilDone (resetPoll connection)
        afterReset <- observeConnection connection
        pure (started, polled, afterReset)
