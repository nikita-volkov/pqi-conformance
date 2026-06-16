-- | Coverage for 'Pqi.sendPrepare': asynchronously preparing a named
-- statement and collecting its result.
module Pqi.Conformance.Operation.SendPrepare
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "sendPrepare" do
    it "prepares a statement asynchronously" \conninfo ->
      differential proxy conninfo \connection -> do
        sent <- sendPrepare connection "conformance_send_prepare" "select $1 :: int4 * 2" Nothing
        results <- drainResults connection
        pure (sent, results)
