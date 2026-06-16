-- | Coverage for 'Pqi.sendDescribePrepared': asynchronously describing a
-- prepared statement.
module Pqi.Conformance.Operation.SendDescribePrepared
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
  describe "sendDescribePrepared" do
    it "describes a prepared statement asynchronously" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- sendPrepare connection "conformance_send_desc" "select $1 :: int4 * 2" Nothing
        _ <- drainResults connection
        sent <- sendDescribePrepared connection "conformance_send_desc"
        results <- drainResults connection
        pure (sent, results)
