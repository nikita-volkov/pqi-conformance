-- | Coverage for 'Pqi.sendQueryPrepared': asynchronously executing a
-- previously prepared statement.
module Pqi.Conformance.Operation.SendQueryPrepared
  ( spec,
  )
where

import Pqi (IsConnection (..))
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "sendQueryPrepared" do
    it "executes a prepared statement asynchronously" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- sendPrepare connection "conformance_send_exec" "select $1 :: int4 * 2" Nothing
        _ <- drainResults connection
        sent <- sendQueryPrepared connection "conformance_send_exec" [Just ("21", Lq.Text)] Lq.Text
        results <- drainResults connection
        pure (sent, results)
