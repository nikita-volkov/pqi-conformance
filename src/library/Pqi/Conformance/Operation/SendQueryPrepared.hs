-- | Coverage for 'Pqi.sendQueryPrepared': asynchronously executing a
-- previously prepared statement.
module Pqi.Conformance.Operation.SendQueryPrepared
  ( spec,
  )
where

import qualified Pqi
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "sendQueryPrepared" do
    it "executes a prepared statement asynchronously" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection . sendPrepare "conformance_send_exec" "select $1 :: int4 * 2" Nothing
        _ <- drainResults connection
        sent <- connection . sendQueryPrepared "conformance_send_exec" [Just ("21", Lq.Text)] Lq.Text
        results <- drainResults connection
        pure (sent, results)
