-- | Coverage for 'Pqi.sendDescribePrepared': asynchronously describing a
-- prepared statement.
module Pqi.Conformance.Operation.SendDescribePrepared
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "sendDescribePrepared" do
    it "describes a prepared statement asynchronously" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.sendPrepare "conformance_send_desc" "select $1 :: int4 * 2" Nothing
        _ <- drainResults connection
        sent <- connection.sendDescribePrepared "conformance_send_desc"
        results <- drainResults connection
        pure (sent, results)
