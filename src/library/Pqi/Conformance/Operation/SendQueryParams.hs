-- | Coverage for 'Pqi.sendQueryParams': the asynchronous parameterized
-- query, including a null parameter.
module Pqi.Conformance.Operation.SendQueryParams
  ( spec,
  )
where

import Pqi (IsConnection (..))
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, int4Oid)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "sendQueryParams" do
    it "sends a parameterized query and collects its result" \conninfo ->
      differential proxy conninfo \connection -> do
        sent <-
          sendQueryParams
            connection
            "select $1 :: int4 + $2 :: int4, $3 :: text"
            [Just (int4Oid, "40", Lq.Text), Just (int4Oid, "2", Lq.Text), Nothing]
            Lq.Text
        results <- drainResults connection
        pure (sent, results)
