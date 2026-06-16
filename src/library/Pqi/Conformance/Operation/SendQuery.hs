-- | Coverage for 'Pqi.sendQuery': submitting a command without waiting,
-- then collecting its results, including multi-statement scripts, a mid-script
-- error, and the empty query.
module Pqi.Conformance.Operation.SendQuery
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
  describe "sendQuery" do
    let sendAndDrain sql conninfo =
          differential proxy conninfo \connection -> do
            sent <- sendQuery connection sql
            results <- drainResults connection
            pure (sent, results)
    it "sends a query and collects its result"
      $ sendAndDrain "select i, i * 10 from generate_series (1, 3) as i"
    it "yields multiple results for multiple statements"
      $ sendAndDrain "select 1; select 2, 3; select 4"
    it "ends the results at a mid-script error"
      $ sendAndDrain "select 1; select 1 / 0; select 3"
    it "handles an empty string"
      $ sendAndDrain ""
