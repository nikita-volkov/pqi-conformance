-- | Coverage for 'Pqi.consumeInput': reading server input into the
-- driver's buffer so that 'Pqi.isBusy' can settle before collecting
-- results.
module Pqi.Conformance.Operation.ConsumeInput
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
  describe "consumeInput" do
    it "drives result collection together with isBusy" \conninfo ->
      differential proxy conninfo \connection -> do
        sent <- sendQuery connection "select 42"
        let settle (0 :: Int) = pure False
            settle n = do
              consumed <- consumeInput connection
              busy <- isBusy connection
              if busy then threadDelay 1000 >> settle (n - 1) else pure consumed
        consumed <- settle 10000
        results <- drainResults connection
        pure (sent, consumed, results)
