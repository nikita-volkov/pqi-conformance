-- | Coverage for 'Pqi.consumeInput': reading server input into the
-- driver's buffer so that 'Pqi.isBusy' can settle before collecting
-- results.
module Pqi.Conformance.Operation.ConsumeInput
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
  describe "consumeInput" do
    it "drives result collection together with isBusy" \conninfo ->
      differential adapter conninfo \connection -> do
        sent <- Pqi.sendQuery connection "select 42"
        let settle (0 :: Int) = pure False
            settle n = do
              consumed <- Pqi.consumeInput connection
              busy <- Pqi.isBusy connection
              if busy then threadDelay 1000 >> settle (n - 1) else pure consumed
        consumed <- settle 10000
        results <- drainResults connection
        pure (sent, consumed, results)
