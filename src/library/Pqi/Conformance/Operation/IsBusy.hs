-- | Coverage for 'Pqi.isBusy': reporting whether a 'Pqi.getResult'
-- would block, settling to not-busy once the result has arrived.
module Pqi.Conformance.Operation.IsBusy
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
  describe "isBusy" do
    it "settles to not-busy after the result arrives" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- sendQuery connection "select 42"
        let settle (0 :: Int) = isBusy connection
            settle n = do
              _ <- consumeInput connection
              busy <- isBusy connection
              if busy then threadDelay 1000 >> settle (n - 1) else pure busy
        stillBusy <- settle 10000
        _ <- drainResults connection
        pure stillBusy
