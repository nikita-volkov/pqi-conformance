-- | Coverage for 'Pqi.isBusy': reporting whether a 'Pqi.getResult'
-- would block, settling to not-busy once the result has arrived.
module Pqi.Conformance.Operation.IsBusy
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
  describe "isBusy" do
    it "settles to not-busy after the result arrives" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.sendQuery connection "select 42"
        let settle (0 :: Int) = Pqi.isBusy connection
            settle n = do
              _ <- Pqi.consumeInput connection
              busy <- Pqi.isBusy connection
              if busy then threadDelay 1000 >> settle (n - 1) else pure busy
        stillBusy <- settle 10000
        _ <- drainResults connection
        pure stillBusy
