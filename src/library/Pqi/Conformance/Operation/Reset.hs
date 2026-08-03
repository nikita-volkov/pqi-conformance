-- | Coverage for 'Pqi.reset': a blocking reset restores a fresh session,
-- discarding transaction state and prepared statements.
module Pqi.Conformance.Operation.Reset
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "reset" do
    it "restores a fresh session" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.prepare connection "conformance_reset" "select 1" Nothing
        _ <- Pqi.exec connection "begin"
        inTransaction <- Pqi.transactionStatus connection
        Pqi.reset connection
        afterReset <- observeConnection connection
        -- The prepared statement must be gone in the fresh session.
        describeAfter <- Pqi.describePrepared connection "conformance_reset" >>= traverse observeResult
        pure (inTransaction, afterReset, describeAfter)
