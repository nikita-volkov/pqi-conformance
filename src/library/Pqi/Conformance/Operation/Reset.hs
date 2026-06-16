-- | Coverage for 'Pqi.reset': a blocking reset restores a fresh session,
-- discarding transaction state and prepared statements.
module Pqi.Conformance.Operation.Reset
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "reset" do
    it "restores a fresh session" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "conformance_reset" "select 1" Nothing
        _ <- exec connection "begin"
        inTransaction <- transactionStatus connection
        reset connection
        afterReset <- observeConnection connection
        -- The prepared statement must be gone in the fresh session.
        describeAfter <- describePrepared connection "conformance_reset" >>= traverse observeResult
        pure (inTransaction, afterReset, describeAfter)
