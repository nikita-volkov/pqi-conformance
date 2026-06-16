-- | Coverage for 'Pqi.transactionStatus': in-transaction status tracking
-- across a successful command, a failed command, and a rollback.
module Pqi.Conformance.Operation.TransactionStatus
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "transactionStatus" do
    it "tracks status through a transaction" \conninfo ->
      differential proxy conninfo \connection -> do
        idle <- transactionStatus connection
        _ <- exec connection "begin"
        inTransaction <- transactionStatus connection
        _ <- exec connection "select 1 / 0"
        inError <- transactionStatus connection
        _ <- exec connection "rollback"
        afterRollback <- transactionStatus connection
        pure (idle, inTransaction, inError, afterRollback)
