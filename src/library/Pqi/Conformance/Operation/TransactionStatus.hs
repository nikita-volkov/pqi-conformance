-- | Coverage for 'Pqi.transactionStatus': in-transaction status tracking
-- across a successful command, a failed command, and a rollback.
module Pqi.Conformance.Operation.TransactionStatus
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "transactionStatus" do
    it "tracks status through a transaction" \conninfo ->
      differential adapter conninfo \connection -> do
        idle <- Pqi.transactionStatus connection
        _ <- Pqi.exec connection "begin"
        inTransaction <- Pqi.transactionStatus connection
        _ <- Pqi.exec connection "select 1 / 0"
        inError <- Pqi.transactionStatus connection
        _ <- Pqi.exec connection "rollback"
        afterRollback <- Pqi.transactionStatus connection
        pure (idle, inTransaction, inError, afterRollback)
