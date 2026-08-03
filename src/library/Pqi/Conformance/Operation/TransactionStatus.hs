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
        idle <- connection . transactionStatus
        _ <- connection . exec "begin"
        inTransaction <- connection . transactionStatus
        _ <- connection . exec "select 1 / 0"
        inError <- connection . transactionStatus
        _ <- connection . exec "rollback"
        afterRollback <- connection . transactionStatus
        pure (idle, inTransaction, inError, afterRollback)
