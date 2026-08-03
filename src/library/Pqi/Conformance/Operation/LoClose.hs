-- | Coverage for 'Pqi.loClose': closing an open large object, after which
-- reads through the stale descriptor fail.
module Pqi.Conformance.Operation.LoClose
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (inTransaction)
import System.IO (IOMode (..))
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "loClose" do
    it "closes the descriptor and invalidates further reads" \conninfo ->
      differential adapter conninfo \connection ->
        inTransaction connection do
          oid <- Pqi.loCreat connection
          outcome <- for oid \o -> do
            fd <- Pqi.loOpen connection o ReadMode
            closed <- for fd (Pqi.loClose connection)
            readAfterClose <- join <$> for fd \f -> Pqi.loRead connection f 10
            pure (closed, readAfterClose)
          traverse_ (Pqi.loUnlink connection) oid
          pure outcome
