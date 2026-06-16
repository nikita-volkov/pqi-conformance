-- | Coverage for 'Pqi.loClose': closing an open large object, after which
-- reads through the stale descriptor fail.
module Pqi.Conformance.Operation.LoClose
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (inTransaction)
import System.IO (IOMode (..))
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "loClose" do
    it "closes the descriptor and invalidates further reads" \conninfo ->
      differential proxy conninfo \connection ->
        inTransaction connection do
          oid <- loCreat connection
          outcome <- for oid \o -> do
            fd <- loOpen connection o ReadMode
            closed <- for fd \f -> loClose connection f
            readAfterClose <- join <$> for fd \f -> loRead connection f 10
            pure (closed, readAfterClose)
          traverse_ (loUnlink connection) oid
          pure outcome
