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
          oid <- connection.loCreat
          outcome <- for oid \o -> do
            fd <- connection.loOpen o ReadMode
            closed <- for fd (connection.loClose)
            readAfterClose <- join <$> for fd \f -> connection.loRead f 10
            pure (closed, readAfterClose)
          traverse_ connection.loUnlink oid
          pure outcome
