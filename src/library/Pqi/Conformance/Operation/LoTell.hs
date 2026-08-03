-- | Coverage for 'Pqi.loTell': reporting the current seek position of an
-- open large object.
module Pqi.Conformance.Operation.LoTell
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (inTransaction)
import System.IO (IOMode (..), SeekMode (..))
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "loTell" do
    it "reports the position after a write and a seek" \conninfo ->
      differential adapter conninfo \connection ->
        inTransaction connection do
          oid <- Pqi.loCreat connection
          outcome <- for oid \o -> do
            fd <- Pqi.loOpen connection o ReadWriteMode
            positions <- for fd \f -> do
              _ <- Pqi.loWrite connection f "hello, large object"
              afterWrite <- Pqi.loTell connection f
              _ <- Pqi.loSeek connection f AbsoluteSeek 3
              afterSeek <- Pqi.loTell connection f
              pure (afterWrite, afterSeek)
            traverse_ (Pqi.loClose connection) fd
            pure positions
          traverse_ (Pqi.loUnlink connection) oid
          pure outcome
