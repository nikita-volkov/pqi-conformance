-- | Coverage for 'Pqi.loTell': reporting the current seek position of an
-- open large object.
module Pqi.Conformance.Operation.LoTell
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (inTransaction)
import System.IO (IOMode (..), SeekMode (..))
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "loTell" do
    it "reports the position after a write and a seek" \conninfo ->
      differential proxy conninfo \connection ->
        inTransaction connection do
          oid <- loCreat connection
          outcome <- for oid \o -> do
            fd <- loOpen connection o ReadWriteMode
            positions <- for fd \f -> do
              _ <- loWrite connection f "hello, large object"
              afterWrite <- loTell connection f
              _ <- loSeek connection f AbsoluteSeek 3
              afterSeek <- loTell connection f
              pure (afterWrite, afterSeek)
            traverse_ (loClose connection) fd
            pure positions
          traverse_ (loUnlink connection) oid
          pure outcome
