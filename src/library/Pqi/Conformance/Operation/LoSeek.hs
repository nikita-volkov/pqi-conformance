-- | Coverage for 'Pqi.loSeek': repositioning within an open large object
-- with absolute, relative, and from-end seeks.
module Pqi.Conformance.Operation.LoSeek
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
  describe "loSeek" do
    it "seeks absolutely, relatively, and from the end" \conninfo ->
      differential proxy conninfo \connection ->
        inTransaction connection do
          oid <- loCreat connection
          outcome <- for oid \o -> do
            fd <- loOpen connection o ReadWriteMode
            seeks <- for fd \f -> do
              _ <- loWrite connection f "hello, large object"
              absolute <- loSeek connection f AbsoluteSeek 0
              relative <- loSeek connection f RelativeSeek 2
              fromEnd <- loSeek connection f SeekFromEnd (-6)
              pure (absolute, relative, fromEnd)
            traverse_ (loClose connection) fd
            pure seeks
          traverse_ (loUnlink connection) oid
          pure outcome
