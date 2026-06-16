-- | Coverage for 'Pqi.loTruncate': truncating an open large object, after
-- which a seek to the end reports the new size.
module Pqi.Conformance.Operation.LoTruncate
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
  describe "loTruncate" do
    it "truncates to a new size" \conninfo ->
      differential proxy conninfo \connection ->
        inTransaction connection do
          oid <- loCreat connection
          outcome <- for oid \o -> do
            fd <- loOpen connection o ReadWriteMode
            result <- for fd \f -> do
              _ <- loWrite connection f "hello, large object"
              truncated <- loTruncate connection f 5
              newEnd <- loSeek connection f SeekFromEnd 0
              pure (truncated, newEnd)
            traverse_ (loClose connection) fd
            pure result
          traverse_ (loUnlink connection) oid
          pure outcome
