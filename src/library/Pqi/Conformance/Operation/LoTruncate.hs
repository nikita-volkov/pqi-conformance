-- | Coverage for 'Pqi.loTruncate': truncating an open large object, after
-- which a seek to the end reports the new size.
module Pqi.Conformance.Operation.LoTruncate
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
  describe "loTruncate" do
    it "truncates to a new size" \conninfo ->
      differential adapter conninfo \connection ->
        inTransaction connection do
          oid <- Pqi.loCreat connection
          outcome <- for oid \o -> do
            fd <- Pqi.loOpen connection o ReadWriteMode
            result <- for fd \f -> do
              _ <- Pqi.loWrite connection f "hello, large object"
              truncated <- Pqi.loTruncate connection f 5
              newEnd <- Pqi.loSeek connection f SeekFromEnd 0
              pure (truncated, newEnd)
            traverse_ (Pqi.loClose connection) fd
            pure result
          traverse_ (Pqi.loUnlink connection) oid
          pure outcome
