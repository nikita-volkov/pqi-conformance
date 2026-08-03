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
          oid <- connection . loCreat
          outcome <- for oid \o -> do
            fd <- connection . loOpen o ReadWriteMode
            result <- for fd \f -> do
              _ <- connection . loWrite f "hello, large object"
              truncated <- connection . loTruncate f 5
              newEnd <- connection . loSeek f SeekFromEnd 0
              pure (truncated, newEnd)
            traverse_ connection . loClose fd
            pure result
          traverse_ connection . loUnlink oid
          pure outcome
