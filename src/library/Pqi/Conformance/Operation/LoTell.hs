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
          oid <- connection.loCreat
          outcome <- for oid \o -> do
            fd <- connection.loOpen o ReadWriteMode
            positions <- for fd \f -> do
              _ <- connection.loWrite f "hello, large object"
              afterWrite <- connection.loTell f
              _ <- connection.loSeek f AbsoluteSeek 3
              afterSeek <- connection.loTell f
              pure (afterWrite, afterSeek)
            traverse_ connection.loClose fd
            pure positions
          traverse_ connection.loUnlink oid
          pure outcome
