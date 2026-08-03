-- | Coverage for 'Pqi.loSeek': repositioning within an open large object
-- with absolute, relative, and from-end seeks.
module Pqi.Conformance.Operation.LoSeek
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
  describe "loSeek" do
    it "seeks absolutely, relatively, and from the end" \conninfo ->
      differential adapter conninfo \connection ->
        inTransaction connection do
          oid <- connection . loCreat
          outcome <- for oid \o -> do
            fd <- connection . loOpen o ReadWriteMode
            seeks <- for fd \f -> do
              _ <- connection . loWrite f "hello, large object"
              absolute <- connection . loSeek f AbsoluteSeek 0
              relative <- connection . loSeek f RelativeSeek 2
              fromEnd <- connection . loSeek f SeekFromEnd (-6)
              pure (absolute, relative, fromEnd)
            traverse_ connection . loClose fd
            pure seeks
          traverse_ connection . loUnlink oid
          pure outcome
