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
          oid <- Pqi.loCreat connection
          outcome <- for oid \o -> do
            fd <- Pqi.loOpen connection o ReadWriteMode
            seeks <- for fd \f -> do
              _ <- Pqi.loWrite connection f "hello, large object"
              absolute <- Pqi.loSeek connection f AbsoluteSeek 0
              relative <- Pqi.loSeek connection f RelativeSeek 2
              fromEnd <- Pqi.loSeek connection f SeekFromEnd (-6)
              pure (absolute, relative, fromEnd)
            traverse_ (Pqi.loClose connection) fd
            pure seeks
          traverse_ (Pqi.loUnlink connection) oid
          pure outcome
