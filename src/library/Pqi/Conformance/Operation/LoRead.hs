-- | Coverage for 'Pqi.loRead': reading bytes back from an open large
-- object after seeking to its start.
module Pqi.Conformance.Operation.LoRead
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
  describe "loRead" do
    it "reads back what was written" \conninfo ->
      differential proxy conninfo \connection ->
        inTransaction connection do
          oid <- loCreat connection
          outcome <- for oid \o -> do
            fd <- loOpen connection o ReadWriteMode
            readBytes <- for fd \f -> do
              _ <- loWrite connection f "hello, large object"
              _ <- loSeek connection f AbsoluteSeek 0
              loRead connection f 5
            traverse_ (loClose connection) fd
            pure readBytes
          traverse_ (loUnlink connection) oid
          pure outcome
