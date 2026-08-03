-- | Coverage for 'Pqi.loRead': reading bytes back from an open large
-- object after seeking to its start.
module Pqi.Conformance.Operation.LoRead
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
  describe "loRead" do
    it "reads back what was written" \conninfo ->
      differential adapter conninfo \connection ->
        inTransaction connection do
          oid <- Pqi.loCreat connection
          outcome <- for oid \o -> do
            fd <- Pqi.loOpen connection o ReadWriteMode
            readBytes <- for fd \f -> do
              _ <- Pqi.loWrite connection f "hello, large object"
              _ <- Pqi.loSeek connection f AbsoluteSeek 0
              Pqi.loRead connection f 5
            traverse_ (Pqi.loClose connection) fd
            pure readBytes
          traverse_ (Pqi.loUnlink connection) oid
          pure outcome
