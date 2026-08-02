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
          oid <- connection.loCreat
          outcome <- for oid \o -> do
            fd <- connection.loOpen o ReadWriteMode
            readBytes <- for fd \f -> do
              _ <- connection.loWrite f "hello, large object"
              _ <- connection.loSeek f AbsoluteSeek 0
              connection.loRead f 5
            traverse_ connection.loClose fd
            pure readBytes
          traverse_ connection.loUnlink oid
          pure outcome
