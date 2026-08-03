-- | Coverage for 'Pqi.loWrite': writing bytes to an open large object.
module Pqi.Conformance.Operation.LoWrite
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (inTransaction)
import System.IO (IOMode (..))
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "loWrite" do
    it "reports the number of bytes written" \conninfo ->
      differential adapter conninfo \connection ->
        inTransaction connection do
          oid <- Pqi.loCreat connection
          outcome <- for oid \o -> do
            fd <- Pqi.loOpen connection o ReadWriteMode
            written <- for fd \f -> Pqi.loWrite connection f "hello, large object"
            traverse_ (Pqi.loClose connection) fd
            pure written
          traverse_ (Pqi.loUnlink connection) oid
          pure outcome
