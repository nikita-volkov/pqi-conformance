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
          oid <- connection . loCreat
          outcome <- for oid \o -> do
            fd <- connection . loOpen o ReadWriteMode
            written <- for fd \f -> connection . loWrite f "hello, large object"
            traverse_ connection . loClose fd
            pure written
          traverse_ connection . loUnlink oid
          pure outcome
