-- | Coverage for 'Pqi.loWrite': writing bytes to an open large object.
module Pqi.Conformance.Operation.LoWrite
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (inTransaction)
import System.IO (IOMode (..))
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "loWrite" do
    it "reports the number of bytes written" \conninfo ->
      differential proxy conninfo \connection ->
        inTransaction connection do
          oid <- loCreat connection
          outcome <- for oid \o -> do
            fd <- loOpen connection o ReadWriteMode
            written <- for fd \f -> loWrite connection f "hello, large object"
            traverse_ (loClose connection) fd
            pure written
          traverse_ (loUnlink connection) oid
          pure outcome
