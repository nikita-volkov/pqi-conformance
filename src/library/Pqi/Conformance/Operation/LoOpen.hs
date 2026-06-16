-- | Coverage for 'Pqi.loOpen': opening a large object, including the
-- error path for a non-existent object.
module Pqi.Conformance.Operation.LoOpen
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
  describe "loOpen" do
    it "opens an existing object and rejects a missing one" \conninfo ->
      differential proxy conninfo \connection ->
        inTransaction connection do
          oid <- loCreat connection
          opened <- for oid \o -> do
            fd <- loOpen connection o ReadWriteMode
            traverse_ (loClose connection) fd
            pure (isJust fd)
          loUnlink' oid connection
          missing <- loOpen connection 4242424 ReadMode
          pure (opened, isJust missing)
  where
    loUnlink' oid connection = traverse_ (loUnlink connection) oid
