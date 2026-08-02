-- | Coverage for 'Pqi.loOpen': opening a large object, including the
-- error path for a non-existent object.
module Pqi.Conformance.Operation.LoOpen
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
  describe "loOpen" do
    it "opens an existing object and rejects a missing one" \conninfo ->
      differential adapter conninfo \connection ->
        inTransaction connection do
          oid <- connection.loCreat
          opened <- for oid \o -> do
            fd <- connection.loOpen o ReadWriteMode
            traverse_ connection.loClose fd
            pure (isJust fd)
          loUnlink' oid connection
          missing <- connection.loOpen 4242424 ReadMode
          pure (opened, isJust missing)
  where
    loUnlink' oid connection = traverse_ connection.loUnlink oid
