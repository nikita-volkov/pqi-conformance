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
          oid <- Pqi.loCreat connection
          opened <- for oid \o -> do
            fd <- Pqi.loOpen connection o ReadWriteMode
            traverse_ (Pqi.loClose connection) fd
            pure (isJust fd)
          loUnlink' oid connection
          missing <- Pqi.loOpen connection 4242424 ReadMode
          pure (opened, isJust missing)
  where
    loUnlink' oid connection = traverse_ (Pqi.loUnlink connection) oid
