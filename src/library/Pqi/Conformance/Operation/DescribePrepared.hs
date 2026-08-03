-- | Coverage for 'Pqi.describePrepared': reporting a prepared statement's
-- parameter types, including explicitly typed parameters and the
-- unknown-statement error path.
module Pqi.Conformance.Operation.DescribePrepared
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (int8Oid)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "describePrepared" do
    it "reports parameter types" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.prepare connection "conformance_desc" "select $1 :: int4, $2 :: text" Nothing
        Pqi.describePrepared connection "conformance_desc" >>= traverse observeResult

    it "reports explicit parameter types" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.prepare connection "conformance_typed" "select $1" (Just [int8Oid])
        Pqi.describePrepared connection "conformance_typed" >>= traverse observeResult

    it "rejects an unknown statement" \conninfo ->
      differential adapter conninfo \connection ->
        Pqi.describePrepared connection "conformance_missing" >>= traverse observeResult
