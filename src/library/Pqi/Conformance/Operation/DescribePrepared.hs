-- | Coverage for 'Pqi.describePrepared': reporting a prepared statement's
-- parameter types, including explicitly typed parameters and the
-- unknown-statement error path.
module Pqi.Conformance.Operation.DescribePrepared
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (int8Oid)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "describePrepared" do
    it "reports parameter types" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "conformance_desc" "select $1 :: int4, $2 :: text" Nothing
        describePrepared connection "conformance_desc" >>= traverse observeResult

    it "reports explicit parameter types" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "conformance_typed" "select $1" (Just [int8Oid])
        describePrepared connection "conformance_typed" >>= traverse observeResult

    it "rejects an unknown statement" \conninfo ->
      differential proxy conninfo \connection ->
        describePrepared connection "conformance_missing" >>= traverse observeResult
