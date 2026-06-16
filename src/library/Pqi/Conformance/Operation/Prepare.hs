-- | Coverage for 'Pqi.prepare': preparing a named statement, the result
-- it reports, and duplicate-name handling.
module Pqi.Conformance.Operation.Prepare
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "prepare" do
    it "reports its own result" \conninfo ->
      differential proxy conninfo \connection ->
        prepare connection "conformance_prep_result" "select $1 :: int4" Nothing
          >>= traverse observeResult

    it "rejects a duplicate statement name" \conninfo ->
      differential proxy conninfo \connection -> do
        first <-
          prepare connection "conformance_dup" "select 1" Nothing >>= traverse observeResult
        second <-
          prepare connection "conformance_dup" "select 2" Nothing >>= traverse observeResult
        pure (first, second)
