-- | Coverage for 'Pqi.prepare': preparing a named statement, the result
-- it reports, and duplicate-name handling.
module Pqi.Conformance.Operation.Prepare
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "prepare" do
    it "reports its own result" \conninfo ->
      differential adapter conninfo \connection ->
        connection
          . prepare "conformance_prep_result" "select $1 :: int4" Nothing
          >>= traverse observeResult

    it "rejects a duplicate statement name" \conninfo ->
      differential adapter conninfo \connection -> do
        first <-
          connection . prepare "conformance_dup" "select 1" Nothing >>= traverse observeResult
        second <-
          connection . prepare "conformance_dup" "select 2" Nothing >>= traverse observeResult
        pure (first, second)
