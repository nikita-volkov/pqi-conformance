-- | Coverage for 'Pqi.resultStatus': the status reported for each kind of
-- result — tuples, a command, an empty query, and a failure.
module Pqi.Conformance.Operation.ResultStatus
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "resultStatus" do
    it "reports the status for each kind of result" \conninfo ->
      differential proxy conninfo \connection -> do
        let statusOf sql = exec connection sql >>= traverse resultStatus
        tuples <- statusOf "select 1"
        command <- statusOf "create temporary table conformance_status (id int4)"
        empty <- statusOf ""
        failed <- statusOf "selct 1"
        pure (tuples, command, empty, failed)
