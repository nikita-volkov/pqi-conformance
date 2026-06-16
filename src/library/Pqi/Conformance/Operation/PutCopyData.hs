-- | Coverage for 'Pqi.putCopyData': streaming rows into a
-- @COPY FROM STDIN@, including malformed data that the server rejects at end.
module Pqi.Conformance.Operation.PutCopyData
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, execScenario)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "putCopyData" do
    it "streams rows into a COPY FROM STDIN" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "create temporary table conformance_copy (id int4, label text)"
        started <- execScenario "copy conformance_copy from stdin" connection
        firstRow <- putCopyData connection "1\thello\n"
        secondRow <- putCopyData connection "2\tworld\n"
        ended <- putCopyEnd connection Nothing
        outcome <- drainResults connection
        check <-
          execScenario "select count(*), min(label), max(label) from conformance_copy" connection
        pure (started, firstRow, secondRow, ended, outcome, check)

    it "feeds malformed data that the server rejects" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "create temporary table conformance_copy_bad (id int4)"
        started <- execScenario "copy conformance_copy_bad from stdin" connection
        row <- putCopyData connection "not-a-number\n"
        ended <- putCopyEnd connection Nothing
        outcome <- drainResults connection
        pure (started, row, ended, outcome)
