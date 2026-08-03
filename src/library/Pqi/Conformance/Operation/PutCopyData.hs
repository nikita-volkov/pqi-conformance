-- | Coverage for 'Pqi.putCopyData': streaming rows into a
-- @COPY FROM STDIN@, including malformed data that the server rejects at end.
module Pqi.Conformance.Operation.PutCopyData
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, execScenario)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "putCopyData" do
    it "streams rows into a COPY FROM STDIN" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.exec connection "create temporary table conformance_copy (id int4, label text)"
        started <- execScenario "copy conformance_copy from stdin" connection
        firstRow <- Pqi.putCopyData connection "1\thello\n"
        secondRow <- Pqi.putCopyData connection "2\tworld\n"
        ended <- Pqi.putCopyEnd connection Nothing
        outcome <- drainResults connection
        check <-
          execScenario "select count(*), min(label), max(label) from conformance_copy" connection
        pure (started, firstRow, secondRow, ended, outcome, check)

    it "feeds malformed data that the server rejects" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- Pqi.exec connection "create temporary table conformance_copy_bad (id int4)"
        started <- execScenario "copy conformance_copy_bad from stdin" connection
        row <- Pqi.putCopyData connection "not-a-number\n"
        ended <- Pqi.putCopyEnd connection Nothing
        outcome <- drainResults connection
        pure (started, row, ended, outcome)
