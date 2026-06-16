-- | Coverage for 'Pqi.putCopyEnd': finishing a @COPY FROM STDIN@, both
-- committing the rows and aborting the copy with a client error.
module Pqi.Conformance.Operation.PutCopyEnd
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
  describe "putCopyEnd" do
    it "commits the copied rows when ended without an error" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "create temporary table conformance_copy_end (id int4)"
        started <- execScenario "copy conformance_copy_end from stdin" connection
        row <- putCopyData connection "1\n"
        ended <- putCopyEnd connection Nothing
        outcome <- drainResults connection
        check <- execScenario "select count(*) from conformance_copy_end" connection
        pure (started, row, ended, outcome, check)

    it "aborts the copy when ended with an error" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "create temporary table conformance_copy_abort (id int4)"
        started <- execScenario "copy conformance_copy_abort from stdin" connection
        row <- putCopyData connection "1\n"
        ended <- putCopyEnd connection (Just "conformance abort")
        outcome <- drainResults connection
        check <- execScenario "select count(*) from conformance_copy_abort" connection
        pure (started, row, ended, outcome, check)
