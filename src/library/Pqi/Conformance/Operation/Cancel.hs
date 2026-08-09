-- | Coverage for 'Pqi.cancel': requesting cancellation through a handle,
-- both on an idle connection and against a running query (which then fails
-- with SQLSTATE @57014@).
--
-- The full 'Either' value is compared - not just success\/failure - so that
-- any divergence in error text is caught.
module Pqi.Conformance.Operation.Cancel
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import qualified Pqi.Conformance.Operation.Cancel.Cleanup as Cleanup
import qualified Pqi.Conformance.Operation.Cancel.Stale as Stale
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, execScenario)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "cancel" do
    it "succeeds on an idle connection" \conninfo ->
      differential adapter conninfo \connection -> do
        handle <- Pqi.getCancel connection
        for handle Pqi.cancel

    it "fails a running query with 57014" \conninfo ->
      differential adapter conninfo \connection -> do
        sent <- Pqi.sendQuery connection "select pg_sleep(10)"
        threadDelay 100000
        handle <- Pqi.getCancel connection
        cancelled <- for handle Pqi.cancel
        results <- drainResults connection
        usable <- execScenario "select 1" connection
        pure (sent, cancelled, results, usable)

    it "leaves the connection usable after cancelling a short-running query" \conninfo ->
      differential adapter conninfo \connection ->
        replicateM 3 do
          _ <- Pqi.sendQuery connection "select pg_sleep(0.1)"
          threadDelay 50000
          handle <- Pqi.getCancel connection
          _ <- for handle Pqi.cancel
          -- The cancel races the statement's own ~100 ms completion, so the
          -- drained result is non-deterministically FatalError (SQLSTATE 57014)
          -- or TuplesOk; comparing it across adapters is what made this test
          -- flaky. Drain it to settle the connection, then assert only the
          -- property the test is named for: the next command still works.
          _ <- drainResults connection
          execScenario "select 1" connection

    Cleanup.spec adapter
    Stale.spec adapter
