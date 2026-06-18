-- | Coverage for 'Pqi.cancel': requesting cancellation through a handle,
-- both on an idle connection and against a running query (which then fails
-- with SQLSTATE @57014@).
--
-- The full 'Either' value is compared — not just success\/failure — so that
-- any divergence in error text is caught.
module Pqi.Conformance.Operation.Cancel
  ( spec,
  )
where

import Pqi (IsCancel (..), IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, execScenario)
import qualified Pqi.Conformance.Operation.Cancel.Cleanup as Cleanup
import qualified Pqi.Conformance.Operation.Cancel.Stale as Stale
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "cancel" do
    it "succeeds on an idle connection" \conninfo ->
      differential proxy conninfo \connection -> do
        handle <- getCancel connection
        for handle cancel

    it "fails a running query with 57014" \conninfo ->
      differential proxy conninfo \connection -> do
        sent <- sendQuery connection "select pg_sleep(10)"
        threadDelay 100000
        handle <- getCancel connection
        cancelled <- for handle cancel
        results <- drainResults connection
        usable <- execScenario "select 1" connection
        pure (sent, cancelled, results, usable)

    it "leaves the connection usable after cancelling a short-running query" \conninfo ->
      differential proxy conninfo \connection -> do
        outcomes <- replicateM 3 do
          sent <- sendQuery connection "select pg_sleep(0.1)"
          threadDelay 50000
          handle <- getCancel connection
          cancelled <- for handle cancel
          results <- drainResults connection
          usable <- execScenario "select 1" connection
          pure (sent, cancelled, results, usable)
        pure outcomes

    Cleanup.spec proxy
    Stale.spec proxy
