-- | Coverage for 'Pqi.flush': flushing queued output to the server in
-- non-blocking mode until it reports completion.
module Pqi.Conformance.Operation.Flush
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, flushUntilDone)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "flush" do
    it "flushes queued output to completion in non-blocking mode" \conninfo ->
      differential proxy conninfo \connection -> do
        setOk <- setnonblocking connection True
        sent <- sendQuery connection "select 1"
        flushed <- flushUntilDone (flush connection)
        results <- drainResults connection
        restoreOk <- setnonblocking connection False
        pure (setOk, sent, flushed, results, restoreOk)
