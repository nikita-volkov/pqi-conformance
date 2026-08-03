-- | Coverage for 'Pqi.flush': flushing queued output to the server in
-- non-blocking mode until it reports completion.
module Pqi.Conformance.Operation.Flush
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, flushUntilDone)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "flush" do
    it "flushes queued output to completion in non-blocking mode" \conninfo ->
      differential adapter conninfo \connection -> do
        setOk <- connection . setnonblocking True
        sent <- connection . sendQuery "select 1"
        flushed <- flushUntilDone connection . flush
        results <- drainResults connection
        restoreOk <- connection . setnonblocking False
        pure (setOk, sent, flushed, results, restoreOk)
