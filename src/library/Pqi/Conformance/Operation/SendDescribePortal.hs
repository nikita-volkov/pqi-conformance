-- | Coverage for 'Pqi.sendDescribePortal': asynchronously describing a
-- declared cursor's portal.
module Pqi.Conformance.Operation.SendDescribePortal
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "sendDescribePortal" do
    it "describes a declared cursor asynchronously" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "begin"
        _ <- exec connection "declare conformance_async_cursor cursor for select 1 :: int4 as n"
        sent <- sendDescribePortal connection "conformance_async_cursor"
        results <- drainResults connection
        pure (sent, results)
