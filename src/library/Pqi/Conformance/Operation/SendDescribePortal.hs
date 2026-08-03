-- | Coverage for 'Pqi.sendDescribePortal': asynchronously describing a
-- declared cursor's portal.
module Pqi.Conformance.Operation.SendDescribePortal
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "sendDescribePortal" do
    it "describes a declared cursor asynchronously" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.exec "begin"
        _ <- connection.exec "declare conformance_async_cursor cursor for select 1 :: int4 as n"
        sent <- connection.sendDescribePortal "conformance_async_cursor"
        results <- drainResults connection
        pure (sent, results)
