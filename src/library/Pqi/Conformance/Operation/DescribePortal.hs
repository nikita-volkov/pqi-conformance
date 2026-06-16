-- | Coverage for 'Pqi.describePortal': describing a declared cursor's
-- portal and the unknown-portal error path.
module Pqi.Conformance.Operation.DescribePortal
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "describePortal" do
    it "describes a declared cursor" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "begin"
        _ <-
          exec
            connection
            "declare conformance_cursor cursor for select 1 :: int4 as n, 'x' :: text as t"
        describePortal connection "conformance_cursor" >>= traverse observeResult

    it "rejects an unknown portal" \conninfo ->
      differential proxy conninfo \connection ->
        describePortal connection "conformance_no_portal" >>= traverse observeResult
