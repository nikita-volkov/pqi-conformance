-- | Coverage for 'Pqi.describePortal': describing a declared cursor's
-- portal and the unknown-portal error path.
module Pqi.Conformance.Operation.DescribePortal
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "describePortal" do
    it "describes a declared cursor" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.exec "begin"
        _ <-
          connection.exec
            "declare conformance_cursor cursor for select 1 :: int4 as n, 'x' :: text as t"
        connection.describePortal "conformance_cursor" >>= traverse observeResult

    it "rejects an unknown portal" \conninfo ->
      differential adapter conninfo \connection ->
        connection.describePortal "conformance_no_portal" >>= traverse observeResult
