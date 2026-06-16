-- | Coverage for 'Pqi.ftable': the source-table OID of each column.
--
-- A shared @pg_catalog@ table is selected so the OID is identical across the
-- candidate's and the reference's connections (a temporary table's OID would
-- differ per connection).
module Pqi.Conformance.Operation.Ftable
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "ftable" do
    it "reports the source-table OID, or none for a computed column" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select relname, relkind, 1 as computed from pg_catalog.pg_class where false"
        for result \r -> do
          n <- nfields r
          traverse (ftable r) [0 .. n - 1]
