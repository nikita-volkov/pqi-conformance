-- | Coverage for 'Pqi.ftable': the source-table OID of each column.
--
-- A shared @pg_catalog@ table is selected so the OID is identical across the
-- candidate's and the reference's connections (a temporary table's OID would
-- differ per connection).
module Pqi.Conformance.Operation.Ftable
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "ftable" do
    it "reports the source-table OID, or none for a computed column" \conninfo ->
      differential adapter conninfo \connection -> do
        result <- connection . exec "select relname, relkind, 1 as computed from pg_catalog.pg_class where false"
        for result \r -> do
          n <- r . nfields
          traverse r . ftable [0 .. n - 1]
