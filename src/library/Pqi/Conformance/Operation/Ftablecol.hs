-- | Coverage for 'Pqi.ftablecol': the source-column number of each result
-- column.
--
-- A shared @pg_catalog@ table is selected so the provenance is identical across
-- the candidate's and the reference's connections.
module Pqi.Conformance.Operation.Ftablecol
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "ftablecol" do
    it "reports the source-column number, or zero for a computed column" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select relname, relkind, 1 as computed from pg_catalog.pg_class where false"
        for result \r -> do
          n <- nfields r
          traverse (ftablecol r) [0 .. n - 1]
