-- | Coverage for 'Pqi.ntuples': the row count across a multi-row result,
-- an empty result, and a command result.
module Pqi.Conformance.Operation.Ntuples
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "ntuples" do
    it "counts rows across result shapes" \conninfo ->
      differential adapter conninfo \connection -> do
        let countOf sql = connection.exec sql >>= traverse (.ntuples)
        many <- countOf "select i from generate_series (1, 3) as i"
        none <- countOf "select 1 where false"
        command <- countOf "create temporary table conformance_ntuples (id int4)"
        pure (many, none, command)
