-- | Coverage for 'Pqi.ntuples': the row count across a multi-row result,
-- an empty result, and a command result.
module Pqi.Conformance.Operation.Ntuples
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "ntuples" do
    it "counts rows across result shapes" \conninfo ->
      differential proxy conninfo \connection -> do
        let countOf sql = exec connection sql >>= traverse ntuples
        many <- countOf "select i from generate_series (1, 3) as i"
        none <- countOf "select 1 where false"
        command <- countOf "create temporary table conformance_ntuples (id int4)"
        pure (many, none, command)
