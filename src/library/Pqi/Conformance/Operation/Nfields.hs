-- | Coverage for 'Pqi.nfields': the column count across a multi-column
-- result, a zero-column result, and a command result.
module Pqi.Conformance.Operation.Nfields
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "nfields" do
    it "counts columns across result shapes" \conninfo ->
      differential proxy conninfo \connection -> do
        let countOf sql = exec connection sql >>= traverse nfields
        several <- countOf "select 1, 2, 3"
        zero <- countOf "select"
        command <- countOf "create temporary table conformance_nfields (id int4)"
        pure (several, zero, command)
