-- | Coverage for 'Pqi.nfields': the column count across a multi-column
-- result, a zero-column result, and a command result.
module Pqi.Conformance.Operation.Nfields
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "nfields" do
    it "counts columns across result shapes" \conninfo ->
      differential adapter conninfo \connection -> do
        let countOf sql = Pqi.exec connection sql >>= traverse Pqi.nfields
        several <- countOf "select 1, 2, 3"
        zero <- countOf "select"
        command <- countOf "create temporary table conformance_nfields (id int4)"
        pure (several, zero, command)
