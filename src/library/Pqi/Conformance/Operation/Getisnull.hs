-- | Coverage for 'Pqi.getisnull': whether a cell is SQL @NULL@,
-- distinguishing a null from an empty string.
module Pqi.Conformance.Operation.Getisnull
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "getisnull" do
    it "distinguishes null, empty, and non-empty cells" \conninfo ->
      differential adapter conninfo \connection -> do
        result <- connection . exec "select 'x' :: text, '' :: text, null :: int4"
        for result \r -> do
          nonEmpty <- r . getisnull 0 0
          empty <- r . getisnull 0 1
          nullCell <- r . getisnull 0 2
          pure (nonEmpty, empty, nullCell)
