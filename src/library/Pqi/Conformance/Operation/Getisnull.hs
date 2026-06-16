-- | Coverage for 'Pqi.getisnull': whether a cell is SQL @NULL@,
-- distinguishing a null from an empty string.
module Pqi.Conformance.Operation.Getisnull
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "getisnull" do
    it "distinguishes null, empty, and non-empty cells" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select 'x' :: text, '' :: text, null :: int4"
        for result \r -> do
          nonEmpty <- getisnull r 0 0
          empty <- getisnull r 0 1
          nullCell <- getisnull r 0 2
          pure (nonEmpty, empty, nullCell)
