-- | Coverage for @getvalue'@ ('Pqi.getvalue''): a copying cell read whose
-- bytes remain valid after the result is freed.
module Pqi.Conformance.Operation.GetvalueCopy
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "getvalue'" do
    it "copies cells that survive freeing the result" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select 'copied' :: text, null :: int4"
        for result \r -> do
          first <- getvalue' r 0 0
          second <- getvalue' r 0 1
          unsafeFreeResult r
          pure (first, second)
