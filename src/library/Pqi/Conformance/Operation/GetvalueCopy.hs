-- | Coverage for @getvalue'@ ('Pqi.getvalue''): a copying cell read whose
-- bytes remain valid after the result is freed.
module Pqi.Conformance.Operation.GetvalueCopy
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "getvalue'" do
    it "copies cells that survive freeing the result" \conninfo ->
      differential adapter conninfo \connection -> do
        result <- Pqi.exec connection "select 'copied' :: text, null :: int4"
        for result \r -> do
          first <- Pqi.getvalue' r 0 0
          second <- Pqi.getvalue' r 0 1
          Pqi.unsafeFreeResult r
          pure (first, second)
