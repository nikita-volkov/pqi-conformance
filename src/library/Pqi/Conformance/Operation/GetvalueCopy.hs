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
        result <- connection . exec "select 'copied' :: text, null :: int4"
        for result \r -> do
          first <- r . getvalue' 0 0
          second <- r . getvalue' 0 1
          r . unsafeFreeResult
          pure (first, second)
