-- | Coverage for 'Pqi.getResult': collecting the results of an
-- asynchronous command one at a time, ending with the 'Nothing' terminator.
module Pqi.Conformance.Operation.GetResult
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (takeResult)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "getResult" do
    it "yields each result then a Nothing terminator" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- sendQuery connection "select 1 :: int4"
        first <- takeResult connection
        terminator <- takeResult connection
        afterTerminator <- takeResult connection
        pure (first, terminator, afterTerminator)
