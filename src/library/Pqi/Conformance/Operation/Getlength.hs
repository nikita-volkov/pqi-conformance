-- | Coverage for 'Pqi.getlength': the byte length of a cell value,
-- including a multibyte value, an empty string, and a null.
module Pqi.Conformance.Operation.Getlength
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "getlength" do
    it "reports byte lengths across cell shapes" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select 'hello' :: text, 'héllo' :: text, '' :: text, null :: int4"
        for result \r -> do
          n <- nfields r
          traverse (getlength r 0) [0 .. n - 1]
