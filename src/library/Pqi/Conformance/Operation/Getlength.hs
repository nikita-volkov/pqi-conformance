-- | Coverage for 'Pqi.getlength': the byte length of a cell value,
-- including a multibyte value, an empty string, and a null.
module Pqi.Conformance.Operation.Getlength
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "getlength" do
    it "reports byte lengths across cell shapes" \conninfo ->
      differential adapter conninfo \connection -> do
        result <- Pqi.exec connection "select 'hello' :: text, 'héllo' :: text, '' :: text, null :: int4"
        for result \r -> do
          n <- Pqi.nfields r
          traverse (Pqi.getlength r 0) [0 .. n - 1]
