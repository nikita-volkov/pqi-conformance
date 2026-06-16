-- | Coverage for 'Pqi.fformat': the format (text or binary) of each
-- column, which follows the result format requested of
-- 'Pqi.execParams'.
module Pqi.Conformance.Operation.Fformat
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "fformat" do
    it "follows the requested result format" \conninfo ->
      differential proxy conninfo \connection -> do
        let formatsOf fmt =
              execParams connection "select 1 :: int4, 'x' :: text" [] fmt
                >>= traverse \r -> do
                  n <- nfields r
                  traverse (fformat r) [0 .. n - 1]
        textFormats <- formatsOf Lq.Text
        binaryFormats <- formatsOf Lq.Binary
        pure (textFormats, binaryFormats)
