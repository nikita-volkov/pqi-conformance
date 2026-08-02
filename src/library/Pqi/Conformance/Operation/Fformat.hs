-- | Coverage for 'Pqi.fformat': the format (text or binary) of each
-- column, which follows the result format requested of
-- 'Pqi.execParams'.
module Pqi.Conformance.Operation.Fformat
  ( spec,
  )
where

import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Lq.Adapter -> SpecWith ByteString
spec adapter =
  describe "fformat" do
    it "follows the requested result format" \conninfo ->
      differential adapter conninfo \connection -> do
        let formatsOf fmt =
              connection.execParams "select 1 :: int4, 'x' :: text" [] fmt
                >>= traverse \r -> do
                  n <- r.nfields
                  traverse r.fformat [0 .. n - 1]
        textFormats <- formatsOf Lq.Text
        binaryFormats <- formatsOf Lq.Binary
        pure (textFormats, binaryFormats)
