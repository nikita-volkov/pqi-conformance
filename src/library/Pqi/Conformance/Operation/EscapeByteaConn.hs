-- | Coverage for 'Pqi.escapeByteaConn': escaping binary data for a
-- @bytea@ literal across the full byte range.
module Pqi.Conformance.Operation.EscapeByteaConn
  ( spec,
  )
where

import qualified Data.ByteString as ByteString
import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "escapeByteaConn" do
    it "escapes a range of binary inputs" \conninfo ->
      differential adapter conninfo \connection ->
        traverse (Pqi.escapeByteaConn connection) byteaCases
  where
    byteaCases =
      [ "",
        "plain",
        "\NUL\1\2\3",
        ByteString.pack [0 .. 255]
      ]
