-- | Coverage for 'Pqi.escapeByteaConn': escaping binary data for a
-- @bytea@ literal across the full byte range.
module Pqi.Conformance.Operation.EscapeByteaConn
  ( spec,
  )
where

import qualified Data.ByteString as ByteString
import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "escapeByteaConn" do
    it "escapes a range of binary inputs" \conninfo ->
      differential proxy conninfo \connection ->
        traverse (escapeByteaConn connection) byteaCases
  where
    byteaCases =
      [ "",
        "plain",
        "\NUL\1\2\3",
        ByteString.pack [0 .. 255]
      ]
