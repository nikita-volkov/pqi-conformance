-- | Coverage for 'Pqi.unescapeBytea': decoding the textual representation of
-- a @bytea@ value, in both the hex and legacy escape formats, including
-- malformed input.
module Pqi.Conformance.Operation.UnescapeBytea
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Prelude
import qualified Pqi.Conformance.Reference as Reference
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "unescapeBytea" do
    for_ cases \input ->
      it (show input) \_ -> do
        candidate <- Pqi.unescapeBytea adapter input
        reference <- Pqi.unescapeBytea Reference.adapter input
        candidate `shouldBe` reference
  where
    cases =
      [ "",
        "\\x",
        "\\x00",
        "\\x00ff",
        "\\x48656c6c6f",
        "\\X48656C6C6F",
        "\\xAbCd",
        "Hello, world",
        "h\233llo bytes",
        "\\\\",
        "\\001\\002\\003",
        "a\\010b",
        "\\x4",
        "\\x4g",
        "\\xzz",
        "\\x61 62",
        "\\377",
        "\\400",
        "\\000",
        "\\1",
        "\\18",
        "\\8",
        "a\\b",
        "trailing\\",
        "mixed\\134text"
      ]
