-- | Coverage for 'Pqi.fnumber': resolving a column name to an index,
-- folding the argument the way @PQfnumber@ does (ASCII case folding outside
-- double quotes, quoted runs verbatim).
module Pqi.Conformance.Operation.Fnumber
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "fnumber" do
    it "resolves names like an identifier" \conninfo ->
      differential proxy conninfo \connection -> do
        result <- exec connection "select 1 as foo, 2 as \"Bar\""
        for result \r ->
          traverse
            (fnumber r)
            ["foo", "FOO", "Foo", "Bar", "bar", "\"Bar\"", "\"foo\"", "missing"]
