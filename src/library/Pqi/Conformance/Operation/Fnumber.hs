-- | Coverage for 'Pqi.fnumber': resolving a column name to an index,
-- folding the argument the way @PQfnumber@ does (ASCII case folding outside
-- double quotes, quoted runs verbatim).
module Pqi.Conformance.Operation.Fnumber
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "fnumber" do
    it "resolves names like an identifier" \conninfo ->
      differential adapter conninfo \connection -> do
        result <- connection.exec "select 1 as foo, 2 as \"Bar\""
        for result \r ->
          traverse
            r.fnumber
            ["foo", "FOO", "Foo", "Bar", "bar", "\"Bar\"", "\"foo\"", "missing"]
