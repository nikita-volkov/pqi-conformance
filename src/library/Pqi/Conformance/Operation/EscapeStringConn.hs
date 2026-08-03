-- | Coverage for 'Pqi.escapeStringConn': escaping strings for SQL
-- literals, the invalid-encoding error path, and a round-trip through a query.
module Pqi.Conformance.Operation.EscapeStringConn
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (execScenario)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "escapeStringConn" do
    it "escapes a range of strings" \conninfo ->
      differential adapter conninfo \connection ->
        traverse connection.escapeStringConn stringCases

    it "rejects invalid encoding" \conninfo ->
      differential adapter conninfo \connection ->
        connection.escapeStringConn "\255\254"

    it "produces literals that round-trip through a query" \conninfo ->
      differential adapter conninfo \connection -> do
        escaped <- connection.escapeStringConn "it's \\ tricky\nstuff"
        for escaped \literal ->
          execScenario ("select '" <> literal <> "' :: text") connection
  where
    stringCases =
      [ "",
        "plain",
        "it's",
        "back\\slash",
        "newline\nand\ttab",
        "héllo🐘",
        "double''single"
      ]
