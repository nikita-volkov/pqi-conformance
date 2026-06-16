-- | Coverage for 'Pqi.escapeStringConn': escaping strings for SQL
-- literals, the invalid-encoding error path, and a round-trip through a query.
module Pqi.Conformance.Operation.EscapeStringConn
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (execScenario)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "escapeStringConn" do
    it "escapes a range of strings" \conninfo ->
      differential proxy conninfo \connection ->
        traverse (escapeStringConn connection) stringCases

    it "rejects invalid encoding" \conninfo ->
      differential proxy conninfo \connection ->
        escapeStringConn connection "\255\254"

    it "produces literals that round-trip through a query" \conninfo ->
      differential proxy conninfo \connection -> do
        escaped <- escapeStringConn connection "it's \\ tricky\nstuff"
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
