-- | Coverage for 'Pqi.escapeIdentifier': escaping SQL identifiers
-- (including the surrounding quotes) and a round-trip through a query.
module Pqi.Conformance.Operation.EscapeIdentifier
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
  describe "escapeIdentifier" do
    it "escapes a range of identifiers" \conninfo ->
      differential proxy conninfo \connection ->
        traverse (escapeIdentifier connection) identifierCases

    it "produces identifiers that round-trip through a query" \conninfo ->
      differential proxy conninfo \connection -> do
        escaped <- escapeIdentifier connection "Wéird \"column\" name"
        for escaped \identifier ->
          execScenario ("select 1 as " <> identifier) connection
  where
    identifierCases =
      [ "plain",
        "MixedCase",
        "with space",
        "with\"quote",
        "héllo",
        "select"
      ]
