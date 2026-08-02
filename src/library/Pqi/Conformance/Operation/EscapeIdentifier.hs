-- | Coverage for 'Pqi.escapeIdentifier': escaping SQL identifiers
-- (including the surrounding quotes) and a round-trip through a query.
module Pqi.Conformance.Operation.EscapeIdentifier
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
  describe "escapeIdentifier" do
    it "escapes a range of identifiers" \conninfo ->
      differential adapter conninfo \connection ->
        traverse connection.escapeIdentifier identifierCases

    it "produces identifiers that round-trip through a query" \conninfo ->
      differential adapter conninfo \connection -> do
        escaped <- connection.escapeIdentifier "Wéird \"column\" name"
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
