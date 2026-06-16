-- | Coverage for 'Pqi.newNullConnection': the sentinel \"null\" connection
-- is reported as null and bad.
module Pqi.Conformance.Operation.NewNullConnection
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "newNullConnection" do
    it "produces a connection that is null and bad" \conninfo ->
      differentialConnect proxy conninfo \(_ :: Proxy c) _ -> do
        connection <- newNullConnection :: IO c
        nullness <- pure (isNullConnection connection)
        badness <- status connection
        finish connection
        pure (nullness, badness)
