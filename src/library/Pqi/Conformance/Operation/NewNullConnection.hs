-- | Coverage for 'Pqi.newNullConnection': the sentinel \"null\" connection
-- is reported as null and bad.
module Pqi.Conformance.Operation.NewNullConnection
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "newNullConnection" do
    it "produces a connection that is null and bad" \conninfo ->
      differentialConnect adapter conninfo \adapter' _ -> do
        connection <- Pqi.newNullConnection adapter'
        nullness <- pure (Pqi.isNullConnection connection)
        badness <- Pqi.status connection
        Pqi.finish connection
        pure (nullness, badness)
