-- | Coverage for 'Pqi.enterPipelineMode': switching a connection into
-- pipeline mode (idempotently).
module Pqi.Conformance.Operation.EnterPipelineMode
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "enterPipelineMode" do
    it "enters pipeline mode and is idempotent" \conninfo ->
      differential adapter conninfo \connection -> do
        before <- Pqi.pipelineStatus connection
        entered <- Pqi.enterPipelineMode connection
        enteredAgain <- Pqi.enterPipelineMode connection
        while <- Pqi.pipelineStatus connection
        _ <- Pqi.exitPipelineMode connection
        pure (before, entered, enteredAgain, while)
