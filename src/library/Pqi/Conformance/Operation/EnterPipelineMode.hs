-- | Coverage for 'Pqi.enterPipelineMode': switching a connection into
-- pipeline mode (idempotently).
module Pqi.Conformance.Operation.EnterPipelineMode
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "enterPipelineMode" do
    it "enters pipeline mode and is idempotent" \conninfo ->
      differential proxy conninfo \connection -> do
        before <- pipelineStatus connection
        entered <- enterPipelineMode connection
        enteredAgain <- enterPipelineMode connection
        while <- pipelineStatus connection
        _ <- exitPipelineMode connection
        pure (before, entered, enteredAgain, while)
