-- | Coverage for 'Pqi.setErrorVerbosity': setting error verbosity, which
-- returns the previous setting.
module Pqi.Conformance.Operation.SetErrorVerbosity
  ( spec,
  )
where

import qualified Pqi
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "setErrorVerbosity" do
    it "returns the previous setting" \conninfo ->
      differential adapter conninfo \connection -> do
        beforeTerse <- Lq.setErrorVerbosity connection Lq.ErrorsTerse
        beforeVerbose <- Lq.setErrorVerbosity connection Lq.ErrorsVerbose
        beforeDefault <- Lq.setErrorVerbosity connection Lq.ErrorsDefault
        pure (beforeTerse, beforeVerbose, beforeDefault)
