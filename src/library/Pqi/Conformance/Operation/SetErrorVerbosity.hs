-- | Coverage for 'Pqi.setErrorVerbosity': setting error verbosity, which
-- returns the previous setting.
module Pqi.Conformance.Operation.SetErrorVerbosity
  ( spec,
  )
where

import Pqi (IsConnection (..))
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "setErrorVerbosity" do
    it "returns the previous setting" \conninfo ->
      differential proxy conninfo \connection -> do
        beforeTerse <- setErrorVerbosity connection Lq.ErrorsTerse
        beforeVerbose <- setErrorVerbosity connection Lq.ErrorsVerbose
        beforeDefault <- setErrorVerbosity connection Lq.ErrorsDefault
        pure (beforeTerse, beforeVerbose, beforeDefault)
