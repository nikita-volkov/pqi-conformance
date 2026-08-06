-- | Coverage for 'Pqi.resStatus': rendering each 'Pqi.ExecStatus' as the
-- string describing its status code.
module Pqi.Conformance.Operation.ResStatus
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Prelude
import qualified Pqi.Conformance.Reference as Reference
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "resStatus" do
    for_ [minBound .. maxBound] \execStatus ->
      it (show execStatus) \_ -> do
        candidate <- Pqi.resStatus adapter execStatus
        reference <- Pqi.resStatus Reference.adapter execStatus
        candidate `shouldBe` reference
