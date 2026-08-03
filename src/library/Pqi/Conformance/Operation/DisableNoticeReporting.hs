-- | Coverage for 'Pqi.disableNoticeReporting': turning off notice
-- accumulation again so subsequently raised notices are not retained.
module Pqi.Conformance.Operation.DisableNoticeReporting
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "disableNoticeReporting" do
    it "stops retaining notices once disabled" \conninfo ->
      differential adapter conninfo \connection -> do
        Pqi.enableNoticeReporting connection
        whileEnabled <- raiseNoticeAndCollect connection
        Pqi.disableNoticeReporting connection
        whileDisabled <- raiseNoticeAndCollect connection
        pure (whileEnabled, whileDisabled)

raiseNoticeAndCollect :: Pqi.Connection -> IO Bool
raiseNoticeAndCollect connection = do
  _ <- Pqi.exec connection "do $$ begin raise notice 'conformance notice'; end $$"
  isJust <$> Pqi.getNotice connection
