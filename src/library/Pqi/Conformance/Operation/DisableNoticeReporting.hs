-- | Coverage for 'Pqi.disableNoticeReporting': turning off notice
-- accumulation again so subsequently raised notices are not retained.
module Pqi.Conformance.Operation.DisableNoticeReporting
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "disableNoticeReporting" do
    it "stops retaining notices once disabled" \conninfo ->
      differential proxy conninfo \connection -> do
        enableNoticeReporting connection
        whileEnabled <- raiseNoticeAndCollect connection
        disableNoticeReporting connection
        whileDisabled <- raiseNoticeAndCollect connection
        pure (whileEnabled, whileDisabled)

raiseNoticeAndCollect :: (IsConnection c) => c -> IO Bool
raiseNoticeAndCollect connection = do
  _ <- exec connection "do $$ begin raise notice 'conformance notice'; end $$"
  isJust <$> getNotice connection
