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
        connection . enableNoticeReporting
        whileEnabled <- raiseNoticeAndCollect connection
        connection . disableNoticeReporting
        whileDisabled <- raiseNoticeAndCollect connection
        pure (whileEnabled, whileDisabled)

raiseNoticeAndCollect :: Pqi.Connection -> IO Bool
raiseNoticeAndCollect connection = do
  _ <- connection . exec "do $$ begin raise notice 'conformance notice'; end $$"
  isJust <$> connection . getNotice
