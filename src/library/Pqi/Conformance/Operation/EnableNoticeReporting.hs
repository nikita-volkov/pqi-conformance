-- | Coverage for 'Pqi.enableNoticeReporting': turning on notice
-- accumulation so that a raised notice becomes retrievable via
-- 'Pqi.getNotice'.
module Pqi.Conformance.Operation.EnableNoticeReporting
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "enableNoticeReporting" do
    it "makes a raised notice retrievable" \conninfo ->
      differential proxy conninfo \connection -> do
        beforeEnable <- raiseNoticeAndCollect connection
        enableNoticeReporting connection
        afterEnable <- raiseNoticeAndCollect connection
        pure (beforeEnable, afterEnable)

raiseNoticeAndCollect :: (IsConnection c) => c -> IO Bool
raiseNoticeAndCollect connection = do
  _ <- exec connection "do $$ begin raise notice 'conformance notice'; end $$"
  isJust <$> getNotice connection
