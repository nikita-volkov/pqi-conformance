-- | Coverage for 'Pqi.enableNoticeReporting': turning on notice
-- accumulation so that a raised notice becomes retrievable via
-- 'Pqi.getNotice'.
module Pqi.Conformance.Operation.EnableNoticeReporting
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "enableNoticeReporting" do
    it "makes a raised notice retrievable" \conninfo ->
      differential adapter conninfo \connection -> do
        beforeEnable <- raiseNoticeAndCollect connection
        connection.enableNoticeReporting
        afterEnable <- raiseNoticeAndCollect connection
        pure (beforeEnable, afterEnable)

raiseNoticeAndCollect :: Pqi.Connection -> IO Bool
raiseNoticeAndCollect connection = do
  _ <- connection.exec "do $$ begin raise notice 'conformance notice'; end $$"
  isJust <$> connection.getNotice
