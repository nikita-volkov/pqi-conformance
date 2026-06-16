-- | Coverage for 'Pqi.getNotice': retrieving accumulated notices, which
-- are present only while reporting is enabled and drain after retrieval.
--
-- The notice text is compared byte-identically — both adapters must produce
-- the same formatted string as libpq's notice processor at DEFAULT verbosity.
-- At that verbosity, context (@'W'@ field) is suppressed for NOTICE-level
-- messages, so the formatted string is just @\"NOTICE:  \<message\>\\n\"@.
module Pqi.Conformance.Operation.GetNotice
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "getNotice" do
    it "yields the formatted notice while enabled and then drains" \conninfo ->
      differential proxy conninfo \connection -> do
        enableNoticeReporting connection
        _ <- exec connection "do $$ begin raise notice 'conformance notice'; end $$"
        firstNotice <- getNotice connection
        afterDrain <- getNotice connection
        pure (firstNotice, afterDrain)
