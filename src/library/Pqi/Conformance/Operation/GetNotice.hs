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

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "getNotice" do
    it "yields the formatted notice while enabled and then drains" \conninfo ->
      differential adapter conninfo \connection -> do
        Pqi.enableNoticeReporting connection
        _ <- Pqi.exec connection "do $$ begin raise notice 'conformance notice'; end $$"
        firstNotice <- Pqi.getNotice connection
        afterDrain <- Pqi.getNotice connection
        pure (firstNotice, afterDrain)
