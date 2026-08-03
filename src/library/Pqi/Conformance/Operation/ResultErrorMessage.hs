-- | Coverage for 'Pqi.resultErrorMessage': the formatted error string on a
-- failed result, and empty on a successful one.
--
-- The message is compared byte-identically: both adapters must produce the
-- same formatted string as libpq's @PQresultErrorMessage@ at DEFAULT
-- verbosity. The failure scenario uses a @RAISE EXCEPTION@ from a PL\/pgSQL
-- anonymous block, which produces an error without a statement-position field
-- (@'P'@), so the formatted string is fully reproducible from the wire error
-- fields alone.
module Pqi.Conformance.Operation.ResultErrorMessage
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "resultErrorMessage" do
    it "matches the full formatted libpq error string on failure and is empty on success" \conninfo ->
      differential adapter conninfo \connection -> do
        failed <-
          Pqi.exec connection "do $$ begin raise exception 'conformance error'; end $$"
            >>= traverse Pqi.resultErrorMessage
        succeeded <-
          Pqi.exec connection "select 1"
            >>= traverse Pqi.resultErrorMessage
        pure (failed, succeeded)
