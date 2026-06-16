-- | Coverage for 'Pqi.parameterStatus': server-reported parameter
-- settings, including @GUC_REPORT@ updates and an absent parameter.
module Pqi.Conformance.Operation.ParameterStatus
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "parameterStatus" do
    it "reports parameter statuses, including GUC_REPORT updates" \conninfo ->
      differential proxy conninfo \connection -> do
        before <- parameterStatus connection "application_name"
        _ <- exec connection "set application_name to 'pqi-conformance'"
        after <- parameterStatus connection "application_name"
        clientEncoding <- parameterStatus connection "client_encoding"
        standardConformingStrings <- parameterStatus connection "standard_conforming_strings"
        integerDatetimes <- parameterStatus connection "integer_datetimes"
        missing <- parameterStatus connection "no_such_parameter"
        pure (before, after, clientEncoding, standardConformingStrings, integerDatetimes, missing)
