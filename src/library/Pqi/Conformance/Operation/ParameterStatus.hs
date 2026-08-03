-- | Coverage for 'Pqi.parameterStatus': server-reported parameter
-- settings, including @GUC_REPORT@ updates and an absent parameter.
module Pqi.Conformance.Operation.ParameterStatus
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "parameterStatus" do
    it "reports parameter statuses, including GUC_REPORT updates" \conninfo ->
      differential adapter conninfo \connection -> do
        before <- Pqi.parameterStatus connection "application_name"
        _ <- Pqi.exec connection "set application_name to 'pqi-conformance'"
        after <- Pqi.parameterStatus connection "application_name"
        clientEncoding <- Pqi.parameterStatus connection "client_encoding"
        standardConformingStrings <- Pqi.parameterStatus connection "standard_conforming_strings"
        integerDatetimes <- Pqi.parameterStatus connection "integer_datetimes"
        missing <- Pqi.parameterStatus connection "no_such_parameter"
        pure (before, after, clientEncoding, standardConformingStrings, integerDatetimes, missing)
