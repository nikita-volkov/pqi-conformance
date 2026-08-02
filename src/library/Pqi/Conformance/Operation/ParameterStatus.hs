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
        before <- connection.parameterStatus "application_name"
        _ <- connection.exec "set application_name to 'pqi-conformance'"
        after <- connection.parameterStatus "application_name"
        clientEncoding <- connection.parameterStatus "client_encoding"
        standardConformingStrings <- connection.parameterStatus "standard_conforming_strings"
        integerDatetimes <- connection.parameterStatus "integer_datetimes"
        missing <- connection.parameterStatus "no_such_parameter"
        pure (before, after, clientEncoding, standardConformingStrings, integerDatetimes, missing)
