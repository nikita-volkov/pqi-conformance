-- | Coverage for 'Pqi.backendPID': the backend process ID.
--
-- The PID differs between the candidate's and the reference's backends, so
-- only its positivity is compared.
module Pqi.Conformance.Operation.BackendPID
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "backendPID" do
    it "is positive on an open connection" \conninfo ->
      differential proxy conninfo \connection ->
        (> 0) <$> backendPID connection
