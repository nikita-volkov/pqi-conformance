-- | Coverage for 'Pqi.backendPID': the backend process ID.
--
-- The PID differs between the candidate's and the reference's backends, so
-- only its positivity is compared.
module Pqi.Conformance.Operation.BackendPID
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "backendPID" do
    it "is positive on an open connection" \conninfo ->
      differential adapter conninfo \connection ->
        (> 0) <$> Pqi.backendPID connection
