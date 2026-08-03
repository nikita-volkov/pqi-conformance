-- | Coverage for 'Pqi.host': the server host name of the connection.
module Pqi.Conformance.Operation.Host
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "host" do
    it "reports the host from the conninfo" \conninfo ->
      differential adapter conninfo (.host)
