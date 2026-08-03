-- | Coverage for 'Pqi.options': the command-line options of the
-- connection request.
module Pqi.Conformance.Operation.Options
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "options" do
    it "reports the command-line options from the conninfo" \conninfo ->
      differential adapter conninfo (. options)
