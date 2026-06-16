-- | Coverage for 'Pqi.options': the command-line options of the
-- connection request.
module Pqi.Conformance.Operation.Options
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "options" do
    it "reports the command-line options from the conninfo" \conninfo ->
      differential proxy conninfo options
