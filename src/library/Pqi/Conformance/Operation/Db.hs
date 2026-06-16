-- | Coverage for 'Pqi.db': the database name of the connection.
module Pqi.Conformance.Operation.Db
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "db" do
    it "reports the database name from the conninfo" \conninfo ->
      differential proxy conninfo db
