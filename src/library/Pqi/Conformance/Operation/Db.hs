-- | Coverage for 'Pqi.db': the database name of the connection.
module Pqi.Conformance.Operation.Db
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "db" do
    it "reports the database name from the conninfo" \conninfo ->
      differential adapter conninfo (. db)
