-- | Coverage for 'Pqi.isNullConnection': distinguishes the null sentinel
-- from a genuinely open connection.
module Pqi.Conformance.Operation.IsNullConnection
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "isNullConnection" do
    it "is True for the null sentinel and False for an open connection" \conninfo ->
      differentialConnect adapter conninfo \adapter' conninfo' -> do
        nullConn <- Pqi.newNullConnection adapter'
        let nullIsNull = Pqi.isNullConnection nullConn
        Pqi.finish nullConn
        openConn <- Pqi.connectdb adapter' conninfo'
        let openIsNull = Pqi.isNullConnection openConn
        Pqi.finish openConn
        pure (nullIsNull, openIsNull)
