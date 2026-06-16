-- | Coverage for 'Pqi.isNullConnection': distinguishes the null sentinel
-- from a genuinely open connection.
module Pqi.Conformance.Operation.IsNullConnection
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "isNullConnection" do
    it "is True for the null sentinel and False for an open connection" \conninfo ->
      differentialConnect proxy conninfo \(_ :: Proxy c) conninfo' -> do
        nullConn <- newNullConnection :: IO c
        let nullIsNull = isNullConnection nullConn
        finish nullConn
        openConn <- connectdb conninfo' :: IO c
        let openIsNull = isNullConnection openConn
        finish openConn
        pure (nullIsNull, openIsNull)
