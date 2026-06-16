-- | Coverage for 'Pqi.finish': closing a connection releases it without
-- error, whether the connection was open or the null sentinel.
module Pqi.Conformance.Operation.Finish
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "finish" do
    it "closes an open connection cleanly" \conninfo ->
      differentialConnect proxy conninfo \(_ :: Proxy c) conninfo' -> do
        connection <- connectdb conninfo' :: IO c
        before <- status connection
        finish connection
        pure before

    it "closes the null sentinel cleanly" \conninfo ->
      differentialConnect proxy conninfo \(_ :: Proxy c) _ -> do
        connection <- newNullConnection :: IO c
        finish connection
        pure ()
