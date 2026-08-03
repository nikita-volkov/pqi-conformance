-- | Coverage for 'Pqi.finish': closing a connection releases it without
-- error, whether the connection was open or the null sentinel.
module Pqi.Conformance.Operation.Finish
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "finish" do
    it "closes an open connection cleanly" \conninfo ->
      differentialConnect adapter conninfo \adapter' conninfo' -> do
        connection <- adapter' . connectdb conninfo'
        before <- connection . status
        connection . finish
        pure before

    it "closes the null sentinel cleanly" \conninfo ->
      differentialConnect adapter conninfo \adapter' _ -> do
        connection <- adapter' . newNullConnection
        connection . finish
        pure ()
