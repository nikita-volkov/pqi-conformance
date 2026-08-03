-- | Coverage for 'Pqi.setClientEncoding': changing the client encoding,
-- including rejection of an unknown encoding (which leaves the session intact).
module Pqi.Conformance.Operation.SetClientEncoding
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (execScenario)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "setClientEncoding" do
    it "rejects an unknown encoding and leaves the session usable" \conninfo ->
      differential adapter conninfo \connection -> do
        rejected <- Pqi.setClientEncoding connection "BOGUS_ENCODING"
        unchanged <- Pqi.clientEncoding connection
        stillIdle <- Pqi.transactionStatus connection
        stillWorks <- execScenario "select 1" connection
        pure (rejected, unchanged, stillIdle, stillWorks)
