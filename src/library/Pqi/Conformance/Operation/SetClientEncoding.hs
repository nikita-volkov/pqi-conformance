-- | Coverage for 'Pqi.setClientEncoding': changing the client encoding,
-- including rejection of an unknown encoding (which leaves the session intact).
module Pqi.Conformance.Operation.SetClientEncoding
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (execScenario)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "setClientEncoding" do
    it "rejects an unknown encoding and leaves the session usable" \conninfo ->
      differential proxy conninfo \connection -> do
        rejected <- setClientEncoding connection "BOGUS_ENCODING"
        unchanged <- clientEncoding connection
        stillIdle <- transactionStatus connection
        stillWorks <- execScenario "select 1" connection
        pure (rejected, unchanged, stillIdle, stillWorks)
