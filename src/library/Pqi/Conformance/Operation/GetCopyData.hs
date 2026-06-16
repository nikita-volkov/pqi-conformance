-- | Coverage for 'Pqi.getCopyData': receiving rows from a
-- @COPY TO STDOUT@ in text, CSV, and binary formats.
module Pqi.Conformance.Operation.GetCopyData
  ( spec,
  )
where

import Pqi (IsConnection)
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (collectCopyOut, drainResults, execScenario)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "getCopyData" do
    let copyOut sql conninfo =
          differential proxy conninfo \connection -> do
            started <- execScenario sql connection
            rows <- collectCopyOut connection
            outcome <- drainResults connection
            pure (started, rows, outcome)
    it "receives text rows"
      $ copyOut "copy (select i, i * 2 from generate_series (1, 3) as i) to stdout"
    it "receives CSV rows with a header"
      $ copyOut
        "copy (select i as n, 'v' || i as v from generate_series (1, 2) as i) to stdout (format csv, header)"
    it "receives binary rows"
      $ copyOut "copy (select i from generate_series (1, 2) as i) to stdout (format binary)"
