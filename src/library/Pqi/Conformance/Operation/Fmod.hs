-- | Coverage for 'Pqi.fmod': the type modifier of each column (e.g. the
-- precision\/scale of a @numeric@), including an out-of-range index.
module Pqi.Conformance.Operation.Fmod
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "fmod" do
    it "reports type modifiers and degrades out of range" \conninfo ->
      differential proxy conninfo \connection -> do
        result <-
          exec connection "select 1.5 :: numeric(10,2), 'pad' :: char(5), 'x' :: varchar(3), true, 1 :: int4"
        for result \r -> do
          n <- nfields r
          modifiers <- traverse (fmod r) [0 .. n - 1]
          outOfRange <- fmod r 9
          pure (modifiers, outOfRange)
