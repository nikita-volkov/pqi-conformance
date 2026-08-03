-- | Coverage for 'Pqi.fmod': the type modifier of each column (e.g. the
-- precision\/scale of a @numeric@), including an out-of-range index.
module Pqi.Conformance.Operation.Fmod
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "fmod" do
    it "reports type modifiers and degrades out of range" \conninfo ->
      differential adapter conninfo \connection -> do
        result <-
          connection . exec "select 1.5 :: numeric(10,2), 'pad' :: char(5), 'x' :: varchar(3), true, 1 :: int4"
        for result \r -> do
          n <- r . nfields
          modifiers <- traverse r . fmod [0 .. n - 1]
          outOfRange <- r . fmod 9
          pure (modifiers, outOfRange)
