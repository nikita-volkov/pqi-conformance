-- | Reproduces
-- <https://github.com/nikita-volkov/pqi-native/issues/9>: after a pipelined
-- statement fails with a server error, a command still queued behind it in
-- that same pipeline is discarded by the server without any response at
-- all (per the pipelining protocol, everything after the failure is
-- skipped until the next @Sync@). A later pipeline sent on the same
-- connection can then misattribute its own @ParseComplete@ to that silently
-- discarded command instead of to itself.
--
-- The discarded command needs to be sent via 'Lq.sendPrepare' specifically:
-- a discarded 'Lq.sendQueryParams' command leaks the same way internally,
-- but every leaked entry it produces is indistinguishable from a correctly
-- popped one, so the symptom never surfaces. A discarded 'Lq.sendPrepare'
-- leaks an entry that, if later popped by an unrelated command's
-- @ParseComplete@, makes that command terminate immediately as
-- 'Lq.CommandOk' instead of collecting its real result - observable here as
-- the second pipeline's plain @SELECT@ coming back 'Lq.CommandOk' instead of
-- 'Lq.TuplesOk' with an empty row list.
module Pqi.Conformance.Operation.PipelineSync.Desync
  ( spec,
  )
where

import qualified Pqi
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (takeCommandResults, takeResult)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "desync after a mid-pipeline server error" do
    it "a later pipeline's results are not misattributed" \conninfo ->
      differential adapter conninfo \connection -> do
        entered <- Lq.enterPipelineMode connection

        -- First pipeline: a statement that fails with a server error,
        -- followed by a named-statement prepare that gets silently
        -- discarded as a consequence (the server sends nothing for it).
        failingSent <- Lq.sendQueryParams connection "select 1 / 0" [] Lq.Text
        discardedSent <- Lq.sendPrepare connection "" "select 99" Nothing
        firstSynced <- Lq.pipelineSync connection
        failed <- takeCommandResults connection
        discarded <- takeCommandResults connection
        firstSyncResult <- takeResult connection

        -- Second pipeline, on the same connection: a plain SELECT that must
        -- come back as TuplesOk with an empty row list, not CommandOk.
        selectSent <- Lq.sendQueryParams connection "select 1 where false" [] Lq.Text
        secondSynced <- Lq.pipelineSync connection
        selected <- takeCommandResults connection
        secondSyncResult <- takeResult connection

        exited <- Lq.exitPipelineMode connection
        pure
          ( entered,
            failingSent,
            discardedSent,
            firstSynced,
            failed,
            discarded,
            firstSyncResult,
            selectSent,
            secondSynced,
            selected,
            secondSyncResult,
            exited
          )
