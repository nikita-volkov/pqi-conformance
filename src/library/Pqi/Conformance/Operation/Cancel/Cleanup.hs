-- | Regression coverage for the stale-cancel bug that corrupts a connection
-- after a pipelined query completes.
--
-- Root cause: in pqi-native, 'Pqi.getResult' returns 'Nothing' (the pipeline
-- separator) __before__ reading the trailing 'ReadyForQuery' message, leaving
-- @asyncPending = True@.  If 'Pqi.cancel' is called while @asyncPending@ is
-- still @True@ — as 'Hasql.Comms.Session.cleanUpAfterInterruption' does after
-- draining results — a cancel request is sent to the server even though the
-- query has already finished.  The server receives the signal, sets
-- @QueryCancelPending@, and the __next__ command (e.g. @ABORT@) is cancelled
-- with SQLSTATE @57014@, leaving the connection unusable.
module Pqi.Conformance.Operation.Cancel.Cleanup
  ( spec,
  )
where

import Control.Exception (bracket)
import qualified Pqi
import qualified Pqi as Lq
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "cancel cleanup" do
    -- Reproduces the state that hasql's cleanUpAfterInterruption reaches after
    -- a timeout fires mid-pipeline:
    --
    --   1. drainResults reads CommandComplete → separator, exits loop.
    --      In pqi-native asyncPending is still True here (ReadyForQuery not
    --      yet consumed); in libpq the ReadyForQuery has already been
    --      processed internally.
    --   2. cancel is called.  pqi-native sends a cancel because
    --      asyncPending=True; the query has long since finished so this is
    --      a stale cancel.
    --   3. drainResults reads the remaining ReadyForQuery.
    --   4. The connection re-enters serial mode.
    --   5. exec runs a follow-up command — it must NOT be cancelled by the
    --      stale signal that arrived in step 2.
    it "does not corrupt subsequent commands when cancel is called after pipeline results are drained" \conninfo ->
      bracket (adapter.connectdb conninfo) (.finish) \connection -> do
        -- Enter pipeline mode and dispatch a fast query.
        _ <- connection.enterPipelineMode
        _ <- connection.sendQueryParams "select 1" [] Lq.Text
        _ <- connection.pipelineSync

        -- Wait for the server to process the query so both messages
        -- (CommandComplete + ReadyForQuery) are already in the socket by the
        -- time we start reading.
        threadDelay 10_000 -- 10 ms

        -- Drain the command result then the pipeline separator (Nothing).
        -- After this loop exits, asyncPending=True in pqi-native because
        -- ReadyForQuery has not been read yet.
        let drainAll = do
              mr <- connection.getResult
              case mr of
                Nothing -> pure ()
                Just _ -> drainAll
        drainAll

        -- Send cancel.  Because asyncPending=True in pqi-native, a cancel
        -- request is dispatched to the server despite the query being done.
        handle <- connection.getCancel
        for_ handle (.cancel)

        -- Give the stale cancel enough time to reach the server and set
        -- QueryCancelPending before the next command arrives.
        threadDelay 10_000 -- 10 ms

        -- Read the ReadyForQuery that was still pending.
        drainAll

        -- Exit pipeline mode (sends an implicit Sync in the reference impl).
        _ <- connection.exitPipelineMode

        -- A follow-up command must succeed; 57014 here means the stale cancel
        -- corrupted the connection.
        mResult <- connection.exec "select 1"
        case mResult of
          Nothing -> expectationFailure "exec returned no result after pipeline cleanup"
          Just result -> do
            status <- result.resultStatus
            status `shouldBe` Lq.TuplesOk
