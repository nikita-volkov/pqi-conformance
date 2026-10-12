-- | Coverage for 'Pqi.getResult' when the connection is lost between the
-- command being sent and its results being read - which is how a pool
-- connection that died while idle shows up to its next user. Every
-- asynchronous way of starting a command is covered ('Pqi.sendQuery',
-- 'Pqi.sendQueryParams', 'Pqi.sendPrepare', 'Pqi.sendQueryPrepared', and a
-- pipeline), against each way of dying: a silent close, a server-announced
-- termination (@pg_terminate_backend@), and a reset.
--
-- The contract is @libpq@'s: 'Pqi.getResult' never throws on a lost
-- connection. It returns a 'Pqi.FatalError' result - for a termination
-- first the server's own @FATAL@ (while 'Pqi.status' is still
-- 'Pqi.ConnectionOk'), then a made-up one without error fields whose message
-- is \"server closed the connection unexpectedly\" - and 'Pqi.status'
-- becomes 'Pqi.ConnectionBad'. A consumer written against that contract
-- (hasql's @Connection.use@ and pool, notably, which classify a dead
-- connection by its status) breaks on anything else: see
-- <https://github.com/nikita-volkov/hasql/issues/341>.
--
-- The scenario also reuses the handle afterwards and finishes it, because
-- the divergence that was found left the handle in a state in which
-- 'Pqi.finish' never returned. See
-- "Pqi.Conformance.Scenario.LostConnection" for what is observed and how a
-- throw or a hang is folded into the observation, and
-- "Pqi.Conformance.Fixture.ConnectionLoss" for why the connection goes
-- through a proxy instead of being killed directly.
module Pqi.Conformance.Operation.GetResult.LostConnection
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Prelude
import qualified Pqi.Conformance.Scenario.LostConnection as LostConnection
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "getResult" do
    LostConnection.spec
      adapter
      [ ( "sendQuery, a multi-statement text",
          LostConnection.asynchronous LostConnection.UntilNothing \connection ->
            pure <$> Pqi.sendQuery connection "select 1; select 2"
        ),
        ( "sendQueryParams",
          LostConnection.asynchronous LostConnection.UntilNothing \connection ->
            pure <$> Pqi.sendQueryParams connection "select 1" [] Pqi.Text
        ),
        ( "sendPrepare",
          LostConnection.asynchronous LostConnection.UntilNothing \connection ->
            pure <$> Pqi.sendPrepare connection "lost_connection" "select 1" Nothing
        ),
        ( "sendQueryPrepared, a statement prepared before the connection was lost",
          LostConnection.withPreparedStatement "lost_connection" "select 1" do
            LostConnection.asynchronous LostConnection.UntilNothing \connection ->
              pure <$> Pqi.sendQueryPrepared connection "lost_connection" [] Pqi.Text
        ),
        ( "a pipeline of two commands and a sync",
          LostConnection.asynchronous LostConnection.UntilPipelineSync \connection -> do
            entered <- Pqi.enterPipelineMode connection
            first <- Pqi.sendQueryParams connection "select 1" [] Pqi.Text
            second <- Pqi.sendQueryParams connection "select 2" [] Pqi.Text
            synced <- Pqi.pipelineSync connection
            pure [entered, first, second, synced]
        )
      ]
