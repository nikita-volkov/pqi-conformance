-- | Coverage for 'Pqi.exec' when the connection was lost before the call,
-- as it is for the next user of a pooled connection that died while idle,
-- against each way of dying: a silent close, a server-announced termination
-- (@pg_terminate_backend@), and a reset.
--
-- The contract is @libpq@'s: 'Pqi.exec' never throws on a lost connection.
-- It returns a 'Pqi.FatalError' result (or 'Nothing', where @libpq@ does),
-- and 'Pqi.status' becomes 'Pqi.ConnectionBad' with the message in
-- 'Pqi.errorMessage'. Distinct from
-- "Pqi.Conformance.Operation.Exec.ConnectionLostMidQuery", where a
-- hand-rolled server drops the connection /after/ reading the query; here
-- the server is the real one, and it is gone before the query is sent. See
-- <https://github.com/nikita-volkov/hasql/issues/341>.
--
-- See "Pqi.Conformance.Scenario.LostConnection" for what is observed and how
-- a throw or a hang is folded into the observation, and
-- "Pqi.Conformance.Fixture.ConnectionLoss" for why the connection goes
-- through a proxy instead of being killed directly.
module Pqi.Conformance.Operation.Exec.LostConnection
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Prelude
import qualified Pqi.Conformance.Scenario.LostConnection as LostConnection
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "exec" do
    LostConnection.spec
      adapter
      [ ( "a single statement",
          LostConnection.synchronous (`Pqi.exec` "select 1")
        )
      ]
