-- | Coverage for 'Pqi.errorMessage': the connection-level error string.
--
-- The goal is byte-identical output to libpq's @PQerrorMessage@ in all
-- documented scenarios. Error strings are compared in full — not just for
-- presence — so formatting bugs are caught. Scenarios are chosen to avoid
-- statement-position fields (@'P'@), which depend on the client-stored query
-- text and cannot be reproduced from wire fields alone.
--
-- The one structurally incomparable value is the null-connection sentinel
-- @\"connection pointer is NULL\\n\"@: it is hardcoded in libpq rather than
-- derived from a wire response, but both adapters must return exactly that
-- string.
module Pqi.Conformance.Operation.ErrorMessage
  ( spec,
  )
where

import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "errorMessage" do
    it "is empty on a healthy connection" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "select 1"
        errorMessage connection

    it "is populated after a failed exec and cleared by a subsequent success" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "do $$ begin raise exception 'conformance error'; end $$"
        afterFail <- errorMessage connection
        _ <- exec connection "select 1"
        afterSuccess <- errorMessage connection
        pure (afterFail, afterSuccess)

    it "is populated after a failed getResult and cleared by a subsequent success" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- sendQuery connection "do $$ begin raise exception 'conformance error'; end $$"
        _ <- drainResults connection
        afterFail <- errorMessage connection
        _ <- sendQuery connection "select 1"
        _ <- drainResults connection
        afterSuccess <- errorMessage connection
        pure (afterFail, afterSuccess)

    it "is populated after a connection failure" \conninfo ->
      differentialConnect proxy conninfo \(_ :: Proxy c) conninfo' -> do
        conn <- connectdb (conninfo' <> " user=pqi_no_such_user") :: IO c
        msg <- errorMessage conn
        finish conn
        pure msg

    it "is the null-connection sentinel on a null connection" \conninfo ->
      differentialConnect proxy conninfo \(_ :: Proxy c) _ -> do
        conn <- newNullConnection :: IO c
        msg <- errorMessage conn
        finish conn
        pure msg
