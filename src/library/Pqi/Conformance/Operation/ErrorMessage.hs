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

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "errorMessage" do
    it "is empty on a healthy connection" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection . exec "select 1"
        connection . errorMessage

    it "is populated after a failed exec and cleared by a subsequent success" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection . exec "do $$ begin raise exception 'conformance error'; end $$"
        afterFail <- connection . errorMessage
        _ <- connection . exec "select 1"
        afterSuccess <- connection . errorMessage
        pure (afterFail, afterSuccess)

    it "is populated after a failed getResult and cleared by a subsequent success" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection . sendQuery "do $$ begin raise exception 'conformance error'; end $$"
        _ <- drainResults connection
        afterFail <- connection . errorMessage
        _ <- connection . sendQuery "select 1"
        _ <- drainResults connection
        afterSuccess <- connection . errorMessage
        pure (afterFail, afterSuccess)

    it "is populated after a connection failure" \conninfo ->
      differentialConnect adapter conninfo \adapter' conninfo' -> do
        conn <- adapter' . connectdb (conninfo' <> " user=pqi_no_such_user")
        msg <- conn . errorMessage
        conn . finish
        pure msg

    it "is the null-connection sentinel on a null connection" \conninfo ->
      differentialConnect adapter conninfo \adapter' _ -> do
        conn <- adapter' . newNullConnection
        msg <- conn . errorMessage
        conn . finish
        pure msg
