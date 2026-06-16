-- | Coverage for 'Pqi.execParams': the extended query protocol over
-- parameter formats, result formats, inferred types, nulls, and its error
-- paths.
module Pqi.Conformance.Operation.ExecParams
  ( spec,
  )
where

import Pqi (IsConnection (..))
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "execParams" do
    it "text result format" \conninfo ->
      differential proxy conninfo (paramsScenario Lq.Text)
    it "binary result format" \conninfo ->
      differential proxy conninfo (paramsScenario Lq.Binary)
    it "null parameter" \conninfo ->
      differential proxy conninfo (observed "select $1 :: int4 as maybe_value" [Nothing] Lq.Text)
    it "no parameters" \conninfo ->
      differential proxy conninfo (observed "select 'none' :: text" [] Lq.Text)
    it "binary parameter" \conninfo ->
      differential proxy conninfo
        $ observed "select $1 :: int4 * 2" [Just (int4Oid, "\NUL\NUL\NUL*", Lq.Binary)] Lq.Text
    it "binary bytea round-trip" \conninfo ->
      differential proxy conninfo
        $ observed "select $1 :: bytea" [Just (byteaOid, "\NUL\1\2\255", Lq.Binary)] Lq.Binary
    it "inferred parameter type" \conninfo ->
      differential proxy conninfo
        $ observed "select $1 :: int4 + 1" [Just (0, "41", Lq.Text)] Lq.Text
    it "empty string is not null" \conninfo ->
      differential proxy conninfo
        $ observed "select $1 :: text, length ($1 :: text)" [Just (textOid, "", Lq.Text)] Lq.Text
    it "many mixed parameters" \conninfo ->
      differential proxy conninfo
        $ observed
          "select $1 :: int8, $2 :: float8, $3 :: bool, $4 :: text, $5 :: int4, $6 :: text"
          [ Just (int8Oid, "9000000000000000000", Lq.Text),
            Just (float8Oid, "2.5", Lq.Text),
            Just (boolOid, "t", Lq.Text),
            Nothing,
            Just (int4Oid, "-1", Lq.Text),
            Just (textOid, "héllo", Lq.Text)
          ]
          Lq.Text
    it "DML with parameters" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "create temporary table conformance_exec_params (id int4)"
        insert <-
          observed
            "insert into conformance_exec_params values ($1), ($2)"
            [Just (int4Oid, "1", Lq.Text), Just (int4Oid, "2", Lq.Text)]
            Lq.Text
            connection
        check <-
          observed
            "select count(*) from conformance_exec_params where id <= $1"
            [Just (int4Oid, "2", Lq.Text)]
            Lq.Text
            connection
        pure (insert, check)
    it "too few parameters" \conninfo ->
      differential proxy conninfo
        $ observed "select $1 :: int4 + $2 :: int4" [Just (int4Oid, "1", Lq.Text)] Lq.Text
    it "malformed parameter value" \conninfo ->
      differential proxy conninfo
        $ observed "select $1 :: int4" [Just (int4Oid, "not-a-number", Lq.Text)] Lq.Text
    it "multiple statements are rejected" \conninfo ->
      differential proxy conninfo (observed "select 1; select 2" [] Lq.Text)

paramsScenario :: (IsConnection c) => Lq.Format -> c -> IO (Maybe ResultObservation)
paramsScenario resultFormat =
  observed
    "select $1 :: int4 + $2 :: int4 as sum, $3 :: text as label"
    [ Just (int4Oid, "40", Lq.Text),
      Just (int4Oid, "2", Lq.Text),
      Just (textOid, "hi", Lq.Text)
    ]
    resultFormat
