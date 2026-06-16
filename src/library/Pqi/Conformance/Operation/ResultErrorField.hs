-- | Coverage for 'Pqi.resultErrorField': the structured fields carried by
-- a wire error response — SQLSTATE, severity, primary message, detail, hint,
-- positions, internal query, context, and source location.
--
-- These all come from the wire error response and are compared in full
-- (via 'Pqi.Conformance.Observation.observeResult', which captures every
-- 'Pqi.FieldCode').
module Pqi.Conformance.Operation.ResultErrorField
  ( spec,
  )
where

import Pqi (IsConnection)
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (execAllScenario, execScenario)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "resultErrorField" do
    let forCase title sql =
          it title \conninfo -> differential proxy conninfo (execScenario sql)
    forCase "syntax error" "selct 1"
    forCase "undefined table" "select * from pqi_no_such_table"
    forCase "undefined column" "select no_such_column from (select 1) as t"
    forCase "division by zero" "select 1 / 0"
    forCase "statement position past a prefix" "select 1 where tlse"
    forCase
      "detail, hint, and a custom errcode"
      "do $$ begin raise exception 'boom' using detail = 'the detail', hint = 'the hint', errcode = 'P0123'; end $$"
    forCase "internal query and position" "do $$ begin execute 'selct 1'; end $$"
    forCase "value too long" "select 'abc' :: varchar(2)"

    it "constraint violations" \conninfo ->
      differential proxy conninfo
        $ execAllScenario
          [ "create temporary table conformance_errors (id int4 primary key, label text not null)",
            "insert into conformance_errors values (1, 'a')",
            "insert into conformance_errors values (1, 'b')",
            "insert into conformance_errors values (2, null)"
          ]

    it "a failed transaction block rejects further commands" \conninfo ->
      differential proxy conninfo
        $ execAllScenario ["begin", "select 1 / 0", "select 1", "rollback", "select 1"]
