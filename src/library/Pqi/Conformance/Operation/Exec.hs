-- | Coverage for 'Pqi.exec': the simple-query protocol over a range of
-- result shapes, command tags, and degenerate inputs.
module Pqi.Conformance.Operation.Exec
  ( spec,
  )
where

import Pqi (IsConnection)
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "exec" do
    let forCase title sql =
          it title \conninfo -> differential proxy conninfo (execScenario sql)
    forCase "select literal" "select 1"
    forCase "multi-row, multi-column" "select i, i * 2 from generate_series (1, 3) as i"
    forCase "nulls and text" "select null :: int4, 'hello' :: text, true"
    forCase "no rows" "select 1 where false"
    forCase "zero columns" "select"
    forCase "empty query" ""
    forCase "semicolon only" ";"
    forCase "comment only" "-- nothing to see here"
    forCase "whitespace only" "   "
    forCase "multiple statements take the last result" "select 1; select 2, 3"
    forCase "non-ASCII text" "select 'héllo🐘' as greeting"
    forCase "bytea hex output" "select '\\xdeadbeef' :: bytea"
    forCase "large value" "select repeat('x', 100000)"
    forCase "many rows" "select i from generate_series (1, 1000) as i"
    forCase "show command" "show server_version"
    forCase "set command" "set application_name to 'conformance-exec'"
    forCase "DDL command tag" "create temporary table conformance_exec (id int4)"
    forCase "DDL with a notice" "drop table if exists pqi_conformance_absent"
