-- | Coverage for 'Pqi.cmdTuples': the affected-row count (as text) for
-- each kind of command.
module Pqi.Conformance.Operation.CmdTuples
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "cmdTuples" do
    it "reports the affected-row count for each command" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.exec "create temporary table conformance_cmd_tuples (id int4, label text)"
        let countOf sql = connection.exec sql >>= traverse (.cmdTuples)
        insert <- countOf "insert into conformance_cmd_tuples values (1, 'a'), (2, 'b')"
        update <- countOf "update conformance_cmd_tuples set label = 'c' where id = 1"
        delete <- countOf "delete from conformance_cmd_tuples where id = 2"
        select <- countOf "select * from conformance_cmd_tuples"
        ddl <- countOf "create temporary table conformance_cmd_tuples_2 (id int4)"
        pure (insert, update, delete, select, ddl)
