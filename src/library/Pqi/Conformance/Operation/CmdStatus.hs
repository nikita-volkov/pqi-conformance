-- | Coverage for 'Pqi.cmdStatus': the command status tag (e.g.
-- @\"INSERT 0 2\"@) for each kind of command.
module Pqi.Conformance.Operation.CmdStatus
  ( spec,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "cmdStatus" do
    it "reports the command tag for each command" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- exec connection "create temporary table conformance_cmd_status (id int4, label text)"
        let tagOf sql = exec connection sql >>= traverse cmdStatus
        insert <- tagOf "insert into conformance_cmd_status values (1, 'a'), (2, 'b')"
        update <- tagOf "update conformance_cmd_status set label = 'c' where id = 1"
        delete <- tagOf "delete from conformance_cmd_status where id = 2"
        select <- tagOf "select * from conformance_cmd_status"
        pure (insert, update, delete, select)
