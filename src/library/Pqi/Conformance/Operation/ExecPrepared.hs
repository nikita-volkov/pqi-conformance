-- | Coverage for 'Pqi.execPrepared': executing a prepared statement over
-- named and unnamed statements, parameter formats, and the unknown-statement
-- error path.
module Pqi.Conformance.Operation.ExecPrepared
  ( spec,
  )
where

import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Lq.Adapter -> SpecWith ByteString
spec adapter =
  describe "execPrepared" do
    it "executes a prepared statement" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.prepare "conformance_stmt" "select $1 :: text as a, $2 :: int4 as b" Nothing
        connection.execPrepared "conformance_stmt" [Just ("hello", Lq.Text), Just ("7", Lq.Text)] Lq.Text
          >>= traverse observeResult

    it "executes the unnamed prepared statement" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.prepare "" "select $1 :: int4 + 1" Nothing
        connection.execPrepared "" [Just ("41", Lq.Text)] Lq.Text
          >>= traverse observeResult

    it "executes a zero-parameter statement" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.prepare "conformance_no_params" "select 42" Nothing
        connection.execPrepared "conformance_no_params" [] Lq.Text
          >>= traverse observeResult

    it "binds null and binary parameters with a binary result" \conninfo ->
      differential adapter conninfo \connection -> do
        _ <- connection.prepare "conformance_binary" "select $1 :: text, $2 :: bytea" Nothing
        connection.execPrepared "conformance_binary" [Nothing, Just ("\NUL\1\255", Lq.Binary)] Lq.Binary
          >>= traverse observeResult

    it "rejects an unknown statement" \conninfo ->
      differential adapter conninfo \connection ->
        connection.execPrepared "conformance_missing" [] Lq.Text
          >>= traverse observeResult
