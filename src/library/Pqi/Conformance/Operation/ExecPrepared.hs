-- | Coverage for 'Pqi.execPrepared': executing a prepared statement over
-- named and unnamed statements, parameter formats, and the unknown-statement
-- error path.
module Pqi.Conformance.Operation.ExecPrepared
  ( spec,
  )
where

import Pqi (IsConnection (..))
import qualified Pqi as Lq
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "execPrepared" do
    it "executes a prepared statement" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "conformance_stmt" "select $1 :: text as a, $2 :: int4 as b" Nothing
        execPrepared connection "conformance_stmt" [Just ("hello", Lq.Text), Just ("7", Lq.Text)] Lq.Text
          >>= traverse observeResult

    it "executes the unnamed prepared statement" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "" "select $1 :: int4 + 1" Nothing
        execPrepared connection "" [Just ("41", Lq.Text)] Lq.Text
          >>= traverse observeResult

    it "executes a zero-parameter statement" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "conformance_no_params" "select 42" Nothing
        execPrepared connection "conformance_no_params" [] Lq.Text
          >>= traverse observeResult

    it "binds null and binary parameters with a binary result" \conninfo ->
      differential proxy conninfo \connection -> do
        _ <- prepare connection "conformance_binary" "select $1 :: text, $2 :: bytea" Nothing
        execPrepared connection "conformance_binary" [Nothing, Just ("\NUL\1\255", Lq.Binary)] Lq.Binary
          >>= traverse observeResult

    it "rejects an unknown statement" \conninfo ->
      differential proxy conninfo \connection ->
        execPrepared connection "conformance_missing" [] Lq.Text
          >>= traverse observeResult
