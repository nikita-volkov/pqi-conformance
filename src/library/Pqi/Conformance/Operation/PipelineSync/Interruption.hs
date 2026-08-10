-- | Coverage for 'Pqi.pipelineSync' under asynchronous interruption: a
-- resilience gap found in @pqi-native@ while chasing a hang in @hasql@'s
-- @Integration.Sharing.Connection.Use.PipelineAbortedInterruptionCleanupSpec@:
-- an asynchronous exception (e.g. from 'System.Timeout.timeout') landing
-- while a pipelined command is being aborted, or while cleaning up
-- afterwards, can leave a connection permanently unusable instead of merely
-- failing outright.
--
-- The suspected mechanism: none of @pqi-native@'s transport or connection
-- code (@Pqi.Native.Transport@, @Pqi.Native.Connection@, @Pqi.Native.Query@)
-- uses 'Control.Exception.mask', 'Control.Exception.bracket', or any other
-- exception-safety device - all of its state (the read buffer, pipeline
-- status, pending-command counters, ...) lives in plain 'Data.IORef.IORef's
-- updated as a sequence of ordinary, unprotected steps. Its reads go through
-- @network@'s non-blocking-socket + GHC-I\/O-manager path
-- ('Network.Socket.ByteString.recv'), which is /interruptible/ - it can
-- receive an async exception mid-call even under 'Control.Exception.mask' -
-- unlike @pqi-ffi@, whose blocking @libpq@ reads go through a @safe@ FFI
-- import that defers async exceptions until the call returns. An interrupt
-- landing mid-read can therefore desynchronize @pqi-native@'s message
-- framing (bytes already pulled off the socket are dropped instead of being
-- folded back into the read buffer) or abandon its pipeline-tracking state
-- mid-transition, in a way @pqi-ffi@ structurally cannot.
--
-- This scenario mirrors the hasql-level reproduction as closely as possible
-- while staying entirely at the @Pqi@ level (no @hasql@ involved): pipeline
-- a statement that sleeps just long enough to widen the vulnerable window,
-- followed by one guaranteed to fail, sweep an async interrupt finely across
-- the moment the pipeline transitions to \"aborted\", then run the same
-- clean-up sequence @Hasql.Comms.Session.leavePipeline@ runs (sync, drain,
-- flush, drain, exit) - itself timed, since an adapter that can lose an
-- interrupt mid-read can just as easily lose one mid-clean-up - and finally
-- probe the connection with a trivial round-trip. A sound adapter always
-- either finishes promptly or leaves the connection in a state where a
-- fresh round-trip fails fast; it never goes quiet.
module Pqi.Conformance.Operation.PipelineSync.Interruption
  ( spec,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults, float8Oid)
import System.Timeout (timeout)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "resilience to an async interruption around a pipeline abort" do
    it "the connection recovers or fails fast instead of going quiet" \conninfo -> do
      reproduced <- go conninfo delays
      reproduced `shouldBe` False
  where
    go _ [] = pure False
    go conninfo (d : ds) = do
      stalled <- attempt conninfo d
      if stalled then pure True else go conninfo ds
    -- Long enough to give an interrupt landing anywhere near the sleep
    -- statement's completion (where the pipeline flips to "aborted") a
    -- realistic chance to land mid-read, short enough to keep the sweep
    -- affordable.
    sleepMicros = 20000 :: Int
    sleepSeconds = fromIntegral sleepMicros / 1000000 :: Double

    -- Sweep across the moment the sleep statement's result arrives and the
    -- failing statement's error is processed - the vulnerable window is far
    -- narrower than this timer's granularity.
    delays = [sleepMicros + step | step <- [-3000, -2900 .. 6000]] :: [Int]

    attemptsPerDelay = 15 :: Int

    -- An adapter that isn't stuck settles a trivial round-trip in
    -- milliseconds; anything still running after this long counts as a
    -- reproduction of the stall.
    cleanupTimeoutMicros = 1000000 :: Int
    probeTimeoutMicros = 1000000 :: Int

    attempt conninfo delayMicros =
      or <$> replicateM attemptsPerDelay (attemptOnce conninfo delayMicros)

    attemptOnce conninfo delayMicros =
      bracket (Pqi.connectdb adapter conninfo) Pqi.finish \connection -> do
        _ <- Pqi.enterPipelineMode connection
        _ <-
          Pqi.sendQueryParams
            connection
            "select pg_sleep($1)"
            [Just (float8Oid, showBS sleepSeconds, Pqi.Text)]
            Pqi.Text
        _ <- Pqi.sendQueryParams connection "select 1/0" [] Pqi.Text
        _ <- Pqi.pipelineSync connection

        -- Race the interrupt against draining the pipeline's results, tuned
        -- to land around the PipelineOn -> PipelineAborted transition.
        _ <- timeout delayMicros (drainResults connection)

        -- Mirror Hasql.Comms.Session.leavePipeline's clean-up sequence,
        -- itself timed: a second interrupt landing here is just as capable
        -- of desynchronizing the connection as the first one.
        cleanedUp <- timeout cleanupTimeoutMicros do
          _ <- drainResults connection
          status <- Pqi.pipelineStatus connection
          when (status /= Pqi.PipelineOff) do
            _ <- Pqi.pipelineSync connection
            _ <- drainResults connection
            _ <- Pqi.sendFlushRequest connection
            _ <- drainResults connection
            _ <- Pqi.exitPipelineMode connection
            pure ()

        probe <- timeout probeTimeoutMicros do
          _ <- Pqi.sendQuery connection "select 1"
          drainResults connection

        pure (isNothing cleanedUp || isNothing probe)

    showBS :: Double -> ByteString
    showBS = ByteString.Char8.pack . show
