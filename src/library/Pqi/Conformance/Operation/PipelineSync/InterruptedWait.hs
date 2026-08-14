-- | Coverage for an async interruption landing while the only thing still
-- outstanding on a pipeline is its sync's @ReadyForQuery@: the interrupt must
-- not swallow that message.
--
-- This is the sibling of
-- 'Pqi.Conformance.Operation.PipelineSync.Interruption', which sweeps a timer
-- finely across the moment a pipeline aborts and checks that the connection
-- does not go quiet. That sweep races the vulnerable window instead of
-- constructing it, and so passes against an adapter that still wedges its
-- callers in the field. This spec constructs the window.
--
-- The hazard is not the interrupt landing at some unlucky microsecond. It is
-- an adapter that defers async exceptions across the /wait/ for a frame rather
-- than only across the handful of instructions that move the frame's bytes
-- into its buffer. Defer across the wait and the delivery point stops being
-- arbitrary: it is pinned to the instant the frame finally arrives, which is
-- the one moment at which the adapter holds a fully-read message it has not
-- yet accounted for. The exception fires there and the message is dropped.
--
-- Which message gets dropped decides whether that is survivable. A frame from
-- the middle of a command costs the caller a result, and the next drain
-- recovers. The sync's @ReadyForQuery@ costs the caller the connection: the
-- adapter still has a sync outstanding, so the next 'Pqi.getResult' goes back
-- to the socket for a message the backend has already sent and will not send
-- again. Nothing else is coming, so the wait never ends - and an adapter that
-- masks its waits uninterruptibly cannot even be timed out of it.
--
-- Two moves construct that. First, an explicit flush request separates the
-- command's results from the sync's @ReadyForQuery@, so that by the time the
-- sync is sent the connection has nothing left to collect but that one frame.
-- Second, a deferred constraint trigger holds the @ReadyForQuery@ back: a
-- @DEFERRABLE INITIALLY DEFERRED@ trigger fires when the implicit transaction
-- commits, which in the extended protocol is the backend's handling of @Sync@
-- itself, so sleeping inside it delays the @ReadyForQuery@ and nothing else.
--
-- The timing is therefore deliberately coarse. The trigger sleeps for half a
-- second and the interrupt lands after a twentieth of one. There is no window
-- to hit: for the whole of the intervening 450ms the adapter is blocked with
-- an exception already thrown at it, and what this spec observes is what it
-- has finished doing by the time it lets that exception through.
--
-- The assertion is only that the connection settles, not that the sync result
-- survives. Both are legal outcomes and the two conforming adapters differ:
-- the reference defers the interrupt across its wait too - @libpq@'s blocking
-- read is a @safe@ FFI call - collects the @ReadyForQuery@ and loses the
-- result to the interrupt, so its recovery drain yields nothing. A fixed
-- @pqi-native@ honours the interrupt during the wait, having consumed
-- nothing, so its recovery drain still yields the sync result. What no
-- adapter may do is go quiet.
--
-- Every wait here is bounded from outside the thread doing it rather than
-- with 'System.Timeout.timeout', because an adapter that has deferred
-- delivery across its wait cannot be timed out at all. That is why this class
-- of failure presents as a hung test suite rather than a failing one, and why
-- a spec that can provoke it has to be bounded from the outside to report it.
--
-- Found via a hang in @hasql@'s
-- @Integration.Sharing.Connection.Use.PipelineAbortedInterruptionCleanup@.
-- @pqi-native@ 1.0.1.2 fails this spec, 1.0.1.3 passes it.
module Pqi.Conformance.Operation.PipelineSync.InterruptedWait
  ( spec,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, displayException, try)
import qualified Pqi
import Pqi.Conformance.Prelude
import Pqi.Conformance.Scenario (drainResults)
import System.Timeout (timeout)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "an async interruption while the sync's ReadyForQuery is outstanding" do
    it "leaves the connection able to finish the pipeline" \conninfo ->
      bracket (Pqi.connectdb adapter conninfo) Pqi.finish \connection -> do
        setUp connection

        _ <- Pqi.enterPipelineMode connection
        _ <- Pqi.sendQueryParams connection "insert into interrupted_wait values (1)" [] Pqi.Text
        -- Without this the insert's results would sit in the backend's output
        -- buffer until the sync flushed them, and would then arrive in the
        -- same burst as the ReadyForQuery - putting the interrupt on a frame
        -- from the middle of the command rather than on the one that matters.
        _ <- Pqi.sendFlushRequest connection

        -- Collect the insert's result and the separator that closes it, so
        -- that the sync's ReadyForQuery is all the connection has left to
        -- wait for. Bounded, because an adapter whose flush request does not
        -- reach the backend would block here until the sync it has not been
        -- given yet.
        collected <-
          settleWithin collectionBudgetMicros do
            command <- Pqi.getResult connection
            separator <- Pqi.getResult connection
            pure (isJust command, isNothing separator)
        collected `shouldBe` Settled (True, True)

        -- The trigger's sleep now stands between the sync and its
        -- ReadyForQuery, and the interrupt lands a twentieth of the way into
        -- it. Whether it is honoured then or only once the frame arrives is
        -- the adapter's business; what it must not do is take the frame with
        -- it on the way out.
        _ <- Pqi.pipelineSync connection
        _ <- timeout interruptAfterMicros (Pqi.getResult connection)

        -- The backend has sent everything it was going to. An adapter that
        -- accounted for the ReadyForQuery as it read it settles here; one
        -- that dropped it is waiting for it a second time, and will wait
        -- forever.
        recovered <- settleWithin recoveryBudgetMicros (void (drainResults connection))
        recovered `shouldBe` Settled ()
  where
    interruptAfterMicros = 50_000 :: Int
    collectionBudgetMicros = 5_000_000 :: Int
    recoveryBudgetMicros = 5_000_000 :: Int

-- | Create the table the pipeline writes to and the deferred trigger that
-- holds its sync's @ReadyForQuery@ back.
--
-- Everything is temporary, so it lives and dies with this connection and
-- needs no clean-up and no database of its own. The sleep is what widens the
-- gap between the sync and its @ReadyForQuery@ from microseconds to half a
-- second.
setUp :: Pqi.Connection -> IO ()
setUp connection = do
  result <- Pqi.exec connection sql
  status <- traverse Pqi.resultStatus result
  status `shouldBe` Just Pqi.CommandOk
  where
    sql =
      "create temp table interrupted_wait (x int);\
      \create function pg_temp.interrupted_wait_delay() returns trigger language plpgsql as $$ begin perform pg_sleep(0.5); return null; end $$;\
      \create constraint trigger interrupted_wait_delay \
      \  after insert on interrupted_wait \
      \  deferrable initially deferred \
      \  for each row execute function pg_temp.interrupted_wait_delay();"

-- | What became of an action given a deadline: it returned, it threw, or it
-- was still running when the deadline passed.
data Settlement a
  = Settled a
  | Threw String
  | WentQuiet
  deriving stock (Eq, Show)

-- | Run @action@ on its own thread and report how it settled within @budget@
-- microseconds.
--
-- Deliberately not 'System.Timeout.timeout'. @timeout@ bounds an action by
-- throwing an async exception into the thread running it, which does nothing
-- for a thread that has masked async exceptions uninterruptibly - precisely
-- the case this spec exists to detect. Waiting on an
-- 'Control.Concurrent.MVar.MVar' the worker fills moves the bound out of the
-- worker and into an observer that stays responsive no matter what the worker
-- is doing.
settleWithin :: Int -> IO a -> IO (Settlement a)
settleWithin budget action = do
  settled <- newEmptyMVar
  _ <- forkIO (try action >>= putMVar settled)
  outcome <- timeout budget (takeMVar settled)
  pure case outcome of
    Nothing -> WentQuiet
    Just (Left exception) -> Threw (displayException (exception :: SomeException))
    Just (Right value) -> Settled value
