-- | Reusable scenario fragments shared by the per-operation spec modules.
--
-- These are the recurring building blocks — running a query and observing its
-- result, draining the asynchronous result stream, collecting a @COPY OUT@
-- stream, driving a polling loop to its terminal status, and the handful of
-- well-known type OIDs — factored out so each operation module stays focused on
-- the one operation it covers.
module Pqi.Conformance.Scenario
  ( -- * Running queries
    execScenario,
    execAllScenario,
    observed,

    -- * Asynchronous result collection
    drainResults,
    takeResult,
    takeCommandResults,

    -- * Copy and polling loops
    collectCopyOut,
    pollUntilDone,
    flushUntilDone,

    -- * Transactions
    inTransaction,

    -- * Well-known type OIDs
    boolOid,
    byteaOid,
    int2Oid,
    int4Oid,
    int8Oid,
    textOid,
    float8Oid,
  )
where

import Pqi
  ( CopyOutResult (..),
    FlushStatus (..),
    IsConnection (..),
    PollingStatus (..),
  )
import qualified Pqi as Lq
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude

-- | Run 'Pqi.exec' and observe its result (if any).
execScenario :: (IsConnection c) => ByteString -> c -> IO (Maybe ResultObservation)
execScenario sql connection = exec connection sql >>= traverse observeResult

-- | Run a sequence of statements with 'Pqi.exec', observing every result.
execAllScenario :: (IsConnection c) => [ByteString] -> c -> IO [Maybe ResultObservation]
execAllScenario sqls connection = traverse (`execScenario` connection) sqls

-- | Run 'Pqi.execParams' and observe its result (if any).
observed ::
  (IsConnection c) =>
  ByteString ->
  [Maybe (Word32, ByteString, Lq.Format)] ->
  Lq.Format ->
  c ->
  IO (Maybe ResultObservation)
observed sql params resultFormat connection =
  execParams connection sql params resultFormat >>= traverse observeResult

-- | Collect and observe results with 'Pqi.getResult' until it reports
-- completion with 'Nothing'.
drainResults :: (IsConnection c) => c -> IO [ResultObservation]
drainResults connection = go []
  where
    go acc =
      getResult connection >>= \case
        Nothing -> pure (reverse acc)
        Just result -> do
          observation <- observeResult result
          go (observation : acc)

-- | One 'Pqi.getResult' step, observed.
takeResult :: (IsConnection c) => c -> IO (Maybe ResultObservation)
takeResult connection = getResult connection >>= traverse observeResult

-- | The results of one pipelined command: its result and the 'Nothing'
-- separator that ends it.
takeCommandResults ::
  (IsConnection c) => c -> IO (Maybe ResultObservation, Maybe ResultObservation)
takeCommandResults connection = do
  result <- takeResult connection
  separator <- takeResult connection
  pure (result, separator)

-- | Collect blocking 'Pqi.getCopyData' outcomes until the stream reports
-- anything other than a row (normally 'CopyOutDone'), including that final
-- outcome.
collectCopyOut :: (IsConnection c) => c -> IO [CopyOutResult]
collectCopyOut connection = go (1000 :: Int) []
  where
    go 0 acc = pure (reverse acc)
    go n acc =
      getCopyData connection False >>= \case
        CopyOutRow row -> go (n - 1) (CopyOutRow row : acc)
        terminal -> pure (reverse (terminal : acc))

-- | Drive a polling loop to its terminal status, spinning with a small delay
-- instead of waiting on the socket. Bails out as failed after ten seconds.
pollUntilDone :: IO PollingStatus -> IO PollingStatus
pollUntilDone poll = go (10000 :: Int)
  where
    go 0 = pure PollingFailed
    go n =
      poll >>= \case
        PollingReading -> threadDelay 1000 >> go (n - 1)
        PollingWriting -> threadDelay 1000 >> go (n - 1)
        terminal -> pure terminal

-- | Drive 'Pqi.flush' to a terminal status, spinning while it reports
-- 'FlushWriting'. Bails out as failed after ten seconds.
flushUntilDone :: IO FlushStatus -> IO FlushStatus
flushUntilDone doFlush = go (10000 :: Int)
  where
    go 0 = pure FlushFailed
    go n =
      doFlush >>= \case
        FlushWriting -> threadDelay 1000 >> go (n - 1)
        terminal -> pure terminal

-- | Run an action between @begin@ and @commit@. Large-object operations must
-- run inside a transaction block.
inTransaction :: (IsConnection c) => c -> IO a -> IO a
inTransaction connection action = do
  _ <- exec connection "begin"
  result <- action
  _ <- exec connection "commit"
  pure result

boolOid :: Word32
boolOid = 16

byteaOid :: Word32
byteaOid = 17

int2Oid :: Word32
int2Oid = 21

int4Oid :: Word32
int4Oid = 23

int8Oid :: Word32
int8Oid = 20

textOid :: Word32
textOid = 25

float8Oid :: Word32
float8Oid = 701
