-- | The scenario shared by the @LostConnection@ specs of the operations that
-- run a command: the victim connects through a proxy
-- ("Pqi.Conformance.Fixture.ConnectionLoss"), the connection is killed in
-- one of three ways, and the victim runs its next command and then keeps
-- using the same handle.
--
-- The observation is the /transcript/ of everything the victim saw: the
-- return value of every call, the connection's @status@ and @errorMessage@
-- after it, and whether the call threw or hung. The scenario asserts nothing
-- about @libpq@'s values itself; the reference produces them and the
-- candidate is compared against them.
--
-- Every call of the victim is /bounded/: it runs under
-- 'System.Timeout.timeout' and under 'Control.Exception.try', so that a
-- candidate which throws or hangs fails the comparison with a transcript
-- that says so (@THREW …@, @TIMED OUT@), rather than killing the run - or
-- taking the whole suite down with it, as a @finish@ that never returns
-- does. The same applies to the reference, so that the comparison is of like
-- with like; a reference that throws would be a broken fixture.
module Pqi.Conformance.Scenario.LostConnection
  ( -- * Specs
    spec,
    Operation,
    asynchronous,
    synchronous,
    Collection (..),
    withPreparedStatement,
  )
where

import Control.Exception (SomeException, try)
import Data.List (intercalate)
import GHC.Conc (threadWaitRead)
import qualified Pqi
import Pqi.Conformance.Fixture.ConnectionLoss
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation (ResultObservation (..), observeResult)
import Pqi.Conformance.Prelude
import System.Timeout (timeout)
import Test.Hspec

-- | The ways in which the connection is killed.
data Death
  = -- | A graceful close (FIN) with no protocol message.
    SilentClose
  | -- | @pg_terminate_backend@ from a second connection: the server announces
    -- it with a @FATAL@ and then closes.
    ServerAnnouncedTermination
  | -- | The peer answers with an @RST@.
    Reset
  deriving stock (Eq, Show, Enum, Bounded)

deathTitle :: Death -> String
deathTitle = \case
  SilentClose -> "after a silent close"
  ServerAnnouncedTermination -> "after a server-announced termination"
  Reset -> "after a reset"

-- | Kill the connection, given the harness conninfo, the proxy and the
-- victim's backend pid, and return once the victim's own socket has noticed.
--
-- The proxy only knows that it has closed or reset its side. Whether the
-- victim's kernel has /processed/ the resulting @FIN@, @RST@ or @FATAL@ is
-- another matter, and it matters: a victim that writes before the @RST@ has
-- arrived has its write accepted and fails on its next read, whereas after
-- it the write itself fails. @libpq@ reports the same either way, so the
-- reference does not show the difference, but an adapter that reports the
-- two paths differently flips between two outcomes from run to run. Waiting
-- for the victim's socket to turn readable - which a pending @FIN@, @RST@ or
-- unread bytes all make it, without consuming any of them - removes that
-- race.
kill :: Death -> ByteString -> Proxy -> Pqi.Connection -> Int32 -> IO ()
kill death conninfo proxy victim pid = do
  case death of
    SilentClose -> sever proxy
    ServerAnnouncedTermination -> terminateBackend conninfo pid >> serverClosed proxy
    Reset -> reset proxy
  void (try @SomeException (Pqi.socket victim >>= traverse_ awaitReadable))
  where
    awaitReadable fd = timeout callDeadlineMicros (threadWaitRead fd)

-- | What the victim does after the connection has been killed.
data Operation = Operation
  { -- | Runs before the connection is killed, so that the command that
    -- follows has something to rely on. Its outcome is part of the
    -- transcript, to show that the preparation really happened.
    setup :: Pqi.Connection -> IO [String],
    -- | The command, together with the observation of its outcome.
    command :: Pqi.Connection -> IO [String]
  }

-- | A command that is sent, and whose results are then collected with
-- 'Pqi.getResult'. The sends are all run and their returned flags
-- recorded, in order.
asynchronous :: Collection -> (Pqi.Connection -> IO [Bool]) -> Operation
asynchronous collection send =
  Operation
    { setup = \_ -> pure [],
      command = \connection -> do
        sent <- bounded (send connection)
        collected <- collect collection connection
        pure (("send: " <> renderCall show sent) : collected)
    }

-- | A command that returns its result synchronously: no 'Pqi.getResult'
-- loop.
synchronous :: (Pqi.Connection -> IO (Maybe Pqi.Result)) -> Operation
synchronous run =
  Operation
    { setup = \_ -> pure [],
      command = \connection -> do
        returned <- bounded (run connection >>= traverse observeResult)
        status <- bounded (Pqi.status connection)
        message <- bounded (Pqi.errorMessage connection)
        pure
          [ "returned: " <> renderCall renderMaybeResult returned,
            "status: " <> renderCall show status,
            "errorMessage: " <> renderCall show message
          ]
    }

-- | Prepare a statement before the connection is killed, so that the command
-- can use the cached statement, as a prepared-statement cache does.
withPreparedStatement :: ByteString -> ByteString -> Operation -> Operation
withPreparedStatement name sql operation =
  operation
    { setup = \connection -> do
        prepared <- Pqi.prepare connection name sql Nothing >>= traverse observeResult
        rest <- setup operation connection
        pure (("prepared: " <> renderMaybeResult prepared) : rest)
    }

-- | How the results of an asynchronous command are collected.
data Collection
  = -- | Until the first 'Nothing': the end of a command's results.
    UntilNothing
  | -- | Through the pipeline: until the sync result, or until the stream
    -- reports 'Nothing' twice in a row, since in pipeline mode a 'Nothing'
    -- separates every command's results from the next one's.
    UntilPipelineSync

-- | The most results that are collected. A connection that is lost yields
-- one or two; a candidate that fails to notice would otherwise loop.
resultCap :: Int
resultCap = 10

-- | How long any single call of the victim is allowed to take.
callDeadlineMicros :: Int
callDeadlineMicros = 5_000_000

-- | The outcome of one bounded call.
data Call a
  = Returned a
  | Threw String
  | TimedOut

bounded :: IO a -> IO (Call a)
bounded action =
  try @SomeException (timeout callDeadlineMicros action) <&> \case
    Left exception -> Threw (show exception)
    Right Nothing -> TimedOut
    Right (Just value) -> Returned value
  where
    (<&>) = flip fmap

renderCall :: (a -> String) -> Call a -> String
renderCall render = \case
  Returned value -> render value
  Threw exception -> "THREW " <> exception
  TimedOut -> "TIMED OUT after " <> show callDeadlineMicros <> "us"

renderMaybeResult :: Maybe ResultObservation -> String
renderMaybeResult = \case
  Nothing -> "Nothing"
  Just observation ->
    -- Only the error fields that are set: all of them are always observed,
    -- and nearly all are absent.
    "Just " <> show observation {errorFields = filter (isJust . snd) (errorFields observation)}

-- | Collect results and observe each of them, together with the
-- connection's @status@ and @errorMessage@ right after the call.
collect :: Collection -> Pqi.Connection -> IO [String]
collect collection connection = go 1 False
  where
    go :: Int -> Bool -> IO [String]
    go number previousWasNothing
      | number > resultCap = pure ["getResult: not finished after " <> show resultCap <> " calls"]
      | otherwise = do
          returned <- bounded (Pqi.getResult connection >>= traverse observeResult)
          status <- bounded (Pqi.status connection)
          message <- bounded (Pqi.errorMessage connection)
          let observation =
                [ "getResult #" <> show number <> ": " <> renderCall renderMaybeResult returned,
                  "  status: " <> renderCall show status,
                  "  errorMessage: " <> renderCall show message
                ]
              finished = case (collection, returned) of
                (_, Threw _) -> True
                (_, TimedOut) -> True
                (UntilNothing, Returned Nothing) -> True
                (UntilPipelineSync, Returned Nothing) -> previousWasNothing
                (UntilPipelineSync, Returned (Just ResultObservation {status = Pqi.PipelineSync})) -> True
                _ -> False
          if finished
            then pure observation
            else (observation <>) <$> go (number + 1) (isNothingResult returned)
    isNothingResult = \case
      Returned Nothing -> True
      _ -> False

-- | The same handle, used again: a new command, then what the connection
-- says about itself.
reuse :: Pqi.Connection -> IO [String]
reuse connection = do
  sent <- bounded (Pqi.sendQuery connection "select 1")
  message <- bounded (Pqi.errorMessage connection)
  result <- bounded (Pqi.getResult connection >>= traverse observeResult)
  status <- bounded (Pqi.status connection)
  pure
    [ "afterwards, sendQuery: " <> renderCall show sent,
      "afterwards, errorMessage: " <> renderCall show message,
      "afterwards, getResult: " <> renderCall renderMaybeResult result,
      "afterwards, status: " <> renderCall show status
    ]

-- | What the victim saw, one line per observation.
--
-- 'Show' renders the lines as they are, so that a failed comparison is
-- diffed line by line.
newtype Transcript = Transcript [String]
  deriving newtype (Eq)

instance Show Transcript where
  show (Transcript observations) = intercalate "\n" observations

scenario :: Death -> Operation -> Pqi.Adapter -> ByteString -> IO Transcript
scenario death operation adapter conninfo =
  withProxy conninfo \proxiedConninfo proxy -> do
    victim <- Pqi.connectdb adapter proxiedConninfo
    connected <- bounded (Pqi.status victim)
    prepared <- bounded (setup operation victim)
    pid <- Pqi.backendPID victim
    kill death conninfo proxy victim pid
    during <- command operation victim
    afterwards <- reuse victim
    finished <- bounded (Pqi.finish victim)
    pure
      ( Transcript
          ( concat
              [ ["connected: " <> renderCall show connected],
                case prepared of
                  Returned observations -> observations
                  Threw exception -> ["setup: THREW " <> exception]
                  TimedOut -> ["setup: TIMED OUT after " <> show callDeadlineMicros <> "us"],
                during,
                afterwards,
                ["finish: " <> renderCall show finished]
              ]
          )
      )

-- | The matrix: every way of dying, with every given operation as the
-- victim's next command.
spec :: Pqi.Adapter -> [(String, Operation)] -> SpecWith ByteString
spec adapter operations =
  describe "lost connection" do
    for_ [minBound .. maxBound] \death ->
      describe (deathTitle death) do
        for_ operations \(title, operation) ->
          it title \conninfo ->
            differentialConnect adapter conninfo (scenario death operation)
