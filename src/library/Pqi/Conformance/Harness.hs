-- | The differential-testing harness: a throwaway PostgreSQL container and
-- the comparison combinators.
module Pqi.Conformance.Harness
  ( -- * Container
    containerHook,

    -- * Comparison
    differential,
    differentialConnect,
  )
where

import Control.Exception (bracket, bracket_)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text as Text
import Data.Unique (hashUnique, newUnique)
import qualified Pqi
import Pqi.Conformance.Prelude
import qualified Pqi.Conformance.Reference as Reference
import Test.Hspec
import qualified TestcontainersPostgresql as TcPg

-- | Boot a single trust-auth PostgreSQL container for the whole spec tree and
-- hand each example a ready conninfo string.
containerHook :: SpecWith ByteString -> Spec
containerHook = aroundAll (TcPg.run config) . aroundWith withConninfo
  where
    config =
      TcPg.Config
        { TcPg.tagName = "postgres:17",
          TcPg.forwardLogs = False,
          TcPg.auth = TcPg.TrustAuth
        }
    withConninfo action (host, port) = action (conninfo host port)
    conninfo host port =
      ByteString.Char8.pack
        ( "host="
            <> Text.unpack host
            <> " port="
            <> show port
            <> " user=postgres dbname=postgres"
        )

-- | Run a scenario on both the candidate and the FFI reference (each on its own
-- fresh connection to the same database) and assert that the two observations
-- are equal.
--
-- Each call creates a fresh database for the scenario and drops it afterwards,
-- so tests are isolated even when the container is shared.
differential ::
  (Eq a, Show a, HasCallStack) =>
  Pqi.Adapter ->
  ByteString ->
  (Pqi.Connection -> IO a) ->
  Expectation
differential adapter adminConninfo scenario =
  withTestDb adminConninfo \testConninfo -> do
    candidate <- bracket (adapter.connectdb testConninfo) (.finish) scenario
    reference <- bracket (Reference.adapter.connectdb testConninfo) (.finish) scenario
    candidate `shouldBe` reference

-- Create a uniquely named database, run the action against it, and drop it on
-- exit (including on exception). The admin conninfo must point to an existing
-- database (e.g. @dbname=postgres@); the test conninfo appended with the new
-- database name is passed to the action. In libpq keyword=value strings the
-- last occurrence of a keyword wins, so appending @dbname=…@ overrides any
-- earlier value.
withTestDb :: ByteString -> (ByteString -> IO a) -> IO a
withTestDb adminConninfo action = do
  u <- newUnique
  let dbName = ByteString.Char8.pack ("lq" <> show (abs (hashUnique u)))
  bracket_
    (adminExec adminConninfo ("create database " <> dbName))
    (adminExec adminConninfo ("drop database " <> dbName))
    (action (adminConninfo <> " dbname=" <> dbName))

adminExec :: ByteString -> ByteString -> IO ()
adminExec conninfo sql = do
  conn <- Reference.adapter.connectdb conninfo
  _ <- conn.exec sql
  conn.finish

-- | Like 'differential', but for scenarios that exercise connection
-- establishment itself ('Pqi.connectdb' on a broken conninfo,
-- 'Pqi.connectStart', 'Pqi.newNullConnection', ...): instead of an opened
-- connection the scenario receives the conninfo and the adapter to use, and
-- manages any connections it opens itself.
differentialConnect ::
  (Eq a, Show a, HasCallStack) =>
  Pqi.Adapter ->
  ByteString ->
  (Pqi.Adapter -> ByteString -> IO a) ->
  Expectation
differentialConnect adapter conninfo scenario = do
  candidate <- scenario adapter conninfo
  reference <- scenario Reference.adapter conninfo
  candidate `shouldBe` reference
