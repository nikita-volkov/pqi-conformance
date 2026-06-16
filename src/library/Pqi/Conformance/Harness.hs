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
import Pqi (IsConnection (..))
import Pqi.Conformance.Prelude
import Pqi.Conformance.Reference (Reference)
import Test.Hspec
import qualified TestcontainersPostgresql as TcPg

-- | Boot a single trust-auth PostgreSQL container for the whole spec tree and
-- hand each example a ready conninfo string.
containerHook :: SpecWith ByteString -> Spec
containerHook = aroundAll (TcPg.run config) . aroundWith withConninfo
  where
    config =
      TcPg.Config
        { TcPg.forwardLogs = False,
          TcPg.distro = TcPg.Distro17,
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
  forall c a.
  (Eq a, Show a, IsConnection c) =>
  Proxy c ->
  ByteString ->
  (forall c'. (IsConnection c') => c' -> IO a) ->
  Expectation
differential _ adminConninfo scenario =
  withTestDb adminConninfo \testConninfo -> do
    candidate <- bracket (connectdb testConninfo :: IO c) finish scenario
    reference <- bracket (connectdb testConninfo :: IO Reference) finish scenario
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
  conn <- connectdb conninfo :: IO Reference
  _ <- exec conn sql
  finish conn

-- | Like 'differential', but for scenarios that exercise connection
-- establishment itself ('Pqi.connectdb' on a broken conninfo,
-- 'Pqi.connectStart', 'Pqi.newNullConnection', ...): instead of an
-- opened connection the scenario receives the conninfo and the candidate's
-- connection type (via 'Proxy'), and manages any connections it opens itself.
differentialConnect ::
  forall c a.
  (Eq a, Show a, IsConnection c) =>
  Proxy c ->
  ByteString ->
  (forall c'. (IsConnection c') => Proxy c' -> ByteString -> IO a) ->
  Expectation
differentialConnect proxy conninfo scenario = do
  candidate <- scenario proxy conninfo
  reference <- scenario (Proxy :: Proxy Reference) conninfo
  candidate `shouldBe` reference
