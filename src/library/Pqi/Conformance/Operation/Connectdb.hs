-- | Coverage for 'Pqi.connectdb': a blocking connection from a conninfo
-- string, including the rejected-conninfo error paths and SCRAM-SHA-256
-- authentication.
module Pqi.Conformance.Operation.Connectdb
  ( spec,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import qualified Pqi.Conformance.Reference as Reference
import Test.Hspec
import qualified TestcontainersPostgresql as TcPg

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter = do
  describe "connectdb" do
    it "opens a usable connection" \conninfo ->
      differential adapter conninfo observeConnection

    it "accepts a URI-format conninfo" \conninfo ->
      differentialConnect adapter conninfo \adapter' conninfo' -> do
        connection <- adapter' . connectdb (kvToUri conninfo')
        s <- connection . status
        connection . finish
        pure s

    it "rejects an unknown database" \conninfo ->
      differentialConnect adapter conninfo \adapter' conninfo' -> do
        connection <- adapter' . connectdb (conninfo' <> " dbname=pqi_no_such_db")
        observation <- connection . status
        nullness <- pure connection . isNullConnection
        connection . finish
        pure (observation, nullness)

    it "rejects an unknown user" \conninfo ->
      differentialConnect adapter conninfo \adapter' conninfo' -> do
        connection <- adapter' . connectdb (conninfo' <> " user=pqi_no_such_user")
        observation <- connection . status
        connection . finish
        pure observation

    it "defaults the user like the reference when user is omitted" \conninfo ->
      differentialConnect adapter conninfo \adapter' conninfo' -> do
        connection <- adapter' . connectdb (dropUser conninfo')
        resolvedUser <- connection . user
        observedStatus <- connection . status
        observedError <- connection . errorMessage
        connection . finish
        pure (resolvedUser, observedStatus, observedError)

  describe "SCRAM-SHA-256 authentication" do
    it "the candidate authenticates and queries like the FFI reference" \_ ->
      let scramConfig =
            TcPg.Config
              { TcPg.tagName = "postgres:17",
                TcPg.forwardLogs = False,
                TcPg.auth = TcPg.CredentialsAuth "scram" "secret"
              }

          scramScenario :: Pqi.Connection -> IO (Maybe ResultObservation)
          scramScenario connection = connection . exec "select 1 as scram_works" >>= traverse observeResult
       in TcPg.run scramConfig \(host, port) -> do
            let conninfo =
                  ByteString.Char8.pack
                    ( "host="
                        <> Text.unpack host
                        <> " port="
                        <> show port
                        <> " user=scram password=secret dbname=scram"
                    )
            candidate <- bracket (adapter . connectdb conninfo) (. finish) scramScenario
            reference <- bracket (Reference.adapter . connectdb conninfo) (. finish) scramScenario
            candidate `shouldBe` reference

-- | Drop the @user=…@ token from a @key=value@ conninfo, leaving the user
-- unspecified so the adapter must apply its own default. libpq derives the
-- default from the operating-system login name; an adapter that hardcodes a
-- different default (e.g. @postgres@) diverges here.
dropUser :: ByteString -> ByteString
dropUser raw =
  ByteString.Char8.unwords
    (filter (not . ("user=" `ByteString.isPrefixOf`)) (ByteString.Char8.words raw))

-- | Convert a @key=value@ conninfo to a @postgresql://@ URI, for testing
-- that adapters accept URI-format connection strings.
kvToUri :: ByteString -> ByteString
kvToUri raw =
  "postgresql://"
    <> user
    <> (if ByteString.null password then "" else ":" <> password)
    <> (if ByteString.null user && ByteString.null password then "" else "@")
    <> host
    <> (if ByteString.null port then "" else ":" <> port)
    <> (if ByteString.null dbname then "" else "/" <> dbname)
  where
    pairs = Map.fromList $ mapMaybe toPair (ByteString.Char8.words raw)
    get k = Map.findWithDefault "" k pairs
    toPair token = case ByteString.Char8.break (== '=') token of
      (k, v) | not (ByteString.null v) -> Just (k, ByteString.drop 1 v)
      _ -> Nothing
    host = get "host"
    port = get "port"
    user = get "user"
    password = get "password"
    dbname = get "dbname"
