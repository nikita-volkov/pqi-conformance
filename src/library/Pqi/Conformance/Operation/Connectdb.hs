-- | Coverage for 'Pqi.connectdb': a blocking connection from a conninfo
-- string, including the rejected-conninfo error paths and SCRAM-SHA-256
-- authentication.
module Pqi.Conformance.Operation.Connectdb
  ( spec,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text as Text
import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Observation
import Pqi.Conformance.Prelude
import Pqi.Conformance.Reference (Reference)
import Test.Hspec
import qualified TestcontainersPostgresql as TcPg

spec :: forall c. (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy = do
  describe "connectdb" do
    it "opens a usable connection" \conninfo ->
      differential proxy conninfo observeConnection

    it "rejects an unknown database" \conninfo ->
      differentialConnect proxy conninfo \(_ :: Proxy d) conninfo' -> do
        connection <- connectdb (conninfo' <> " dbname=pqi_no_such_db") :: IO d
        observation <- status connection
        nullness <- pure (isNullConnection connection)
        finish connection
        pure (observation, nullness)

    it "rejects an unknown user" \conninfo ->
      differentialConnect proxy conninfo \(_ :: Proxy d) conninfo' -> do
        connection <- connectdb (conninfo' <> " user=pqi_no_such_user") :: IO d
        observation <- status connection
        finish connection
        pure observation

  describe "SCRAM-SHA-256 authentication" do
    it "the candidate authenticates and queries like the FFI reference" \_ ->
      let scramConfig =
            TcPg.Config
              { TcPg.forwardLogs = False,
                TcPg.distro = TcPg.Distro17,
                TcPg.auth = TcPg.CredentialsAuth "scram" "secret"
              }

          scramScenario :: forall c. (IsConnection c) => c -> IO (Maybe ResultObservation)
          scramScenario connection = exec connection "select 1 as scram_works" >>= traverse observeResult
       in TcPg.run scramConfig \(host, port) -> do
            let conninfo =
                  ByteString.Char8.pack
                    ( "host="
                        <> Text.unpack host
                        <> " port="
                        <> show port
                        <> " user=scram password=secret dbname=scram"
                    )
            native <- bracket (connectdb conninfo) finish (scramScenario @c)
            reference <- bracket (connectdb conninfo) finish (scramScenario @Reference)
            native `shouldBe` reference
