-- | Coverage for 'Pqi.connectionNeedsPassword': whether the server actually
-- challenged for a password during authentication and none was available
-- (mirroring libpq's @password_needed@ state, not merely whether a
-- @password=@ field was present in the conninfo).
module Pqi.Conformance.Operation.ConnectionNeedsPassword
  ( spec,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text as Text
import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import qualified Pqi.Conformance.Reference as Reference
import Test.Hspec
import qualified TestcontainersPostgresql as TcPg

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter = do
  describe "connectionNeedsPassword" do
    it "reports False under trust auth even when a password is supplied" \conninfo ->
      differential adapter (conninfo <> " password=irrelevant") Pqi.connectionNeedsPassword

    it "reports False once SCRAM authentication succeeded with the right password" \_ ->
      let scramConfig =
            TcPg.Config
              { TcPg.tagName = "postgres:17",
                TcPg.forwardLogs = False,
                TcPg.auth = TcPg.CredentialsAuth "scram" "secret"
              }
       in TcPg.run scramConfig \(host, port) -> do
            let conninfo = scramConninfo host port "secret"
            candidate <- bracket (Pqi.connectdb adapter conninfo) Pqi.finish Pqi.connectionNeedsPassword
            reference <- bracket (Pqi.connectdb Reference.adapter conninfo) Pqi.finish Pqi.connectionNeedsPassword
            candidate `shouldBe` reference

    it "reports True when SCRAM authentication is challenged for but no password was supplied" \_ ->
      let scramConfig =
            TcPg.Config
              { TcPg.tagName = "postgres:17",
                TcPg.forwardLogs = False,
                TcPg.auth = TcPg.CredentialsAuth "scram" "secret"
              }
       in TcPg.run scramConfig \(host, port) -> do
            let conninfo = scramConninfoNoPassword host port
            candidate <- bracket (Pqi.connectdb adapter conninfo) Pqi.finish Pqi.connectionNeedsPassword
            reference <- bracket (Pqi.connectdb Reference.adapter conninfo) Pqi.finish Pqi.connectionNeedsPassword
            candidate `shouldBe` reference

scramConninfo :: Text -> Word16 -> ByteString -> ByteString
scramConninfo host port password =
  ByteString.Char8.pack
    ( "host="
        <> Text.unpack host
        <> " port="
        <> show port
        <> " user=scram password="
        <> ByteString.Char8.unpack password
        <> " dbname=scram"
    )

scramConninfoNoPassword :: Text -> Word16 -> ByteString
scramConninfoNoPassword host port =
  ByteString.Char8.pack
    ( "host="
        <> Text.unpack host
        <> " port="
        <> show port
        <> " user=scram dbname=scram"
    )
