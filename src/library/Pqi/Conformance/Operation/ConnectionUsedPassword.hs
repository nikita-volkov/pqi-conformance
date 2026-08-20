-- | Coverage for 'Pqi.connectionUsedPassword': whether the server actually
-- challenged for a password during authentication (mirroring libpq's
-- @password_needed@ state, not merely whether a @password=@ field was
-- present in the conninfo).
module Pqi.Conformance.Operation.ConnectionUsedPassword
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
  describe "connectionUsedPassword" do
    it "reports False under trust auth even when a password is supplied" \conninfo ->
      differential adapter (conninfo <> " password=irrelevant") Pqi.connectionUsedPassword

    it "reports True once SCRAM authentication was actually challenged for" \_ ->
      let scramConfig =
            TcPg.Config
              { TcPg.tagName = "postgres:17",
                TcPg.forwardLogs = False,
                TcPg.auth = TcPg.CredentialsAuth "scram" "secret"
              }
       in TcPg.run scramConfig \(host, port) -> do
            let conninfo = scramConninfo host port
            candidate <- bracket (Pqi.connectdb adapter conninfo) Pqi.finish Pqi.connectionUsedPassword
            reference <- bracket (Pqi.connectdb Reference.adapter conninfo) Pqi.finish Pqi.connectionUsedPassword
            candidate `shouldBe` reference

scramConninfo :: Text -> Word16 -> ByteString
scramConninfo host port =
  ByteString.Char8.pack
    ( "host="
        <> Text.unpack host
        <> " port="
        <> show port
        <> " user=scram password=secret dbname=scram"
    )
