-- | Coverage for a conninfo that gives both @host@ and @hostaddr@, in the
-- @keyword=value@ form and in the URI form (@?hostaddr=…@).
--
-- Per libpq's documentation
-- (<https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNECT-HOSTADDR>),
-- when @hostaddr@ is present the client connects to that numeric address and
-- must not resolve @host@: the name is kept only for authentication, SSL
-- certificate verification and the like. A name that no resolver can
-- translate is therefore harmless as long as @hostaddr@ is usable.
--
-- Found in @pqi-native@ (<https://github.com/nikita-volkov/pqi-native/issues/11>):
-- @hostaddr@ is not one of the keys 'Pqi.Native.Connection.parseConnInfo'
-- recognises, so it was never consulted when dialling. The candidate resolved
-- @host@ through DNS regardless, and so reported a connection failure for a
-- conninfo that @libpq@ connects with. The key was also left in the
-- parameters forwarded to the server in the startup packet, where @libpq@
-- never sends it.
--
-- The scenario points @hostaddr@ at a loopback proxy in front of the shared
-- container ('withProxy'), which makes the numeric address valid wherever the
-- container's Docker daemon runs, and gives @host@ a name reserved by
-- RFC 2606 to never resolve. The observation is the connection status
-- together with the parsed host. The status carries the divergence. The host
-- pins that the name is still what 'Pqi.host' reports, as @libpq@ does, so a
-- fix that simply replaced @host@ with @hostaddr@ would be caught.
--
-- The URI form is its own example because @pqi-native@ parses a URI's query
-- through a separate code path, where a fix to the @keyword=value@ one does
-- not reach.
module Pqi.Conformance.Operation.Connectdb.Hostaddr
  ( spec,
  )
where

import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Pqi
import Pqi.Conformance.Fixture.ConnectionLoss (withProxy)
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "connectdb" do
    describe "a conninfo with an unresolvable host and a hostaddr" do
      it "connects to the hostaddr without resolving the host, like the reference" \conninfo ->
        differentialConnect adapter conninfo \adapter' conninfo' ->
          withProxy conninfo' \proxiedConninfo _ ->
            -- Later keywords win, so these override the proxy's @host@.
            observe adapter' (proxiedConninfo <> " host=nonexistent.invalid.host hostaddr=127.0.0.1")

      it "does the same when given as a URI query parameter" \conninfo ->
        differentialConnect adapter conninfo \adapter' conninfo' ->
          withProxy conninfo' \proxiedConninfo _ ->
            observe
              adapter'
              ( "postgresql://postgres@nonexistent.invalid.host:"
                  <> proxyPort proxiedConninfo
                  <> "/postgres?hostaddr=127.0.0.1"
              )

observe :: Pqi.Adapter -> ByteString -> IO (Pqi.ConnStatus, Maybe ByteString)
observe adapter conninfo = do
  connection <- Pqi.connectdb adapter conninfo
  observedStatus <- Pqi.status connection
  observedHost <- Pqi.host connection
  Pqi.finish connection
  pure (observedStatus, observedHost)

-- | The port the proxy listens on: 'withProxy' appends it as the last
-- keyword of the conninfo it hands out.
proxyPort :: ByteString -> ByteString
proxyPort conninfo =
  case [value | token <- reverse (ByteString.Char8.words conninfo), Just value <- [ByteString.Char8.stripPrefix "port=" token]] of
    value : _ -> value
    [] -> error "proxyPort: no port in the proxied conninfo"
