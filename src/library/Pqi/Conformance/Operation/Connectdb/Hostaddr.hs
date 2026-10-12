-- | Coverage for a conninfo that gives both @host@ and @hostaddr@.
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
module Pqi.Conformance.Operation.Connectdb.Hostaddr
  ( spec,
  )
where

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
          withProxy conninfo' \proxiedConninfo _ -> do
            -- Later keywords win, so these override the proxy's @host@.
            connection <-
              Pqi.connectdb
                adapter'
                (proxiedConninfo <> " host=nonexistent.invalid.host hostaddr=127.0.0.1")
            observedStatus <- Pqi.status connection
            observedHost <- Pqi.host connection
            Pqi.finish connection
            pure (observedStatus, observedHost)
