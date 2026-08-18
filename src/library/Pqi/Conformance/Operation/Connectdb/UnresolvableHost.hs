-- | Coverage for @connectdb@ against a host name that DNS cannot resolve.
--
-- Found in @pqi-native@
-- (<https://github.com/nikita-volkov/hasql/issues/329>): the candidate's
-- 'Pqi.errorMessage' is the raw 'Show'n form of the underlying
-- 'Network.Socket.getAddrInfo' exception (@could not connect to server:
-- Network.Socket.getAddrInfo (called with preferred socket
-- type\/protocol: ...): does not exist (Name or service not known)@),
-- whereas libpq's own message for the identical failure is its own,
-- differently-worded (and locale-translated) sentence (@could not
-- translate host name "..." to address: Name or service not known@).
-- Both mention the resolver failure, but in unrelated phrasing, so a
-- downstream classifier that pattern-matches on libpq's wording (e.g.
-- @Hasql.Connection@'s networking-error patterns) reacts differently to
-- the two adapters for what is otherwise the same underlying failure.
--
-- Deliberately left failing, same as
-- 'Pqi.Conformance.Operation.Connectdb.MissingUnixSocketDirectory': the
-- mismatch it demonstrates is the point.
module Pqi.Conformance.Operation.Connectdb.UnresolvableHost
  ( spec,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "connectdb" do
    describe "a host name that DNS cannot resolve" do
      it "reports a failure message shaped like the reference's" \_ ->
        differentialConnect adapter conninfo \adapter' conninfo' -> do
          connection <- Pqi.connectdb adapter' conninfo'
          observedStatus <- Pqi.status connection
          observedError <- Pqi.errorMessage connection
          Pqi.finish connection
          pure (observedStatus, observedError)
  where
    -- A hostname reserved by RFC 2606 conventions for never resolving:
    -- this test is only about the shape of the failure, not about a real
    -- connection succeeding.
    conninfo = "host=nonexistent.invalid.host"
