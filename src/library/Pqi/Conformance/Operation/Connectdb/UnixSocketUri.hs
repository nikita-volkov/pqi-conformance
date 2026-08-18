-- | Coverage for a @postgresql:\/\/@ URI conninfo whose host segment is a
-- percent-encoded Unix-domain socket directory (e.g. @%2ftmp%2fsome-dir@),
-- per PostgreSQL's own URI convention:
-- <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING-URIS>.
--
-- Found in @pqi-native@: @parseUri@'s host branch is never run through
-- @pctDecode@, unlike every sibling component (user, password, database,
-- query params), which all are. The candidate therefore treats the
-- still-percent-encoded string as a literal hostname to resolve via DNS,
-- instead of decoding it into the filesystem socket-directory path the URI
-- names.
--
-- This is deliberately checked via 'Pqi.host' rather than
-- 'Pqi.errorMessage': both adapters fail to connect either way (there is no
-- such socket directory), but their failure text is produced by unrelated,
-- adapter-specific machinery (DNS resolution vs. a filesystem check) and
-- isn't expected to match byte-for-byte. What must match is the host each
-- adapter parsed out of the conninfo, which is deterministic and
-- OS\/locale-independent.
module Pqi.Conformance.Operation.Connectdb.UnixSocketUri
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
    describe "a URI conninfo with a percent-encoded Unix-socket host" do
      it "decodes the host the same way the reference does" \_ ->
        differentialConnect adapter conninfo \adapter' conninfo' -> do
          connection <- Pqi.connectdb adapter' conninfo'
          observedHost <- Pqi.host connection
          Pqi.finish connection
          pure observedHost
  where
    -- Points at a socket directory that doesn't exist: this test is only
    -- about how the host segment is parsed, not about a real connection
    -- succeeding.
    conninfo = "postgresql://%2ftmp%2fpqi-conformance-nonexistent-socket-dir"
