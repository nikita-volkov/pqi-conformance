-- | Coverage for @connectdb@ against a Unix-socket directory that doesn't
-- exist on disk (as opposed to one that exists but has nothing listening).
--
-- Both adapters fail at the very same syscall here - a @connect(2)@ on a
-- Unix-domain socket path whose containing directory is absent, i.e.
-- @ENOENT@ - so unlike
-- 'Pqi.Conformance.Operation.Connectdb.UnixSocketUri' (whose two adapters
-- fail via genuinely unrelated machinery: DNS resolution vs. a filesystem
-- check), this spec does compare 'Pqi.errorMessage' exactly. It's
-- expected to fail on @pqi-native@ right now, and deliberately left that
-- way: the mismatch it demonstrates is exactly what confuses downstream
-- string-matching classifiers.
--
-- Found in @pqi-native@
-- (<https://github.com/nikita-volkov/hasql/issues/329>): the candidate's
-- 'Pqi.errorMessage' is the raw 'Show'n form of the underlying
-- 'System.IO.Error.IOException' (@could not connect to server:
-- Network.Socket.connect: ...@), which happens to contain the substring
-- @"could not connect to server"@ - one of @Hasql.Connection@'s
-- /networking/ (transient) patterns. libpq's own message for the identical
-- failure (@connection to server on socket "..." failed: No such file or
-- directory@) does not contain that phrase, so it falls through to
-- @OtherConnectionError@ (permanent misconfiguration) instead. The two
-- adapters therefore drive @Hasql.Connection.acquire@ to opposite
-- classifications of the same failure.
module Pqi.Conformance.Operation.Connectdb.MissingUnixSocketDirectory
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
    describe "a Unix-socket directory that doesn't exist" do
      it "reports a failure message shaped like the reference's" \_ ->
        differentialConnect adapter conninfo \adapter' conninfo' -> do
          connection <- Pqi.connectdb adapter' conninfo'
          observedStatus <- Pqi.status connection
          observedError <- Pqi.errorMessage connection
          Pqi.finish connection
          pure (observedStatus, observedError)
  where
    -- A directory that is guaranteed not to exist: this test is only about
    -- the shape of the failure, not about a real connection succeeding.
    conninfo = "host=/tmp/pqi-conformance-nonexistent-socket-dir"
