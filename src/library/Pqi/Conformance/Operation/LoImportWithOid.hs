-- | Coverage for 'Pqi.loImportWithOid': importing a file as a new large
-- object with an explicitly requested OID.
--
-- Each run removes the object it creates, so the explicit OID is free for the
-- reference run and can be compared in full. @lo_import@ is self-contained and
-- runs in autocommit; it needs no explicit transaction block.
module Pqi.Conformance.Operation.LoImportWithOid
  ( spec,
  )
where

import qualified Data.ByteString as ByteString
import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import System.Directory (removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "loImportWithOid" do
    it "imports a file as a large object with an explicit OID" \conninfo ->
      differential adapter conninfo \connection -> do
        (path, handle) <- openBinaryTempFile "/tmp" "pqi-conformance-import-oid"
        ByteString.hPut handle "pqi conformance payload"
        hClose handle
        _ <- connection.loUnlink explicitOid
        imported <- connection.loImportWithOid path explicitOid
        unlinked <- for imported connection.loUnlink
        removeFile path
        pure (imported, unlinked)
  where
    explicitOid = 424243
