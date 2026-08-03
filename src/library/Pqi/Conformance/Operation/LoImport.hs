-- | Coverage for 'Pqi.loImport': importing a file as a new large object.
--
-- The server-assigned OID differs between runs, so only its presence is
-- compared. @lo_import@ is self-contained and runs in autocommit; it needs no
-- explicit transaction block.
module Pqi.Conformance.Operation.LoImport
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
  describe "loImport" do
    it "imports a file as a large object" \conninfo ->
      differential adapter conninfo \connection -> do
        (path, handle) <- openBinaryTempFile "/tmp" "pqi-conformance-import"
        ByteString.hPut handle "pqi conformance payload"
        hClose handle
        imported <- Pqi.loImport connection path
        traverse_ (Pqi.loUnlink connection) imported
        removeFile path
        pure (isJust imported)
