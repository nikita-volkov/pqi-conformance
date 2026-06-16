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
import Pqi (IsConnection (..))
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import System.Directory (removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Hspec

spec :: (IsConnection c) => Proxy c -> SpecWith ByteString
spec proxy =
  describe "loImport" do
    it "imports a file as a large object" \conninfo ->
      differential proxy conninfo \connection -> do
        (path, handle) <- openBinaryTempFile "/tmp" "pqi-conformance-import"
        ByteString.hPut handle "pqi conformance payload"
        hClose handle
        imported <- loImport connection path
        traverse_ (loUnlink connection) imported
        removeFile path
        pure (isJust imported)
