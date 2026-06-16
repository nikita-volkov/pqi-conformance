-- | Coverage for 'Pqi.loExport': exporting a large object back to a file,
-- round-tripping its bytes.
--
-- @lo_import@\/@lo_export@ are self-contained and run in autocommit; they need
-- no explicit transaction block.
module Pqi.Conformance.Operation.LoExport
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
  describe "loExport" do
    it "exports an imported object, round-tripping its bytes" \conninfo ->
      differential proxy conninfo \connection -> do
        (importPath, importHandle) <- openBinaryTempFile "/tmp" "pqi-conformance-export-in"
        ByteString.hPut importHandle payload
        hClose importHandle
        (exportPath, exportHandle) <- openBinaryTempFile "/tmp" "pqi-conformance-export-out"
        hClose exportHandle
        imported <- loImport connection importPath
        exported <- for imported \o -> loExport connection o exportPath
        traverse_ (loUnlink connection) imported
        roundTripped <- ByteString.readFile exportPath
        removeFile importPath
        removeFile exportPath
        pure (exported, roundTripped == payload)
  where
    payload = "pqi conformance payload\n" <> ByteString.pack [0 .. 255]
