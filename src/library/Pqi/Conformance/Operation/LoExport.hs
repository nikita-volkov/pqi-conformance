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
import qualified Pqi
import Pqi.Conformance.Harness
import Pqi.Conformance.Prelude
import System.Directory (removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Hspec

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "loExport" do
    it "exports an imported object, round-tripping its bytes" \conninfo ->
      differential adapter conninfo \connection -> do
        (importPath, importHandle) <- openBinaryTempFile "/tmp" "pqi-conformance-export-in"
        ByteString.hPut importHandle payload
        hClose importHandle
        (exportPath, exportHandle) <- openBinaryTempFile "/tmp" "pqi-conformance-export-out"
        hClose exportHandle
        imported <- connection.loImport importPath
        exported <- for imported \o -> connection.loExport o exportPath
        traverse_ connection.loUnlink imported
        roundTripped <- ByteString.readFile exportPath
        removeFile importPath
        removeFile exportPath
        pure (exported, roundTripped == payload)
  where
    payload = "pqi conformance payload\n" <> ByteString.pack [0 .. 255]
