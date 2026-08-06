-- | The reference adapter: a direct @postgresql-libpq@ wrapper used as the
-- ground truth in differential tests. It is intentionally independent of the
-- @pqi-ffi@ package (despite producing byte-identical output) so that
-- adapter test suites can depend on @pqi-conformance@ without a circular
-- dependency through @pqi-ffi@.
module Pqi.Conformance.Reference
  ( adapter,
  )
where

import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified GHC.Exts
import qualified GHC.IO
import qualified Pqi
import Pqi.Conformance.Prelude

-- | The reference adapter.
adapter :: Pqi.Adapter
adapter =
  Pqi.Adapter
    { Pqi.name = "postgresql-libpq (reference)",
      Pqi.connectdb = \conninfo -> mkConnection <$> LibPQ.connectdb conninfo,
      Pqi.connectStart = \conninfo -> mkConnection <$> LibPQ.connectStart conninfo,
      Pqi.newNullConnection = mkConnection <$> LibPQ.newNullConnection,
      Pqi.unescapeBytea = \input -> LibPQ.unescapeBytea input,
      Pqi.resStatus = \execStatus -> LibPQ.resStatus (toExecStatus execStatus)
    }

-- | Build a 'Pqi.Connection' whose fields close over the given
-- @postgresql-libpq@ connection handle.
mkConnection :: LibPQ.Connection -> Pqi.Connection
mkConnection c =
  Pqi.Connection
    { Pqi.connectPoll = fromPollingStatus <$> LibPQ.connectPoll c,
      Pqi.isNullConnection = LibPQ.isNullConnection c,
      Pqi.finish = LibPQ.finish c,
      Pqi.reset = LibPQ.reset c,
      Pqi.resetStart = LibPQ.resetStart c,
      Pqi.resetPoll = fromPollingStatus <$> LibPQ.resetPoll c,
      Pqi.db = LibPQ.db c,
      Pqi.user = LibPQ.user c,
      Pqi.pass = LibPQ.pass c,
      Pqi.host = LibPQ.host c,
      Pqi.port = LibPQ.port c,
      Pqi.options = LibPQ.options c,
      Pqi.status = fromConnStatus <$> LibPQ.status c,
      Pqi.transactionStatus = fromTransactionStatus <$> LibPQ.transactionStatus c,
      Pqi.parameterStatus = \name -> LibPQ.parameterStatus c name,
      Pqi.protocolVersion = LibPQ.protocolVersion c,
      Pqi.serverVersion = LibPQ.serverVersion c,
      Pqi.errorMessage = LibPQ.errorMessage c,
      Pqi.socket = LibPQ.socket c,
      Pqi.backendPID = fromIntegral <$> LibPQ.backendPID c,
      Pqi.connectionNeedsPassword = LibPQ.connectionNeedsPassword c,
      Pqi.connectionUsedPassword = LibPQ.connectionUsedPassword c,
      Pqi.exec = \sql -> fmap mkResult <$> LibPQ.exec c sql,
      Pqi.execParams = \sql params resultFormat ->
        fmap mkResult <$> LibPQ.execParams c sql (fmap (fmap toParam) params) (toFormat resultFormat),
      Pqi.prepare = \name sql paramTypes ->
        fmap mkResult <$> LibPQ.prepare c name sql (fmap (fmap toOid) paramTypes),
      Pqi.execPrepared = \name params resultFormat ->
        fmap mkResult <$> LibPQ.execPrepared c name (fmap (fmap toBoundParam) params) (toFormat resultFormat),
      Pqi.describePrepared = \name -> fmap mkResult <$> LibPQ.describePrepared c name,
      Pqi.describePortal = \name -> fmap mkResult <$> LibPQ.describePortal c name,
      Pqi.escapeStringConn = \s -> LibPQ.escapeStringConn c s,
      Pqi.escapeByteaConn = \s -> LibPQ.escapeByteaConn c s,
      Pqi.escapeIdentifier = \s -> LibPQ.escapeIdentifier c s,
      Pqi.sendQuery = \sql -> LibPQ.sendQuery c sql,
      Pqi.sendQueryParams = \sql params resultFormat ->
        LibPQ.sendQueryParams c sql (fmap (fmap toParam) params) (toFormat resultFormat),
      Pqi.sendPrepare = \name sql paramTypes ->
        LibPQ.sendPrepare c name sql (fmap (fmap toOid) paramTypes),
      Pqi.sendQueryPrepared = \name params resultFormat ->
        LibPQ.sendQueryPrepared c name (fmap (fmap toBoundParam) params) (toFormat resultFormat),
      Pqi.sendDescribePrepared = \name -> LibPQ.sendDescribePrepared c name,
      Pqi.sendDescribePortal = \name -> LibPQ.sendDescribePortal c name,
      Pqi.getResult = fmap mkResult <$> LibPQ.getResult c,
      Pqi.consumeInput = LibPQ.consumeInput c,
      Pqi.isBusy = LibPQ.isBusy c,
      Pqi.setnonblocking = \nonBlocking -> LibPQ.setnonblocking c nonBlocking,
      Pqi.isnonblocking = LibPQ.isnonblocking c,
      Pqi.setSingleRowMode = LibPQ.setSingleRowMode c,
      Pqi.flush = fromFlushStatus <$> LibPQ.flush c,
      Pqi.pipelineStatus = fromPipelineStatus <$> LibPQ.pipelineStatus c,
      Pqi.enterPipelineMode = LibPQ.enterPipelineMode c,
      Pqi.exitPipelineMode = LibPQ.exitPipelineMode c,
      Pqi.pipelineSync = LibPQ.pipelineSync c,
      Pqi.sendFlushRequest = LibPQ.sendFlushRequest c,
      Pqi.getCancel = fmap mkCancel <$> LibPQ.getCancel c,
      Pqi.notifies = fmap fromNotify <$> LibPQ.notifies c,
      Pqi.disableNoticeReporting = LibPQ.disableNoticeReporting c,
      Pqi.enableNoticeReporting = LibPQ.enableNoticeReporting c,
      Pqi.getNotice = LibPQ.getNotice c,
      Pqi.putCopyData = \value -> fromCopyInResult <$> LibPQ.putCopyData c value,
      Pqi.putCopyEnd = \reason -> fromCopyInResult <$> LibPQ.putCopyEnd c reason,
      Pqi.getCopyData = \nonBlocking -> fromCopyOutResult <$> LibPQ.getCopyData c nonBlocking,
      Pqi.loCreat = fmap fromOid <$> LibPQ.loCreat c,
      Pqi.loCreate = \oid -> fmap fromOid <$> LibPQ.loCreate c (toOid oid),
      Pqi.loImport = \path -> fmap fromOid <$> LibPQ.loImport c path,
      Pqi.loImportWithOid = \path oid -> fmap fromOid <$> LibPQ.loImportWithOid c path (toOid oid),
      Pqi.loExport = \oid path -> LibPQ.loExport c (toOid oid) path,
      Pqi.loOpen = \oid mode -> fmap fromLibPQLoFd <$> LibPQ.loOpen c (toOid oid) mode,
      Pqi.loWrite = \fd value -> LibPQ.loWrite c (toLibPQLoFd fd) value,
      Pqi.loRead = \fd len -> LibPQ.loRead c (toLibPQLoFd fd) len,
      Pqi.loSeek = \fd mode offset -> LibPQ.loSeek c (toLibPQLoFd fd) mode offset,
      Pqi.loTell = \fd -> LibPQ.loTell c (toLibPQLoFd fd),
      Pqi.loTruncate = \fd len -> LibPQ.loTruncate c (toLibPQLoFd fd) len,
      Pqi.loClose = \fd -> LibPQ.loClose c (toLibPQLoFd fd),
      Pqi.loUnlink = \oid -> LibPQ.loUnlink c (toOid oid),
      Pqi.clientEncoding = LibPQ.clientEncoding c,
      Pqi.setClientEncoding = \encoding -> LibPQ.setClientEncoding c encoding,
      Pqi.setErrorVerbosity = \verbosity ->
        fromVerbosity <$> LibPQ.setErrorVerbosity c (toVerbosity verbosity)
    }

-- | Build a 'Pqi.Result' whose fields close over the given
-- @postgresql-libpq@ result handle.
mkResult :: LibPQ.Result -> Pqi.Result
mkResult r =
  Pqi.Result
    { Pqi.resultStatus = fromExecStatus <$> LibPQ.resultStatus r,
      Pqi.resultErrorMessage = LibPQ.resultErrorMessage r,
      Pqi.resultErrorField = \field -> LibPQ.resultErrorField r (toFieldCode field),
      Pqi.unsafeFreeResult = LibPQ.unsafeFreeResult r,
      Pqi.ntuples = strictly r (fromRow <$> LibPQ.ntuples r),
      Pqi.nfields = strictly r (fromColumn <$> LibPQ.nfields r),
      Pqi.fname = \column -> LibPQ.fname r (toColumn column),
      Pqi.fnumber = \name -> fmap fromColumn <$> LibPQ.fnumber r name,
      Pqi.ftable = \column -> fromOid <$> LibPQ.ftable r (toColumn column),
      Pqi.ftablecol = \column -> fromColumn <$> LibPQ.ftablecol r (toColumn column),
      Pqi.fformat = \column -> fromFormat <$> LibPQ.fformat r (toColumn column),
      Pqi.ftype = \column -> fromOid <$> LibPQ.ftype r (toColumn column),
      Pqi.fmod = \column -> LibPQ.fmod r (toColumn column),
      Pqi.fsize = \column -> LibPQ.fsize r (toColumn column),
      Pqi.getvalue = \row column -> LibPQ.getvalue' r (toRow row) (toColumn column),
      Pqi.getvalue' = \row column -> LibPQ.getvalue' r (toRow row) (toColumn column),
      Pqi.getisnull = \row column -> LibPQ.getisnull r (toRow row) (toColumn column),
      Pqi.getlength = \row column -> LibPQ.getlength r (toRow row) (toColumn column),
      Pqi.nparams = fromIntegral <$> LibPQ.nparams r,
      Pqi.paramtype = \index -> fromOid <$> LibPQ.paramtype r (fromIntegral index),
      Pqi.cmdStatus = LibPQ.cmdStatus r,
      Pqi.cmdTuples = LibPQ.cmdTuples r
    }

-- | Read a number out of a result handle, forcing it while the handle is still
-- guaranteed to be alive.
--
-- @postgresql-libpq@ imports @PQntuples@ and @PQnfields@ as /pure/ functions
-- and defines the wrappers as @withResult res (return . toRow . c_PQntuples)@,
-- so the C call escapes 'Foreign.ForeignPtr.withForeignPtr' as an unevaluated
-- thunk over the raw @PGresult@ pointer. Should the handle become unreachable
-- before that thunk is forced, its @PQclear@ finalizer has already freed the
-- memory and the number read is garbage — which made the reference disagree
-- with a correct candidate whenever a collection happened to land in the
-- window. Every other accessor is imported in 'IO' and is unaffected.
strictly :: LibPQ.Result -> IO Int32 -> IO Int32
strictly r io = do
  !n <- io
  touch r
  pure n
  where
    touch x = GHC.IO.IO \s -> case GHC.Exts.touch# x s of s' -> (# s', () #)

-- | Build a 'Pqi.Cancel' whose field closes over the given
-- @postgresql-libpq@ cancellation handle.
mkCancel :: LibPQ.Cancel -> Pqi.Cancel
mkCancel handle =
  Pqi.Cancel
    { Pqi.cancel = LibPQ.cancel handle
    }

-- * Type conversions

toParam :: (Word32, ByteString, Pqi.Format) -> (LibPQ.Oid, ByteString, LibPQ.Format)
toParam (oid, value, format) = (toOid oid, value, toFormat format)

toBoundParam :: (ByteString, Pqi.Format) -> (ByteString, LibPQ.Format)
toBoundParam (value, format) = (value, toFormat format)

toOid :: Word32 -> LibPQ.Oid
toOid = LibPQ.Oid . fromIntegral

fromOid :: LibPQ.Oid -> Word32
fromOid (LibPQ.Oid value) = fromIntegral value

toRow :: Int32 -> LibPQ.Row
toRow = LibPQ.toRow

fromRow :: LibPQ.Row -> Int32
fromRow = fromIntegral . fromEnum

toColumn :: Int32 -> LibPQ.Column
toColumn = LibPQ.toColumn

fromColumn :: LibPQ.Column -> Int32
fromColumn = fromIntegral . fromEnum

toLibPQLoFd :: Int32 -> LibPQ.LoFd
toLibPQLoFd = LibPQ.LoFd . fromIntegral

fromLibPQLoFd :: LibPQ.LoFd -> Int32
fromLibPQLoFd (LibPQ.LoFd fd) = fromIntegral fd

fromNotify :: LibPQ.Notify -> Pqi.Notify
fromNotify notification =
  Pqi.Notify
    { Pqi.relname = LibPQ.notifyRelname notification,
      Pqi.bePid = fromIntegral (LibPQ.notifyBePid notification),
      Pqi.extra = LibPQ.notifyExtra notification
    }

toFormat :: Pqi.Format -> LibPQ.Format
toFormat = \case
  Pqi.Text -> LibPQ.Text
  Pqi.Binary -> LibPQ.Binary

fromFormat :: LibPQ.Format -> Pqi.Format
fromFormat = \case
  LibPQ.Text -> Pqi.Text
  LibPQ.Binary -> Pqi.Binary

fromExecStatus :: LibPQ.ExecStatus -> Pqi.ExecStatus
fromExecStatus = \case
  LibPQ.EmptyQuery -> Pqi.EmptyQuery
  LibPQ.CommandOk -> Pqi.CommandOk
  LibPQ.TuplesOk -> Pqi.TuplesOk
  LibPQ.CopyOut -> Pqi.CopyOut
  LibPQ.CopyIn -> Pqi.CopyIn
  LibPQ.CopyBoth -> Pqi.CopyBoth
  LibPQ.BadResponse -> Pqi.BadResponse
  LibPQ.NonfatalError -> Pqi.NonfatalError
  LibPQ.FatalError -> Pqi.FatalError
  LibPQ.SingleTuple -> Pqi.SingleTuple
  LibPQ.PipelineSync -> Pqi.PipelineSync
  LibPQ.PipelineAbort -> Pqi.PipelineAbort

toExecStatus :: Pqi.ExecStatus -> LibPQ.ExecStatus
toExecStatus = \case
  Pqi.EmptyQuery -> LibPQ.EmptyQuery
  Pqi.CommandOk -> LibPQ.CommandOk
  Pqi.TuplesOk -> LibPQ.TuplesOk
  Pqi.CopyOut -> LibPQ.CopyOut
  Pqi.CopyIn -> LibPQ.CopyIn
  Pqi.CopyBoth -> LibPQ.CopyBoth
  Pqi.BadResponse -> LibPQ.BadResponse
  Pqi.NonfatalError -> LibPQ.NonfatalError
  Pqi.FatalError -> LibPQ.FatalError
  Pqi.SingleTuple -> LibPQ.SingleTuple
  Pqi.PipelineSync -> LibPQ.PipelineSync
  Pqi.PipelineAbort -> LibPQ.PipelineAbort

fromConnStatus :: LibPQ.ConnStatus -> Pqi.ConnStatus
fromConnStatus = \case
  LibPQ.ConnectionOk -> Pqi.ConnectionOk
  LibPQ.ConnectionBad -> Pqi.ConnectionBad
  LibPQ.ConnectionStarted -> Pqi.ConnectionStarted
  LibPQ.ConnectionMade -> Pqi.ConnectionMade
  LibPQ.ConnectionAwaitingResponse -> Pqi.ConnectionAwaitingResponse
  LibPQ.ConnectionAuthOk -> Pqi.ConnectionAuthOk
  LibPQ.ConnectionSetEnv -> Pqi.ConnectionSetEnv
  LibPQ.ConnectionSSLStartup -> Pqi.ConnectionSSLStartup

fromTransactionStatus :: LibPQ.TransactionStatus -> Pqi.TransactionStatus
fromTransactionStatus = \case
  LibPQ.TransIdle -> Pqi.TransIdle
  LibPQ.TransActive -> Pqi.TransActive
  LibPQ.TransInTrans -> Pqi.TransInTrans
  LibPQ.TransInError -> Pqi.TransInError
  LibPQ.TransUnknown -> Pqi.TransUnknown

fromPollingStatus :: LibPQ.PollingStatus -> Pqi.PollingStatus
fromPollingStatus = \case
  LibPQ.PollingFailed -> Pqi.PollingFailed
  LibPQ.PollingReading -> Pqi.PollingReading
  LibPQ.PollingWriting -> Pqi.PollingWriting
  LibPQ.PollingOk -> Pqi.PollingOk

fromPipelineStatus :: LibPQ.PipelineStatus -> Pqi.PipelineStatus
fromPipelineStatus = \case
  LibPQ.PipelineOn -> Pqi.PipelineOn
  LibPQ.PipelineOff -> Pqi.PipelineOff
  LibPQ.PipelineAborted -> Pqi.PipelineAborted

fromFlushStatus :: LibPQ.FlushStatus -> Pqi.FlushStatus
fromFlushStatus = \case
  LibPQ.FlushOk -> Pqi.FlushOk
  LibPQ.FlushFailed -> Pqi.FlushFailed
  LibPQ.FlushWriting -> Pqi.FlushWriting

fromCopyInResult :: LibPQ.CopyInResult -> Pqi.CopyInResult
fromCopyInResult = \case
  LibPQ.CopyInOk -> Pqi.CopyInOk
  LibPQ.CopyInError -> Pqi.CopyInError
  LibPQ.CopyInWouldBlock -> Pqi.CopyInWouldBlock

fromCopyOutResult :: LibPQ.CopyOutResult -> Pqi.CopyOutResult
fromCopyOutResult = \case
  LibPQ.CopyOutRow value -> Pqi.CopyOutRow value
  LibPQ.CopyOutWouldBlock -> Pqi.CopyOutWouldBlock
  LibPQ.CopyOutDone -> Pqi.CopyOutDone
  LibPQ.CopyOutError -> Pqi.CopyOutError

toVerbosity :: Pqi.Verbosity -> LibPQ.Verbosity
toVerbosity = \case
  Pqi.ErrorsTerse -> LibPQ.ErrorsTerse
  Pqi.ErrorsDefault -> LibPQ.ErrorsDefault
  Pqi.ErrorsVerbose -> LibPQ.ErrorsVerbose

fromVerbosity :: LibPQ.Verbosity -> Pqi.Verbosity
fromVerbosity = \case
  LibPQ.ErrorsTerse -> Pqi.ErrorsTerse
  LibPQ.ErrorsDefault -> Pqi.ErrorsDefault
  LibPQ.ErrorsVerbose -> Pqi.ErrorsVerbose

toFieldCode :: Pqi.FieldCode -> LibPQ.FieldCode
toFieldCode = \case
  Pqi.DiagSeverity -> LibPQ.DiagSeverity
  Pqi.DiagSqlstate -> LibPQ.DiagSqlstate
  Pqi.DiagMessagePrimary -> LibPQ.DiagMessagePrimary
  Pqi.DiagMessageDetail -> LibPQ.DiagMessageDetail
  Pqi.DiagMessageHint -> LibPQ.DiagMessageHint
  Pqi.DiagStatementPosition -> LibPQ.DiagStatementPosition
  Pqi.DiagInternalPosition -> LibPQ.DiagInternalPosition
  Pqi.DiagInternalQuery -> LibPQ.DiagInternalQuery
  Pqi.DiagContext -> LibPQ.DiagContext
  Pqi.DiagSourceFile -> LibPQ.DiagSourceFile
  Pqi.DiagSourceLine -> LibPQ.DiagSourceLine
  Pqi.DiagSourceFunction -> LibPQ.DiagSourceFunction
