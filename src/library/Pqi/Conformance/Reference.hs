-- | The reference connection: a direct @postgresql-libpq@ wrapper used as the
-- ground truth in differential tests. It is intentionally independent of the
-- @pqi-ffi@ adapter so that adapter test suites can depend on
-- @pqi-conformance@ without a circular dependency through @pqi-ffi@.
module Pqi.Conformance.Reference
  ( Reference (..),
    RefResult (..),
    RefCancel (..),
  )
where

import qualified Database.PostgreSQL.LibPQ as LibPQ
import Pqi
  ( IsCancel (..),
    IsConnection (..),
    IsResult (..),
  )
import qualified Pqi
import Pqi.Conformance.Prelude

-- | A direct wrapper over a @postgresql-libpq@ connection, used as the
-- differential-testing reference.
newtype Reference = Reference LibPQ.Connection

-- | A result backed by a C @PGresult@.
newtype RefResult = RefResult LibPQ.Result

-- | A cancellation handle backed by a C @PGcancel@.
newtype RefCancel = RefCancel LibPQ.Cancel

instance IsResult RefResult where
  resultStatus (RefResult r) = fromExecStatus <$> LibPQ.resultStatus r
  resultErrorMessage (RefResult r) = LibPQ.resultErrorMessage r
  resultErrorField (RefResult r) field = LibPQ.resultErrorField r (toFieldCode field)
  unsafeFreeResult (RefResult r) = LibPQ.unsafeFreeResult r
  ntuples (RefResult r) = fromRow <$> LibPQ.ntuples r
  nfields (RefResult r) = fromColumn <$> LibPQ.nfields r
  fname (RefResult r) column = LibPQ.fname r (toColumn column)
  fnumber (RefResult r) name = fmap fromColumn <$> LibPQ.fnumber r name
  ftable (RefResult r) column = fromOid <$> LibPQ.ftable r (toColumn column)
  ftablecol (RefResult r) column = fromColumn <$> LibPQ.ftablecol r (toColumn column)
  fformat (RefResult r) column = fromFormat <$> LibPQ.fformat r (toColumn column)
  ftype (RefResult r) column = fromOid <$> LibPQ.ftype r (toColumn column)
  fmod (RefResult r) column = LibPQ.fmod r (toColumn column)
  fsize (RefResult r) column = LibPQ.fsize r (toColumn column)
  getvalue (RefResult r) row column = LibPQ.getvalue r (toRow row) (toColumn column)
  getvalue' (RefResult r) row column = LibPQ.getvalue' r (toRow row) (toColumn column)
  getisnull (RefResult r) row column = LibPQ.getisnull r (toRow row) (toColumn column)
  getlength (RefResult r) row column = LibPQ.getlength r (toRow row) (toColumn column)
  nparams (RefResult r) = fromIntegral <$> LibPQ.nparams r
  paramtype (RefResult r) index = fromOid <$> LibPQ.paramtype r (fromIntegral index)
  cmdStatus (RefResult r) = LibPQ.cmdStatus r
  cmdTuples (RefResult r) = LibPQ.cmdTuples r

instance IsCancel RefCancel where
  cancel (RefCancel handle) = LibPQ.cancel handle

instance IsConnection Reference where
  type ResultOf Reference = RefResult
  type CancelOf Reference = RefCancel

  connectdb conninfo = Reference <$> LibPQ.connectdb conninfo
  connectStart conninfo = Reference <$> LibPQ.connectStart conninfo
  connectPoll (Reference c) = fromPollingStatus <$> LibPQ.connectPoll c
  newNullConnection = Reference <$> LibPQ.newNullConnection
  isNullConnection (Reference c) = LibPQ.isNullConnection c
  finish (Reference c) = LibPQ.finish c
  reset (Reference c) = LibPQ.reset c
  resetStart (Reference c) = LibPQ.resetStart c
  resetPoll (Reference c) = fromPollingStatus <$> LibPQ.resetPoll c
  db (Reference c) = LibPQ.db c
  user (Reference c) = LibPQ.user c
  pass (Reference c) = LibPQ.pass c
  host (Reference c) = LibPQ.host c
  port (Reference c) = LibPQ.port c
  options (Reference c) = LibPQ.options c
  status (Reference c) = fromConnStatus <$> LibPQ.status c
  transactionStatus (Reference c) = fromTransactionStatus <$> LibPQ.transactionStatus c
  parameterStatus (Reference c) name = LibPQ.parameterStatus c name
  protocolVersion (Reference c) = LibPQ.protocolVersion c
  serverVersion (Reference c) = LibPQ.serverVersion c
  errorMessage (Reference c) = LibPQ.errorMessage c
  socket (Reference c) = LibPQ.socket c
  backendPID (Reference c) = fromIntegral <$> LibPQ.backendPID c
  connectionNeedsPassword (Reference c) = LibPQ.connectionNeedsPassword c
  connectionUsedPassword (Reference c) = LibPQ.connectionUsedPassword c

  exec (Reference c) sql =
    fmap RefResult <$> LibPQ.exec c sql
  execParams (Reference c) sql params resultFormat =
    fmap RefResult <$> LibPQ.execParams c sql (fmap (fmap toParam) params) (toFormat resultFormat)
  prepare (Reference c) name sql paramTypes =
    fmap RefResult <$> LibPQ.prepare c name sql (fmap (fmap toOid) paramTypes)
  execPrepared (Reference c) name params resultFormat =
    fmap RefResult <$> LibPQ.execPrepared c name (fmap (fmap toBoundParam) params) (toFormat resultFormat)
  describePrepared (Reference c) name =
    fmap RefResult <$> LibPQ.describePrepared c name
  describePortal (Reference c) name =
    fmap RefResult <$> LibPQ.describePortal c name

  escapeStringConn (Reference c) = LibPQ.escapeStringConn c
  escapeByteaConn (Reference c) = LibPQ.escapeByteaConn c
  escapeIdentifier (Reference c) = LibPQ.escapeIdentifier c

  sendQuery (Reference c) sql = LibPQ.sendQuery c sql
  sendQueryParams (Reference c) sql params resultFormat =
    LibPQ.sendQueryParams c sql (fmap (fmap toParam) params) (toFormat resultFormat)
  sendPrepare (Reference c) name sql paramTypes =
    LibPQ.sendPrepare c name sql (fmap (fmap toOid) paramTypes)
  sendQueryPrepared (Reference c) name params resultFormat =
    LibPQ.sendQueryPrepared c name (fmap (fmap toBoundParam) params) (toFormat resultFormat)
  sendDescribePrepared (Reference c) name = LibPQ.sendDescribePrepared c name
  sendDescribePortal (Reference c) name = LibPQ.sendDescribePortal c name
  getResult (Reference c) = fmap RefResult <$> LibPQ.getResult c
  consumeInput (Reference c) = LibPQ.consumeInput c
  isBusy (Reference c) = LibPQ.isBusy c
  setnonblocking (Reference c) nonBlocking = LibPQ.setnonblocking c nonBlocking
  isnonblocking (Reference c) = LibPQ.isnonblocking c
  setSingleRowMode (Reference c) = LibPQ.setSingleRowMode c
  flush (Reference c) = fromFlushStatus <$> LibPQ.flush c

  pipelineStatus (Reference c) = fromPipelineStatus <$> LibPQ.pipelineStatus c
  enterPipelineMode (Reference c) = LibPQ.enterPipelineMode c
  exitPipelineMode (Reference c) = LibPQ.exitPipelineMode c
  pipelineSync (Reference c) = LibPQ.pipelineSync c
  sendFlushRequest (Reference c) = LibPQ.sendFlushRequest c

  getCancel (Reference c) = fmap RefCancel <$> LibPQ.getCancel c

  notifies (Reference c) = fmap fromNotify <$> LibPQ.notifies c
  disableNoticeReporting (Reference c) = LibPQ.disableNoticeReporting c
  enableNoticeReporting (Reference c) = LibPQ.enableNoticeReporting c
  getNotice (Reference c) = LibPQ.getNotice c

  putCopyData (Reference c) value = fromCopyInResult <$> LibPQ.putCopyData c value
  putCopyEnd (Reference c) reason = fromCopyInResult <$> LibPQ.putCopyEnd c reason
  getCopyData (Reference c) nonBlocking = fromCopyOutResult <$> LibPQ.getCopyData c nonBlocking

  loCreat (Reference c) = fmap fromOid <$> LibPQ.loCreat c
  loCreate (Reference c) oid = fmap fromOid <$> LibPQ.loCreate c (toOid oid)
  loImport (Reference c) path = fmap fromOid <$> LibPQ.loImport c path
  loImportWithOid (Reference c) path oid = fmap fromOid <$> LibPQ.loImportWithOid c path (toOid oid)
  loExport (Reference c) oid path = LibPQ.loExport c (toOid oid) path
  loOpen (Reference c) oid mode = fmap fromLoFd <$> LibPQ.loOpen c (toOid oid) mode
  loWrite (Reference c) fd value = LibPQ.loWrite c (toLoFd fd) value
  loRead (Reference c) fd len = LibPQ.loRead c (toLoFd fd) len
  loSeek (Reference c) fd mode offset = LibPQ.loSeek c (toLoFd fd) mode offset
  loTell (Reference c) fd = LibPQ.loTell c (toLoFd fd)
  loTruncate (Reference c) fd len = LibPQ.loTruncate c (toLoFd fd) len
  loClose (Reference c) fd = LibPQ.loClose c (toLoFd fd)
  loUnlink (Reference c) oid = LibPQ.loUnlink c (toOid oid)

  clientEncoding (Reference c) = LibPQ.clientEncoding c
  setClientEncoding (Reference c) encoding = LibPQ.setClientEncoding c encoding
  setErrorVerbosity (Reference c) verbosity =
    fromVerbosity <$> LibPQ.setErrorVerbosity c (toVerbosity verbosity)

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

toLoFd :: Pqi.LoFd -> LibPQ.LoFd
toLoFd (Pqi.LoFd fd) = LibPQ.LoFd (fromIntegral fd)

fromLoFd :: LibPQ.LoFd -> Pqi.LoFd
fromLoFd (LibPQ.LoFd fd) = Pqi.LoFd (fromIntegral fd)

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
