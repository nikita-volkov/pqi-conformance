-- | A reusable differential-testing toolkit for @pqi@ adapters.
--
-- An adapter's test suite calls 'specs' with its 'Pqi.Adapter' value. The
-- battery runs the same operation on the candidate and on the FFI reference
-- and asserts that the protocol-derived observations match.
--
-- The throwaway PostgreSQL container lifecycle is baked into 'specs', so
-- adapter test suites only need @hspec (specs MyAdapter.adapter)@; they
-- don't have to know about @testcontainers@ at all. The SCRAM-SHA-256
-- authentication spec is part of the @connectdb@ group and boots its own
-- password-auth container.
module Pqi.Conformance
  ( specs,
  )
where

import qualified Pqi
import Pqi.Conformance.Harness
import qualified Pqi.Conformance.Operation.BackendPID as BackendPID
import qualified Pqi.Conformance.Operation.Cancel as Cancel
import qualified Pqi.Conformance.Operation.ClientEncoding as ClientEncoding
import qualified Pqi.Conformance.Operation.CmdStatus as CmdStatus
import qualified Pqi.Conformance.Operation.CmdTuples as CmdTuples
import qualified Pqi.Conformance.Operation.ConnectPoll as ConnectPoll
import qualified Pqi.Conformance.Operation.ConnectStart as ConnectStart
import qualified Pqi.Conformance.Operation.Connectdb as Connectdb
import qualified Pqi.Conformance.Operation.ConnectionNeedsPassword as ConnectionNeedsPassword
import qualified Pqi.Conformance.Operation.ConnectionUsedPassword as ConnectionUsedPassword
import qualified Pqi.Conformance.Operation.ConsumeInput as ConsumeInput
import qualified Pqi.Conformance.Operation.Db as Db
import qualified Pqi.Conformance.Operation.DescribePortal as DescribePortal
import qualified Pqi.Conformance.Operation.DescribePrepared as DescribePrepared
import qualified Pqi.Conformance.Operation.DisableNoticeReporting as DisableNoticeReporting
import qualified Pqi.Conformance.Operation.EnableNoticeReporting as EnableNoticeReporting
import qualified Pqi.Conformance.Operation.EnterPipelineMode as EnterPipelineMode
import qualified Pqi.Conformance.Operation.ErrorMessage as ErrorMessage
import qualified Pqi.Conformance.Operation.EscapeByteaConn as EscapeByteaConn
import qualified Pqi.Conformance.Operation.EscapeIdentifier as EscapeIdentifier
import qualified Pqi.Conformance.Operation.EscapeStringConn as EscapeStringConn
import qualified Pqi.Conformance.Operation.Exec as Exec
import qualified Pqi.Conformance.Operation.ExecParams as ExecParams
import qualified Pqi.Conformance.Operation.ExecPrepared as ExecPrepared
import qualified Pqi.Conformance.Operation.ExitPipelineMode as ExitPipelineMode
import qualified Pqi.Conformance.Operation.Fformat as Fformat
import qualified Pqi.Conformance.Operation.Finish as Finish
import qualified Pqi.Conformance.Operation.Finish.PostFinishSend as Finish.PostFinishSend
import qualified Pqi.Conformance.Operation.Flush as Flush
import qualified Pqi.Conformance.Operation.Fmod as Fmod
import qualified Pqi.Conformance.Operation.Fname as Fname
import qualified Pqi.Conformance.Operation.Fnumber as Fnumber
import qualified Pqi.Conformance.Operation.Fsize as Fsize
import qualified Pqi.Conformance.Operation.Ftable as Ftable
import qualified Pqi.Conformance.Operation.Ftablecol as Ftablecol
import qualified Pqi.Conformance.Operation.Ftype as Ftype
import qualified Pqi.Conformance.Operation.GetCancel as GetCancel
import qualified Pqi.Conformance.Operation.GetCopyData as GetCopyData
import qualified Pqi.Conformance.Operation.GetNotice as GetNotice
import qualified Pqi.Conformance.Operation.GetResult as GetResult
import qualified Pqi.Conformance.Operation.Getisnull as Getisnull
import qualified Pqi.Conformance.Operation.Getlength as Getlength
import qualified Pqi.Conformance.Operation.Getvalue as Getvalue
import qualified Pqi.Conformance.Operation.GetvalueCopy as GetvalueCopy
import qualified Pqi.Conformance.Operation.Host as Host
import qualified Pqi.Conformance.Operation.IsBusy as IsBusy
import qualified Pqi.Conformance.Operation.IsNullConnection as IsNullConnection
import qualified Pqi.Conformance.Operation.Isnonblocking as Isnonblocking
import qualified Pqi.Conformance.Operation.LoClose as LoClose
import qualified Pqi.Conformance.Operation.LoCreat as LoCreat
import qualified Pqi.Conformance.Operation.LoCreate as LoCreate
import qualified Pqi.Conformance.Operation.LoExport as LoExport
import qualified Pqi.Conformance.Operation.LoImport as LoImport
import qualified Pqi.Conformance.Operation.LoImportWithOid as LoImportWithOid
import qualified Pqi.Conformance.Operation.LoOpen as LoOpen
import qualified Pqi.Conformance.Operation.LoRead as LoRead
import qualified Pqi.Conformance.Operation.LoSeek as LoSeek
import qualified Pqi.Conformance.Operation.LoTell as LoTell
import qualified Pqi.Conformance.Operation.LoTruncate as LoTruncate
import qualified Pqi.Conformance.Operation.LoUnlink as LoUnlink
import qualified Pqi.Conformance.Operation.LoWrite as LoWrite
import qualified Pqi.Conformance.Operation.NewNullConnection as NewNullConnection
import qualified Pqi.Conformance.Operation.Nfields as Nfields
import qualified Pqi.Conformance.Operation.Notifies as Notifies
import qualified Pqi.Conformance.Operation.Nparams as Nparams
import qualified Pqi.Conformance.Operation.Ntuples as Ntuples
import qualified Pqi.Conformance.Operation.Options as Options
import qualified Pqi.Conformance.Operation.ParameterStatus as ParameterStatus
import qualified Pqi.Conformance.Operation.Paramtype as Paramtype
import qualified Pqi.Conformance.Operation.Pass as Pass
import qualified Pqi.Conformance.Operation.PipelineStatus as PipelineStatus
import qualified Pqi.Conformance.Operation.PipelineSync as PipelineSync
import qualified Pqi.Conformance.Operation.Port as Port
import qualified Pqi.Conformance.Operation.Prepare as Prepare
import qualified Pqi.Conformance.Operation.ProtocolVersion as ProtocolVersion
import qualified Pqi.Conformance.Operation.PutCopyData as PutCopyData
import qualified Pqi.Conformance.Operation.PutCopyEnd as PutCopyEnd
import qualified Pqi.Conformance.Operation.Reset as Reset
import qualified Pqi.Conformance.Operation.ResetPoll as ResetPoll
import qualified Pqi.Conformance.Operation.ResetStart as ResetStart
import qualified Pqi.Conformance.Operation.ResultErrorField as ResultErrorField
import qualified Pqi.Conformance.Operation.ResultErrorMessage as ResultErrorMessage
import qualified Pqi.Conformance.Operation.ResultStatus as ResultStatus
import qualified Pqi.Conformance.Operation.SendDescribePortal as SendDescribePortal
import qualified Pqi.Conformance.Operation.SendDescribePrepared as SendDescribePrepared
import qualified Pqi.Conformance.Operation.SendFlushRequest as SendFlushRequest
import qualified Pqi.Conformance.Operation.SendPrepare as SendPrepare
import qualified Pqi.Conformance.Operation.SendQuery as SendQuery
import qualified Pqi.Conformance.Operation.SendQueryParams as SendQueryParams
import qualified Pqi.Conformance.Operation.SendQueryPrepared as SendQueryPrepared
import qualified Pqi.Conformance.Operation.ServerVersion as ServerVersion
import qualified Pqi.Conformance.Operation.SetClientEncoding as SetClientEncoding
import qualified Pqi.Conformance.Operation.SetErrorVerbosity as SetErrorVerbosity
import qualified Pqi.Conformance.Operation.SetSingleRowMode as SetSingleRowMode
import qualified Pqi.Conformance.Operation.Setnonblocking as Setnonblocking
import qualified Pqi.Conformance.Operation.Socket as Socket
import qualified Pqi.Conformance.Operation.Status as Status
import qualified Pqi.Conformance.Operation.TransactionStatus as TransactionStatus
import qualified Pqi.Conformance.Operation.UnescapeBytea as UnescapeBytea
import qualified Pqi.Conformance.Operation.UnsafeFreeResult as UnsafeFreeResult
import qualified Pqi.Conformance.Operation.User as User
import Test.Hspec

-- | The full conformance battery: every per-operation spec under one shared
-- trust-auth container, plus SCRAM-SHA-256 authentication (which boots its own
-- password-auth container). Every operation spec is differential against the
-- FFI reference.
specs :: Pqi.Adapter -> Spec
specs adapter = parallel do
  containerHook do
    -- Connection lifecycle
    Connectdb.spec adapter
    ConnectStart.spec adapter
    ConnectPoll.spec adapter
    NewNullConnection.spec adapter
    IsNullConnection.spec adapter
    Finish.spec adapter
    Finish.PostFinishSend.spec adapter
    Reset.spec adapter
    ResetStart.spec adapter
    ResetPoll.spec adapter
    -- Connection information accessors
    Db.spec adapter
    User.spec adapter
    Pass.spec adapter
    Host.spec adapter
    Port.spec adapter
    Options.spec adapter
    Status.spec adapter
    TransactionStatus.spec adapter
    ParameterStatus.spec adapter
    ProtocolVersion.spec adapter
    ServerVersion.spec adapter
    ErrorMessage.spec adapter
    Socket.spec adapter
    BackendPID.spec adapter
    ConnectionNeedsPassword.spec adapter
    ConnectionUsedPassword.spec adapter
    -- Querying
    Exec.spec adapter
    ExecParams.spec adapter
    Prepare.spec adapter
    ExecPrepared.spec adapter
    DescribePrepared.spec adapter
    DescribePortal.spec adapter
    -- Escaping
    EscapeStringConn.spec adapter
    EscapeByteaConn.spec adapter
    EscapeIdentifier.spec adapter
    UnescapeBytea.spec adapter
    -- Asynchronous command processing
    SendQuery.spec adapter
    SendQueryParams.spec adapter
    SendPrepare.spec adapter
    SendQueryPrepared.spec adapter
    SendDescribePrepared.spec adapter
    SendDescribePortal.spec adapter
    GetResult.spec adapter
    ConsumeInput.spec adapter
    IsBusy.spec adapter
    Setnonblocking.spec adapter
    Isnonblocking.spec adapter
    SetSingleRowMode.spec adapter
    Flush.spec adapter
    -- Pipelining
    PipelineStatus.spec adapter
    EnterPipelineMode.spec adapter
    ExitPipelineMode.spec adapter
    PipelineSync.spec adapter
    SendFlushRequest.spec adapter
    -- Cancellation
    GetCancel.spec adapter
    Cancel.spec adapter
    -- Notifications and notices
    Notifies.spec adapter
    DisableNoticeReporting.spec adapter
    EnableNoticeReporting.spec adapter
    GetNotice.spec adapter
    -- Copy sub-protocol
    PutCopyData.spec adapter
    PutCopyEnd.spec adapter
    GetCopyData.spec adapter
    -- Large objects
    LoCreat.spec adapter
    LoCreate.spec adapter
    LoImport.spec adapter
    LoImportWithOid.spec adapter
    LoExport.spec adapter
    LoOpen.spec adapter
    LoWrite.spec adapter
    LoRead.spec adapter
    LoSeek.spec adapter
    LoTell.spec adapter
    LoTruncate.spec adapter
    LoClose.spec adapter
    LoUnlink.spec adapter
    -- Connection control
    ClientEncoding.spec adapter
    SetClientEncoding.spec adapter
    SetErrorVerbosity.spec adapter
    -- Result inspection
    ResultStatus.spec adapter
    ResultErrorMessage.spec adapter
    ResultErrorField.spec adapter
    UnsafeFreeResult.spec adapter
    Ntuples.spec adapter
    Nfields.spec adapter
    Fname.spec adapter
    Fnumber.spec adapter
    Ftable.spec adapter
    Ftablecol.spec adapter
    Fformat.spec adapter
    Ftype.spec adapter
    Fmod.spec adapter
    Fsize.spec adapter
    Getvalue.spec adapter
    GetvalueCopy.spec adapter
    Getisnull.spec adapter
    Getlength.spec adapter
    Nparams.spec adapter
    Paramtype.spec adapter
    CmdStatus.spec adapter
    CmdTuples.spec adapter
