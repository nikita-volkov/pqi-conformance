-- | A reusable differential-testing toolkit for @pqi@ adapters.
--
-- An adapter's test suite calls 'specs' with a @'Proxy' \@MyConnection@. The
-- battery runs the same operation on the candidate and on the FFI reference
-- and asserts that the protocol-derived observations match.
--
-- The throwaway PostgreSQL container lifecycle is baked into 'specs', so
-- adapter test suites only need @hspec (specs (Proxy \@MyConnection))@; they
-- don't have to know about @testcontainers@ at all. The SCRAM-SHA-256
-- authentication spec is part of the @connectdb@ group and boots its own
-- password-auth container.
module Pqi.Conformance
  ( specs,
  )
where

import Pqi (IsConnection)
import Pqi.Conformance.Harness
import qualified Pqi.Conformance.Operation.BackendPID as BackendPID
import qualified Pqi.Conformance.Operation.Cancel as Cancel
import qualified Pqi.Conformance.Operation.CancelCleanup as CancelCleanup
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
import qualified Pqi.Conformance.Operation.StaleCancel as StaleCancel
import qualified Pqi.Conformance.Operation.Status as Status
import qualified Pqi.Conformance.Operation.TransactionStatus as TransactionStatus
import qualified Pqi.Conformance.Operation.UnsafeFreeResult as UnsafeFreeResult
import qualified Pqi.Conformance.Operation.User as User
import Pqi.Conformance.Prelude
import Test.Hspec

-- | The full conformance battery: every per-operation spec under one shared
-- trust-auth container, plus SCRAM-SHA-256 authentication (which boots its own
-- password-auth container). Every operation spec is differential against the
-- FFI reference.
specs :: (IsConnection c) => Proxy c -> Spec
specs proxy = parallel do
  containerHook do
    -- Connection lifecycle
    Connectdb.spec proxy
    ConnectStart.spec proxy
    ConnectPoll.spec proxy
    NewNullConnection.spec proxy
    IsNullConnection.spec proxy
    Finish.spec proxy
    Reset.spec proxy
    ResetStart.spec proxy
    ResetPoll.spec proxy
    -- Connection information accessors
    Db.spec proxy
    User.spec proxy
    Pass.spec proxy
    Host.spec proxy
    Port.spec proxy
    Options.spec proxy
    Status.spec proxy
    TransactionStatus.spec proxy
    ParameterStatus.spec proxy
    ProtocolVersion.spec proxy
    ServerVersion.spec proxy
    ErrorMessage.spec proxy
    Socket.spec proxy
    BackendPID.spec proxy
    ConnectionNeedsPassword.spec proxy
    ConnectionUsedPassword.spec proxy
    -- Querying
    Exec.spec proxy
    ExecParams.spec proxy
    Prepare.spec proxy
    ExecPrepared.spec proxy
    DescribePrepared.spec proxy
    DescribePortal.spec proxy
    -- Escaping
    EscapeStringConn.spec proxy
    EscapeByteaConn.spec proxy
    EscapeIdentifier.spec proxy
    -- Asynchronous command processing
    SendQuery.spec proxy
    SendQueryParams.spec proxy
    SendPrepare.spec proxy
    SendQueryPrepared.spec proxy
    SendDescribePrepared.spec proxy
    SendDescribePortal.spec proxy
    GetResult.spec proxy
    ConsumeInput.spec proxy
    IsBusy.spec proxy
    Setnonblocking.spec proxy
    Isnonblocking.spec proxy
    SetSingleRowMode.spec proxy
    Flush.spec proxy
    -- Pipelining
    PipelineStatus.spec proxy
    EnterPipelineMode.spec proxy
    ExitPipelineMode.spec proxy
    PipelineSync.spec proxy
    SendFlushRequest.spec proxy
    -- Cancellation
    GetCancel.spec proxy
    Cancel.spec proxy
    CancelCleanup.spec proxy
    StaleCancel.spec proxy
    -- Notifications and notices
    Notifies.spec proxy
    DisableNoticeReporting.spec proxy
    EnableNoticeReporting.spec proxy
    GetNotice.spec proxy
    -- Copy sub-protocol
    PutCopyData.spec proxy
    PutCopyEnd.spec proxy
    GetCopyData.spec proxy
    -- Large objects
    LoCreat.spec proxy
    LoCreate.spec proxy
    LoImport.spec proxy
    LoImportWithOid.spec proxy
    LoExport.spec proxy
    LoOpen.spec proxy
    LoWrite.spec proxy
    LoRead.spec proxy
    LoSeek.spec proxy
    LoTell.spec proxy
    LoTruncate.spec proxy
    LoClose.spec proxy
    LoUnlink.spec proxy
    -- Connection control
    ClientEncoding.spec proxy
    SetClientEncoding.spec proxy
    SetErrorVerbosity.spec proxy
    -- Result inspection
    ResultStatus.spec proxy
    ResultErrorMessage.spec proxy
    ResultErrorField.spec proxy
    UnsafeFreeResult.spec proxy
    Ntuples.spec proxy
    Nfields.spec proxy
    Fname.spec proxy
    Fnumber.spec proxy
    Ftable.spec proxy
    Ftablecol.spec proxy
    Fformat.spec proxy
    Ftype.spec proxy
    Fmod.spec proxy
    Fsize.spec proxy
    Getvalue.spec proxy
    GetvalueCopy.spec proxy
    Getisnull.spec proxy
    Getlength.spec proxy
    Nparams.spec proxy
    Paramtype.spec proxy
    CmdStatus.spec proxy
    CmdTuples.spec proxy
