-- | Plain, comparable snapshots of a connection or result.
--
-- The candidate adapter and the FFI reference produce values of /different/
-- types (@'Pqi.ResultOf' candidate@ vs @'Pqi.ResultOf'
-- 'Pqi.Ffi.Connection'@), so they cannot be compared directly. Instead we
-- project each into one of these driver-independent records and compare those.
--
-- Only protocol-derived information is captured: both adapters parse the same
-- wire bytes, so these fields genuinely agree. Per-connection identity values
-- (@backendPID@, @socket@) are structurally incomparable across connections
-- and are omitted. All result fields — including the flat error message text
-- and all structured error fields — are captured in full and compared
-- byte-identically.
module Pqi.Conformance.Observation
  ( ResultObservation (..),
    FieldObservation (..),
    CellObservation (..),
    observeResult,
    ConnectionObservation (..),
    observeConnection,
  )
where

import Pqi (IsConnection (..), IsResult (..))
import qualified Pqi as Lq
import Pqi.Conformance.Prelude

-- | A snapshot of an entire result: its status, structured error report,
-- shape, per-field metadata, and every cell.
data ResultObservation = ResultObservation
  { status :: Lq.ExecStatus,
    -- | Every structured field of the error report, keyed by 'Lq.FieldCode'.
    -- All of them are carried by the wire error response.
    errorFields :: [(Lq.FieldCode, Maybe ByteString)],
    -- | The flat formatted error message, byte-identical to libpq's
    -- @PQresultErrorMessage@ at DEFAULT verbosity.
    errorMessage :: Maybe ByteString,
    ntuples :: Int32,
    nfields :: Int32,
    nparams :: Int32,
    paramTypes :: [Word32],
    fields :: [FieldObservation],
    rows :: [[CellObservation]],
    cmdStatus :: Maybe ByteString,
    cmdTuples :: Maybe ByteString
  }
  deriving stock (Eq, Show)

-- | A snapshot of one result column's metadata.
data FieldObservation = FieldObservation
  { name :: Maybe ByteString,
    typeOid :: Word32,
    modifier :: Int,
    size :: Int,
    format :: Lq.Format,
    tableOid :: Word32,
    tableColumn :: Int32
  }
  deriving stock (Eq, Show)

-- | A snapshot of one cell.
data CellObservation = CellObservation
  { value :: Maybe ByteString,
    isNull :: Bool,
    length :: Int
  }
  deriving stock (Eq, Show)

-- | Project a result into a 'ResultObservation'.
observeResult :: (IsResult r) => r -> IO ResultObservation
observeResult result = do
  status <- Lq.resultStatus result
  errorFields <-
    traverse
      (\code -> (,) code <$> Lq.resultErrorField result code)
      [minBound .. maxBound]
  errorMessage <- Lq.resultErrorMessage result
  ntuples <- Lq.ntuples result
  nfields <- Lq.nfields result
  nparams <- Lq.nparams result
  paramTypes <- traverse (Lq.paramtype result) [0 .. nparams - 1]
  fields <- traverse (observeField result) [0 .. nfields - 1]
  rows <- traverse (\row -> traverse (observeCell result row) [0 .. nfields - 1]) [0 .. ntuples - 1]
  cmdStatus <- Lq.cmdStatus result
  cmdTuples <- Lq.cmdTuples result
  pure ResultObservation {..}

observeField :: (IsResult r) => r -> Int32 -> IO FieldObservation
observeField result column = do
  name <- Lq.fname result column
  typeOid <- Lq.ftype result column
  modifier <- Lq.fmod result column
  size <- Lq.fsize result column
  format <- Lq.fformat result column
  tableOid <- Lq.ftable result column
  tableColumn <- Lq.ftablecol result column
  pure FieldObservation {..}

observeCell :: (IsResult r) => r -> Int32 -> Int32 -> IO CellObservation
observeCell result row column = do
  value <- Lq.getvalue result row column
  isNull <- Lq.getisnull result row column
  length <- Lq.getlength result row column
  pure CellObservation {..}

-- | A snapshot of the comparable portion of a connection's state. The
-- candidate and the reference open their connections from the same conninfo
-- string, so the conninfo-derived identity accessors agree as well.
data ConnectionObservation = ConnectionObservation
  { status :: Lq.ConnStatus,
    transactionStatus :: Lq.TransactionStatus,
    serverVersion :: Int,
    serverVersionParam :: Maybe ByteString,
    protocolVersion :: Int,
    db :: Maybe ByteString,
    user :: Maybe ByteString,
    pass :: Maybe ByteString,
    host :: Maybe ByteString,
    port :: Maybe ByteString,
    options :: Maybe ByteString,
    connectionNeedsPassword :: Bool,
    connectionUsedPassword :: Bool,
    isNull :: Bool
  }
  deriving stock (Eq, Show)

-- | Project a connection into a 'ConnectionObservation'.
observeConnection :: (IsConnection c) => c -> IO ConnectionObservation
observeConnection connection = do
  status <- Lq.status connection
  transactionStatus <- Lq.transactionStatus connection
  serverVersion <- Lq.serverVersion connection
  serverVersionParam <- Lq.parameterStatus connection "server_version"
  protocolVersion <- Lq.protocolVersion connection
  db <- Lq.db connection
  user <- Lq.user connection
  pass <- Lq.pass connection
  host <- Lq.host connection
  port <- Lq.port connection
  options <- Lq.options connection
  connectionNeedsPassword <- Lq.connectionNeedsPassword connection
  connectionUsedPassword <- Lq.connectionUsedPassword connection
  let isNull = Lq.isNullConnection connection
  pure ConnectionObservation {..}
