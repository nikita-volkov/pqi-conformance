-- | Plain, comparable snapshots of a connection or result.
--
-- Every adapter produces the same concrete 'Pqi.Connection'\/'Pqi.Result'
-- record, so comparing candidate and reference values directly would also
-- compare the per-connection identity fields (@backendPID@, @socket@), which
-- are structurally incomparable across two independently-opened connections.
-- Projecting each into one of these driver-independent records first keeps
-- the comparison to protocol-derived information only.
--
-- Only protocol-derived information is captured: both adapters parse the same
-- wire bytes, so these fields genuinely agree. All result fields — including
-- the flat error message text and all structured error fields — are captured
-- in full and compared byte-identically.
module Pqi.Conformance.Observation
  ( ResultObservation (..),
    FieldObservation (..),
    CellObservation (..),
    observeResult,
    ConnectionObservation (..),
    observeConnection,
  )
where

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
observeResult :: Lq.Result -> IO ResultObservation
observeResult result = do
  status <- result . resultStatus
  errorFields <-
    traverse
      (\code -> (,) code <$> result . resultErrorField code)
      [minBound .. maxBound]
  errorMessage <- result . resultErrorMessage
  ntuples <- result . ntuples
  nfields <- result . nfields
  nparams <- result . nparams
  paramTypes <- traverse result . paramtype [0 .. nparams - 1]
  fields <- traverse (observeField result) [0 .. nfields - 1]
  rows <- traverse (\row -> traverse (observeCell result row) [0 .. nfields - 1]) [0 .. ntuples - 1]
  cmdStatus <- result . cmdStatus
  cmdTuples <- result . cmdTuples
  pure ResultObservation {..}

observeField :: Lq.Result -> Int32 -> IO FieldObservation
observeField result column = do
  name <- result . fname column
  typeOid <- result . ftype column
  modifier <- result . fmod column
  size <- result . fsize column
  format <- result . fformat column
  tableOid <- result . ftable column
  tableColumn <- result . ftablecol column
  pure FieldObservation {..}

observeCell :: Lq.Result -> Int32 -> Int32 -> IO CellObservation
observeCell result row column = do
  value <- result . getvalue row column
  isNull <- result . getisnull row column
  length <- result . getlength row column
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
observeConnection :: Lq.Connection -> IO ConnectionObservation
observeConnection connection = do
  status <- connection . status
  transactionStatus <- connection . transactionStatus
  serverVersion <- connection . serverVersion
  serverVersionParam <- connection . parameterStatus "server_version"
  protocolVersion <- connection . protocolVersion
  db <- connection . db
  user <- connection . user
  pass <- connection . pass
  host <- connection . host
  port <- connection . port
  options <- connection . options
  connectionNeedsPassword <- connection . connectionNeedsPassword
  connectionUsedPassword <- connection . connectionUsedPassword
  let isNull = connection . isNullConnection
  pure ConnectionObservation {..}
