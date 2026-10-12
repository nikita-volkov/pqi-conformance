-- | Fixtures for killing a live connection in a deterministic way: an
-- in-process TCP proxy between the victim and the container, which can end
-- the connection in each of the ways a connection can really die.
--
-- A direct connection cannot be killed deterministically. A dead server
-- answers the victim's next write with a reset, and that reset races with the
-- victim reading whatever the server said before it went away (the @FATAL@
-- of a terminated backend, or just the EOF): the same scenario run against
-- the same server surfaces as a clean EOF in most attempts and as a
-- @Connection reset by peer@ in the rest. The proxy removes the race by
-- being the one party that decides what the victim sees:
--
-- * Once the database side is gone, the client side of the proxy /stays
--   open and keeps being drained/. The victim's next write is swallowed
--   instead of reset, so \"the send still succeeds, the read finds the EOF\"
--   holds on every run.
-- * A reset is delivered exactly when asked for, instead of whenever the
--   kernel gets around to answering a write.
--
-- The proxy rewrites nothing at the protocol level, so it works for any
-- adapter.
module Pqi.Conformance.Fixture.ConnectionLoss
  ( -- * Proxy
    withProxy,
    Proxy (..),

    -- * Server-side termination
    terminateBackend,
  )
where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, tryPutMVar, tryReadMVar)
import Control.Exception (IOException, SomeException, bracket, bracketOnError, catch, finally, onException, try)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString
import qualified Pqi
import Pqi.Conformance.Prelude
import qualified Pqi.Conformance.Reference as Reference

-- | The means of ending the proxied connection. All of them need the victim
-- to have connected first.
data Proxy = Proxy
  { -- | Silent close: shut down the database-facing socket, so that the
    -- victim receives a graceful close (FIN) without any protocol message
    -- before it. The client-facing socket stays open and keeps being
    -- drained. Returns once the FIN has been delivered to the victim.
    sever :: IO (),
    -- | Reset: close the client-facing socket with @SO_LINGER@ @{on, 0}@, so
    -- that the victim receives an @RST@, and close the database side. Returns
    -- once both forwarding threads have stopped.
    reset :: IO (),
    -- | Block until the /database/ side has closed on its own (after
    -- 'terminateBackend', for instance) and its last bytes, together with
    -- the FIN, have been delivered to the victim. From then on the victim's
    -- writes are swallowed, never forwarded to the dead server (which would
    -- answer them with a reset that races with the victim's read).
    serverClosed :: IO ()
  }

-- | Hand the action a conninfo that routes through a proxy to the same
-- server as the given one, together with the means of killing the connection.
--
-- The proxy accepts one client. The listening socket and every thread
-- started here are gone when the action returns.
withProxy :: ByteString -> (ByteString -> Proxy -> IO a) -> IO a
withProxy conninfo action =
  bracket listenOnLoopback Socket.close \listener -> do
    port <- Socket.socketPort listener
    sessionVar <- newEmptyMVar
    let serve = try @SomeException (establish listener host upstreamPort) >>= void . tryPutMVar sessionVar
        established = readMVar sessionVar >>= either (ioError . userError . show) pure
        proxy =
          Proxy
            { sever = do
                Session {..} <- established
                quietly (Socket.shutdown upstream Socket.ShutdownBoth)
                readMVar upstreamGone,
              reset = do
                Session {..} <- established
                quietly (Socket.setSockOptValue client Socket.Linger (Socket.SockOptValue (Socket.StructLinger 1 0)))
                quietly (Socket.close client)
                quietly (Socket.shutdown upstream Socket.ShutdownBoth)
                readMVar clientToUpstreamStopped
                readMVar upstreamToClientStopped,
              serverClosed = do
                Session {..} <- established
                readMVar upstreamGone
            }
        proxiedConninfo = conninfo <> " host=127.0.0.1 port=" <> ByteString.Char8.pack (show port)
    bracket (forkIO serve) (\_ -> teardown listener sessionVar) \_ -> action proxiedConninfo proxy
  where
    (host, upstreamPort) = upstreamAddress conninfo
    teardown listener sessionVar = do
      -- Closing the listener wakes an acceptor still waiting for a client,
      -- and the acceptor always fills the session variable, so this is
      -- bounded.
      quietly (Socket.close listener)
      readMVar sessionVar >>= \case
        Left _ -> pure ()
        Right Session {..} -> do
          quietly (Socket.shutdown client Socket.ShutdownBoth)
          quietly (Socket.shutdown upstream Socket.ShutdownBoth)
          readMVar clientToUpstreamStopped
          readMVar upstreamToClientStopped
          quietly (Socket.close client)
          quietly (Socket.close upstream)

-- | One proxied connection: the two sockets, and the state of the two
-- forwarding threads.
data Session = Session
  { client :: Socket.Socket,
    upstream :: Socket.Socket,
    -- | Filled when the database side has reached EOF (or failed) and the
    -- FIN has been forwarded to the client. Writes of the client are
    -- swallowed from then on.
    upstreamGone :: MVar (),
    clientToUpstreamStopped :: MVar (),
    upstreamToClientStopped :: MVar ()
  }

-- | Accept the client, dial the database and start the forwarding threads.
establish :: Socket.Socket -> String -> String -> IO Session
establish listener host port = do
  (client, _) <- Socket.accept listener
  upstream <- connectUpstream host port `onException` Socket.close client
  quietly (Socket.setSocketOption client Socket.NoDelay 1)
  quietly (Socket.setSocketOption upstream Socket.NoDelay 1)
  upstreamGone <- newEmptyMVar
  clientToUpstreamStopped <- newEmptyMVar
  upstreamToClientStopped <- newEmptyMVar
  _ <- forkIO (quietly (clientToUpstream client upstream upstreamGone) `finally` putMVar clientToUpstreamStopped ())
  _ <- forkIO (quietly (upstreamToClient client upstream upstreamGone) `finally` putMVar upstreamToClientStopped ())
  pure Session {..}

-- | Forward what the victim writes to the database, until the victim closes
-- its side. After the database side is gone, keep reading and discard.
clientToUpstream :: Socket.Socket -> Socket.Socket -> MVar () -> IO ()
clientToUpstream client upstream upstreamGone =
  pump `finally` quietly (Socket.shutdown upstream Socket.ShutdownSend)
  where
    pump = do
      chunk <- Socket.ByteString.recv client chunkSize
      unless (ByteString.null chunk) do
        gone <- isJust <$> tryReadMVar upstreamGone
        unless gone (quietly (Socket.ByteString.sendAll upstream chunk))
        pump

-- | Forward what the database writes to the victim, until the database side
-- ends, and pass the end on to the victim as a FIN.
upstreamToClient :: Socket.Socket -> Socket.Socket -> MVar () -> IO ()
upstreamToClient client upstream upstreamGone =
  pump `finally` do
    quietly (Socket.shutdown client Socket.ShutdownSend)
    void (tryPutMVar upstreamGone ())
  where
    pump = do
      chunk <- Socket.ByteString.recv upstream chunkSize
      unless (ByteString.null chunk) do
        Socket.ByteString.sendAll client chunk
        pump

chunkSize :: Int
chunkSize = 16384

-- | Terminate a backend from a second connection, opened directly to the
-- container (the conninfo is the harness's, not a proxied one), and wait
-- until the backend is gone from @pg_stat_activity@.
--
-- Uses the one-argument @pg_terminate_backend@. The pid is the victim's
-- 'Pqi.backendPID'.
terminateBackend :: ByteString -> Int32 -> IO ()
terminateBackend conninfo pid =
  bracket (Pqi.connectdb Reference.adapter conninfo) Pqi.finish \connection -> do
    _ <- Pqi.exec connection ("select pg_terminate_backend(" <> pidText <> ")")
    awaitGone connection (500 :: Int)
  where
    pidText = ByteString.Char8.pack (show pid)
    awaitGone connection attemptsLeft = do
      remaining <-
        Pqi.exec connection ("select count(*) from pg_stat_activity where pid = " <> pidText) >>= \case
          Nothing -> ioError (userError "terminateBackend: pg_stat_activity query returned no result")
          Just result -> Pqi.getvalue result 0 0
      case remaining of
        Just "0" -> pure ()
        _
          | attemptsLeft <= 0 -> ioError (userError ("terminateBackend: backend " <> show pid <> " is still alive"))
          | otherwise -> threadDelay 10_000 >> awaitGone connection (attemptsLeft - 1)

-- | The host and port of the database in a @keyword=value@ conninfo. The
-- last occurrence of a keyword wins, as in libpq.
upstreamAddress :: ByteString -> (String, String)
upstreamAddress conninfo =
  (keyword "host" "localhost", keyword "port" "5432")
  where
    assignments =
      [ (key, drop 1 value)
      | token <- words (ByteString.Char8.unpack conninfo),
        let (key, value) = break (== '=') token
      ]
    keyword key fallback = fromMaybe fallback (lookup key (reverse assignments))

listenOnLoopback :: IO Socket.Socket
listenOnLoopback = do
  address : _ <-
    Socket.getAddrInfo
      (Just Socket.defaultHints {Socket.addrSocketType = Socket.Stream})
      (Just "127.0.0.1")
      (Just "0")
  bracketOnError (Socket.socket (Socket.addrFamily address) (Socket.addrSocketType address) (Socket.addrProtocol address)) Socket.close \sock -> do
    Socket.bind sock (Socket.addrAddress address)
    Socket.listen sock 1
    pure sock

-- | Connect to the first of the host's addresses that accepts.
connectUpstream :: String -> String -> IO Socket.Socket
connectUpstream host port = do
  addresses :: [Socket.AddrInfo] <-
    Socket.getAddrInfo
      (Just Socket.defaultHints {Socket.addrSocketType = Socket.Stream})
      (Just host)
      (Just port)
  foldr
    (\address next -> attempt address `catch` \(_ :: IOException) -> next)
    (ioError (userError ("could not connect to " <> host <> ":" <> port)))
    addresses
  where
    attempt address =
      bracketOnError (Socket.socket (Socket.addrFamily address) (Socket.addrSocketType address) (Socket.addrProtocol address)) Socket.close \sock -> do
        Socket.connect sock (Socket.addrAddress address)
        pure sock

-- | Run an action for its effect, ignoring any exception: the sockets here
-- are being shut down from several sides at once, and a socket that is
-- already gone is the expected outcome rather than an error.
quietly :: IO () -> IO ()
quietly action = void (try @SomeException action)
