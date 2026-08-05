-- | Regression coverage for a hang discovered via @hasql-pool@'s
-- @Specs.Use@ integration suite: sending a command on a connection after
-- 'Pqi.finish' has closed it could block forever on the native adapter
-- instead of failing promptly.
--
-- Root cause: 'Pqi.finish' closed the transport socket but never updated the
-- connection's status away from @ConnectionOk@. Nothing downstream of
-- 'Pqi.finish' short-circuited on a closed connection, so later protocol
-- operations kept attempting to use the closed socket's file descriptor. The
-- first such send after close typically failed fast with "Bad file
-- descriptor", but in a process doing other concurrent socket I\/O (exactly
-- the situation in a real test suite or application — e.g. testcontainers
-- and a connection pool's own background threads keep other sockets open),
-- that closed file descriptor number could already have been reused for an
-- unrelated socket by the time of a second attempt. Writing to the (silently
-- reused) fd then blocked waiting for buffer space that a completely
-- unrelated peer would never free — which is exactly what @hasql@'s
-- @cleanUpAfterInterruption@ flow does after a first send fails: it always
-- re-attempts a further write to deallocate prepared statements.
--
-- This mirrors hasql-pool's @Sessions.closeConn@ helper, which deliberately
-- calls 'Pqi.finish' on a connection and then keeps using it (to test that
-- the pool notices and evicts it) — a supported use of the low-level
-- @onLibpqConnection@ escape hatch, and the code path in which the hang was
-- first observed (hasql-pool's @Specs.UseSpec@, "Connection errors cause
-- eviction of connection").
--
-- The primary check here is deterministic and does not depend on winning
-- any race: 'Pqi.status' must report 'Pqi.ConnectionBad' once 'Pqi.finish'
-- has run, so that the @ConnectionOk@ guards already present in the query
-- functions (see @Pqi.Native.Query.sendAsync@, @withReady@) reject any
-- further operation outright instead of touching the closed socket at all.
-- A secondary, best-effort check replays a send immediately after finish
-- under a timeout, so that if the file-descriptor race above is ever hit
-- again, it fails the suite instead of hanging it.
module Pqi.Conformance.Operation.Finish.PostFinishSend
  ( spec,
  )
where

import Control.Exception (SomeException, try)
import qualified Pqi
import Pqi.Conformance.Prelude
import System.Timeout (timeout)
import Test.Hspec

-- | Generous relative to a healthy roundtrip (milliseconds), but far below
-- what a genuinely hung send would ever recover within.
sendTimeoutMicros :: Int
sendTimeoutMicros = 3_000_000

spec :: Pqi.Adapter -> SpecWith ByteString
spec adapter =
  describe "finish" do
    describe "sending after finish" do
      -- Deliberately not a 'differential' check against the FFI reference:
      -- postgresql-libpq leaves 'Pq.status' looking stale (@ConnectionOk@)
      -- after 'PQfinish' too (querying a freed @PGconn@ is undefined
      -- behaviour in C, not something conformance testing can safely pin
      -- down as a reference). Regardless of what the reference does, a
      -- well-behaved adapter should mark itself unusable so nothing
      -- downstream is tempted to touch the closed socket again.
      it "reports the connection as bad, rather than leaving it looking usable" \conninfo -> do
        connection <- Pqi.connectdb adapter conninfo
        _ <- Pqi.exec connection "select 1"
        Pqi.finish connection
        status <- Pqi.status connection
        status `shouldBe` Pqi.ConnectionBad

      it "does not block indefinitely on a send issued right after finish" \conninfo -> do
        connection <- Pqi.connectdb adapter conninfo
        _ <- Pqi.exec connection "select 1"
        Pqi.finish connection
        result <- timeout sendTimeoutMicros (try @SomeException (Pqi.exec connection "select 1"))
        -- 'Nothing' means the send never completed within the bound — i.e.
        -- it hung. Succeeding or throwing are both acceptable outcomes here;
        -- only hanging is not.
        isJust result `shouldBe` True
