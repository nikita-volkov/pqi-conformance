# v1.0.6.0

## Non-breaking

- Added a `pipelineSync` spec covering a pipelined command whose result gets misattributed to a later, unrelated command after a prior pipeline aborted on a server error: a named-statement prepare discarded by the server after an earlier command in the same pipeline fails, followed by a plain `SELECT` in the next pipeline on the same connection, which must come back `TuplesOk` rather than short-circuiting as `CommandOk`. `pqi-native` 1.0.1.5 fails it, 1.0.1.6 fixes it. Found via `pqi-native` issue #9.

- Added a `connectdb` spec covering a mid-handshake server rejection: a listener that accepts the connection, writes the first three bytes of an `E`rrorResponse frame, then closes before the frame completes - mirroring how a server sheds load with e.g. "sorry, too many clients already". A sound adapter reports `ConnectionBad` with a classified error message, the same way it reports any other rejected handshake, instead of letting the underlying I/O exception escape `connectdb`.

  Unlike most operation specs, this one drives a raw listener rather than the shared PostgreSQL container: the failure is about how an adapter reacts to a truncated read, which a real server only triggers racily (e.g. under `max_connections` pressure). A hand-rolled listener reproduces the exact byte pattern deterministically. Adds a new `network` dependency.

  The spec asserts full equality on both `status` and `errorMessage`, not just `status`: an adapter that swaps in a made-up message instead of reproducing libpq's actual wording would otherwise slip through unnoticed. Getting that message comparison to hold required two adjustments once real libpq's behavior was checked against: the conninfo passes `sslmode=disable`, since libpq negotiates SSL before the startup packet by default and the candidate adapters under test never do, so without it the two sides would be reacting to the truncated bytes at different points in the protocol; and the listener's host is given as a literal IP (`127.0.0.1`) rather than a name, since libpq only parenthesizes a resolved IP in its failure message when it differs from the given host string, which a literal IP never does. Both attempts now also share one listener/port, since the failure message embeds the port number and two independently-bound ephemeral listeners would otherwise never produce an equal message even when every other detail matches.

  `pqi-native` throws an uncaught `IOException` out of `connectdb` here, since only the initial TCP connect was wrapped in an exception handler, not the handshake read that follows it. Found via [`pqi-native` issue #8](https://github.com/nikita-volkov/pqi-native/issues/8), itself found while investigating `hasql` issue #329.

# v1.0.5.0

## Non-breaking

- Added `sendQueryParams` specs covering a parameter list longer than 65535: `libpq` rejects it locally, without writing anything to the socket, both standalone and in the middle of a pipeline where the commands surrounding it must still get dispatched and drained without desync. `pqi-native` 1.0.1.3 fails both, encoding the parameter count as a wrapping 16-bit field instead of validating it, which corrupted the `Bind` message and desynchronized the connection; 1.0.1.4 fixes it. Found via `hasql` issue #326.

# v1.0.4.0

## Non-breaking

- Added a `pipelineSync` spec covering an async interruption landing while the sync's `ReadyForQuery` is the only thing left outstanding. An adapter that defers the interrupt across the wait for a frame, rather than across only the instructions that move the frame into its buffer, has delivery pinned to the instant the frame arrives - the one moment at which it holds a fully-read message it has not yet accounted for. Dropping that particular message costs the caller the connection rather than a result: the adapter still has a sync outstanding, so the next `getResult` goes back to the socket for a message the backend has already sent and will not send again.

  Unlike the existing interruption spec, which sweeps a timer across the window, this one constructs it. A flush request separates the command's results from the sync's `ReadyForQuery`, and a `DEFERRABLE INITIALLY DEFERRED` trigger - fired by the backend's handling of `Sync` itself - holds that `ReadyForQuery` back for half a second, so the interrupt lands with 450ms of margin either side.

  `pqi-native` 1.0.1.2 fails it, 1.0.1.3 passes it. Found via a hang in `hasql`'s `Integration.Sharing.Connection.Use.PipelineAbortedInterruptionCleanup`

# v1.0.3.0

## Non-breaking

- Added a `pipelineSync` spec covering resilience to an asynchronous interruption around a pipeline abort. Found the native adapter can leave a connection permanently unusable instead of failing outright, since none of its transport/connection code is exception-safe

# v1.0.2.1

## Fixes

- Fix a flaky test

# v1.0.2.0

## Non-breaking

- Added a differential spec for `sendQueryParams` in pipeline mode, catching that the native adapter charges its own `ParseComplete` against a pending `sendPrepare` and so emits a spurious leading `CommandOk`, shifting every later result

# v1.0.1.0

## Non-breaking

- Picked up `pqi` 1.1.0.0, which renamed `Notify`'s fields to `notifyRelname`/`notifyBePid`/`notifyExtra` to match `postgresql-libpq`

# v1.0.0.1

Doc corrections.

# v1.0.0.0

## Non-breaking

- Differential spec for the new `resStatus` field of `Pqi.Adapter`

# v0.1.2.2

## Fixes

- Fixed the reference adapter's `ntuples` and `nfields` intermittently returning garbage, which made the differential suite fail correct candidates. `postgresql-libpq` imports `PQntuples` and `PQnfields` as pure functions and lets the resulting thunk escape `withForeignPtr`, so if the result handle got collected before the count was forced, the number was read from memory that `PQclear` had already freed. Both counts are now forced while the handle is still alive.

# v0.1.2.1

- Fixed the publishing.

# v0.1.2.0

- Added another test.

# v0.1.1.1

- Fixed the docs.

# v0.1.1.0

## Non-breaking

- Added differential coverage for extra conninfo params (e.g. `application_name`) supplied at connect time, closing the gap where only `SET`-based updates were covered

# v0.1.0.1

## Fixes

- Removed remaining `OverloadedRecordDot` usages from the source, since that extension (along with `DuplicateRecordFields` and `NoFieldSelectors`) was dropped from the default extensions to support GHC 8.10

# v0.1.0.0

## Breaking

- Migrated to record-of-functions API structure

## Non-breaking

- Added parity specification for pipeline sync operations
- Added UnescapeBytea to the conformance test battery
