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
