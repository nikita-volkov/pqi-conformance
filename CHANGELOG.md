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
