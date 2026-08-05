# v0.1.1.1

Fix the docs

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
