# AGENTS.md

## What this suite is

A differential battery. Every spec runs the same scenario on a **candidate**
adapter and on the **reference** (a direct `postgresql-libpq` connection, which
is the C `libpq` library itself), both against the same throwaway PostgreSQL
container, then asserts the two observations are equal.

`libpq`'s behaviour is the specification. Where a candidate and the reference
disagree, the candidate is wrong.

Why this suite carries so much weight: `pqi-native` is largely LLM-generated,
so its trustworthiness comes from the proof system that checks its output
rather than from the process that produced it. See
[_Making libpq a choice_](https://nikita-volkov.github.io/pqi-making-libpq-a-choice/)
and [_Hasql v2: the native era_](https://nikita-volkov.github.io/hasql-v2-the-native-era/).

## Policy: every discovered bug is reproduced here

**A behavioural divergence from `libpq` found in any `pqi` adapter is
reproduced here, as a committed spec, before it is fixed in that adapter.**

This holds however obvious the bug looks and wherever it surfaced: a `hasql`
issue, a user report, a failure of this suite, or your own reading of the
adapter's code. The suite is the accumulated record of every divergence ever
found, and that record is the only thing standing between a fixed bug and its
silent return.

A fix that landed in an adapter with no spec here is unfinished work. Write the
spec, then let the adapter repo bump to the version that carries it.

## Adding a reproduction

The suite is organised as one module per API operation under
`Pqi.Conformance.Operation.*`. A scenario with its own setup or its own
conninfo gets a nested submodule below the operation it exercises, for example
`Pqi.Conformance.Operation.Connectdb.UnixSocketUri`. Each module exports a
single `spec`, and the parent module calls it.

1. **Pick a comparable observation.** The scenario must return a value that is
   deterministic and adapter-independent, so that a mismatch means a real
   divergence. When the natural observation fails that bar, move to a
   neighbouring one that meets it. `Connectdb.UnixSocketUri` compares
   `Pqi.host` rather than `Pqi.errorMessage`: both adapters fail to connect
   either way, but their failure text comes from unrelated machinery (DNS
   resolution against a filesystem check) while the parsed host is fully
   determined by the conninfo. The values that are structurally incomparable
   across connections, and how each is handled, are listed in `README.md`.
2. **Write the module.** Build the scenario on `differential` or
   `differentialConnect` from `Pqi.Conformance.Harness`. The module's leading
   haddock states what the scenario covers, what the divergence was and where
   it lived in the candidate, and why this observation was chosen when it is
   not the obvious one.
3. **Register it** in both places: the `import qualified` and the `spec` call
   in the parent module, and `other-modules` in `pqi-conformance.cabal`.
4. **Verify it both ways.** This package is a library with no test suite of its
   own, so run it from the adapter checkouts, whose `cabal.project.local`
   already points here. `cabal test` in `../pqi-native` goes **red** on the
   unfixed candidate, and `cabal test` in `../pqi-ffi` stays **green**. A spec
   that passes against the unfixed candidate reproduces nothing. A spec that
   fails with `pqi-ffi` as the candidate encodes something `libpq` does not
   actually do, so the spec is wrong rather than the adapter.
5. **Release it.** Bump the third version component in
   `pqi-conformance.cabal`, and add a `## Non-breaking` entry to
   `CHANGELOG.md` saying what the spec covers and what it found.
