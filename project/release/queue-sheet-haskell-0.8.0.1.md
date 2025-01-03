# `queue-sheet-haskell` `0.8.0.1` Release Notes

Date
: 2025-01-04

## Overview

Queue Sheet is a utility that builds PDFs of lists.  Printed PDFs can be used
to track progress when offline.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/queue-sheet-haskell#readme>

## This Release

This patch release just changes the default value of the
`optparse-applicative_ge_0_18` flag, which is used to select which
dependencies are required to build the project depending on the version of
`optparse-applicative` that is used.  Since Cabal determines the value of this
flag automatically, there are no changes when using Cabal.  When using Stack,
one now needs to set the flag to `False` when building with old dependencies.

There are no changes to the code in this release.

### Compatibility

GHC versions 8.8.4 through 9.10.1 are supported.  Cabal version 3.0 through
3.12.1.0 are supported.

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - queue-sheet-0.8.0.0

### Issues

There are no known issues at this time.
