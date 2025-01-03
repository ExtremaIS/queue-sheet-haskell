# `queue-sheet-haskell` `0.8.0.0` Release Notes

Date
: 2025-01-03

## Overview

Queue Sheet is a utility that builds PDFs of lists.  Printed PDFs can be used
to track progress when offline.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/queue-sheet-haskell#readme>

## This Release

This release adds compatibility with the latest releases of GHC and removes
support for versions of GHC that were released more than five years ago.  GHC
versions 8.8.4 through 9.10.1 are supported.  Cabal version 3.0 through
3.12.1.0 are supported.

There is one change to the code: tags and items are now separated by
whitespace, instead of commas.  This allows YAML folded block scalars to be
used, to avoid long lines.

### Compatibility

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - queue-sheet-0.8.0.0

### Issues

There are no known issues at this time.
