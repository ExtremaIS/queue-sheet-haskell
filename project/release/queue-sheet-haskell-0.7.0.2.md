# `queue-sheet-haskell` `0.7.0.2` Release Notes

Date
: 2022-03-02

## Overview

Queue Sheet is a utility that builds PDFs of lists.  Printed PDFs can be used
to track progress when offline.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/queue-sheet-haskell#readme>

## This Release

This is a patch release that makes updates to the package infrastructure.  The
package is now published to [Hackage][] and [Stackage][], and dependency
version upper bounds have been bumped.  There are no changes to the API or
CLI.

[Hackage]: <https://hackage.haskell.org/package/queue-sheet>
[Stackage]: <https://stackage.org/package/queue-sheet>

### Dependency Versions

The following dependency version upper bounds have been bumped to support the
latest versions.

* [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative)
* [`text`](https://hackage.haskell.org/package/text)

### Compatibility

Queue Sheet is currently tested with [GHC 8.2.2][] through [GHC 9.2.1][].  The
`.cabal` file uses Cabal version 1.24 (included with GHC 8.2.2), so it should
build fine on relatively old Haskell installations as well as current
installations.

[GHC 8.2.2]: <https://www.haskell.org/ghc/download_ghc_8_2_2.html>
[GHC 9.2.1]: <https://www.haskell.org/ghc/download_ghc_9_2_1.html>

### Issues

There are no known issues at this time.
