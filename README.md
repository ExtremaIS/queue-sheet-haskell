# Queue Sheet

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/queue-sheet-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/queue-sheet-haskell/actions)
[![Hackage](https://img.shields.io/hackage/v/queue-sheet.svg)](https://hackage.haskell.org/package/queue-sheet)
[![Stackage LTS](https://stackage.org/package/queue-sheet/badge/lts)](https://stackage.org/package/queue-sheet)
[![Stackage Nightly](https://stackage.org/package/queue-sheet/badge/nightly)](https://stackage.org/nightly/package/queue-sheet)

* [Overview](#overview)
* [CLI](#cli)
    * [Requirements](#requirements)
    * [Installation](#installation)
        * [`.deb` Package Installation](#deb-package-installation)
        * [`.rpm` Package Installation](#rpm-package-installation)
        * [Installation From Hackage](#installation-from-hackage)
        * [Installation From Stackage](#installation-from-stackage)
    * [Usage](#usage)
* [Project](#project)
    * [Links](#links)
    * [Tags](#tags)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

Queue Sheet is a utility that builds PDFs of lists.  Printed PDFs can be used
to track progress when offline.

Use Queue Sheet to track:

* podcasts
* research papers
* conference videos
* university lectures

## CLI

### Requirements

Queue Sheet has only been tested on Linux.  It *might* work on Windows and
macOS.

Queue Sheet uses [XeTeX][] to build PDFs.  It is usually installed as part of
[TeX Live][].  The LaTeX packages used depend entirely on the contents of the
template.

[XeTeX]: <https://tug.org/xetex/>
[TeX Live]: <https://www.tug.org/texlive/>

### Installation

#### `.deb` Package Installation

Check the [Releases][] page for `.deb` packages.

[Releases]: <https://github.com/ExtremaIS/queue-sheet-haskell/releases>

#### `.rpm` Package Installation

Check the [Releases][] page for `.rpm` packages.

#### Installation From Hackage

Install Queue Sheet from [Hackage][] using [Cabal][] as follows:

```
$ cabal v2-install queue-sheet
```

[Hackage]: <https://hackage.haskell.org/package/queue-sheet>
[Cabal]: <https://www.haskell.org/cabal/>

#### Installation From Stackage

Install Queue Sheet from [Stackage][] using [Stack][] as follows:

```
$ stack install queue-sheet
```

[Stackage]: <https://www.stackage.org/package/queue-sheet>
[Stack]: <https://haskellstack.org/>

### Usage

See the [`queue-sheet` man page][] for usage information.

See the [examples][] directory for example queue files, templates, and built
output.

[`queue-sheet` man page]: <doc/queue-sheet.1.md>
[examples]: <examples>

## Project

Queue Sheet was written quickly to solve a particular pain point.  There are
no plans to put the package on Hackage.

### Links

* Hackage: <https://hackage.haskell.org/package/queue-sheet>
* Stackage: <https://www.stackage.org/package/queue-sheet>
* GitHub: <https://github.com/ExtremaIS/queue-sheet-haskell>
* GitHub Actions CI: <https://github.com/ExtremaIS/queue-sheet-haskell/actions>

### Tags

All releases are tagged in the `main` branch.  Release tags are signed using
the
[`security@extrema.is` GPG key][].

[`security@extrema.is` GPG key]: <http://keys.gnupg.net/pks/lookup?op=vindex&fingerprint=on&search=0x1D484E4B4705FADF>

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/queue-sheet-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the [MIT License][] as specified in the
[`LICENSE`][] file.

[MIT License]: <https://opensource.org/licenses/MIT>
[`LICENSE`]: <LICENSE>
