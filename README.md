# Queue Sheet

[![GitHub CI](https://github.com/ExtremaIS/queue-sheet-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/queue-sheet-haskell/actions)

* [Overview](#overview)
* [Requirements](#requirements)
* [Installation](#installation)
    * [Installation From Source](#installation-from-source)
* [Usage](#usage)
* [Project](#project)
    * [Links](#links)
    * [Releases](#releases)
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

## Requirements

Queue Sheet has only been tested on Linux.  It *might* work on Windows and
macOS.

Queue Sheet uses [XeTeX](https://tug.org/xetex/) to build PDFs.  It is usually
installed as part of [TeX Live](https://www.tug.org/texlive/).  The LaTeX
packages used depend entirely on the contents of the template.

## Installation

### Installation From Source

Queue Sheet can be built from source using
[Stack](https://www.haskellstack.org).  For example, you can install the
latest release (to `~/.local/bin` on Linux) as follows:

```
$ git clone https://github.com/ExtremaIS/queue-sheet-haskell.git
$ cd queue-sheet-haskell
$ stack install
```

## Usage

See the [`queue-sheet` man page](doc/queue-sheet.1.md) for usage information.

See the [examples](examples) directory for example queue files, templates, and
built output.

## Project

Queue Sheet was written quickly to solve a particular pain point.  There are
no plans to put the package on Hackage.

### Links

* GitHub: <https://github.com/ExtremaIS/queue-sheet-haskell>

### Releases

All releases are tagged in the `main` branch.  Release tags are signed using
the
[`security@extrema.is` GPG key](http://keys.gnupg.net/pks/lookup?op=vindex&fingerprint=on&search=0x1D484E4B4705FADF).

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/queue-sheet-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the
[MIT License](https://opensource.org/licenses/MIT) as specified in the
[`LICENSE`](LICENSE) file.
