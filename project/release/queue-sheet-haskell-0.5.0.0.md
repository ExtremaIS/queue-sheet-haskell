# `queue-sheet-haskell` `0.5.0.0` Release Notes

Date
: 2020-11-08

## Overview

This release of Queue Sheet adds support for specifying lists of tags or items
as a string in a simplified CSV format.  Use of the CSV format can result in
fewer changes when updating a file, making the update slightly easier to
perform and the `git diff` output easier to read.  See the updated man page
for details.

This release also switches the Git default branch to `main`.

YAML files used with version `0.4.0.0` work with version `0.5.0.0` without
change, but YAML files that the new CSV format will not work with version
`0.4.0.0` or before.
