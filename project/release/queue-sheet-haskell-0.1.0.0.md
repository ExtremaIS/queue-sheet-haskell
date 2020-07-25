# `queue-sheet-haskell` `0.1.0.0` Release Notes

Date
: 2020-07-25

## Overview

This release of `queue-sheet` keeps the same functionality but changes the
input file format.  In the initial release, sections and queues were defined
in separate YAML files, but now a single YAML file is used to define both.
This enables you to manage multiple queue sheets with different sections
within a single directory.

When building a queue sheet, you can specify the template to use with the
`--template` command-line option.  The default is `template.tex`, which is
backwards-compatible with the initial release.  You can also specify the
name of the built PDF with the `--output` command-line option.  The default
is the base name of the queues file but with a `.pdf` extension, which is
backwards-compatible with the initial release.

## Migration

It is easy to migrate existing queues configuration.  Simple merge the
`sections.yaml` file into the queues file, listing the sections in a
`sections` property and queues in a `queues` property.

Example:

```
---

sections:
  - Functional
  - Programming
  - Other

queues:

  - name: AI Podcast
    section: Other
    date: 2020-01-17
    prev: 72
```
