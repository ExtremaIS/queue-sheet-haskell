---
title: QUEUE-SHEET
section: 1
hyphenate: false
...

# NAME

`queue-sheet` - queue sheet utility

# SYNOPSIS

`queue-sheet` [*OPTIONS*] QUEUES.yaml

# DESCRIPTION

Queue Sheet is a utility that builds PDFs of lists.  The printed PDFs can be
used to track progress through the queues when offline.

# OPTIONS

-h, \--help
:   show help and exit

\--version
:   show version and exit

# ARGUMENTS

*QUEUES.yaml*
:   YAML file specifying queue information

# FILES

## `sections.yaml`

This file defines the names and order of sections.  It is a list of section
names (strings).

## `QUEUES.yaml`

This file defines the queues.  It is a list of queue objects, which have the
following properties:

name
:   name of the queue (string, required)

section
:   name of the section (string, required)

split
:   *true* to display items on separate lines (boolean, default *false*)

tags
:   list of tags (list of string, optional)

date
:   date of last update (string, optional)

prev
:   previous (complete) item (string, optional)

next
:   list of next items (list of string, optional)

If both *prev* and *next* are specified, then *prev* is ignored.

The following tags are supported:

complete
:   no new items will be added to the queue

partial
:   not all items of the source queue are added to the queue

## `template.tex`

This file is the LaTeX template used to build the PDF, with XeTeX.

It is a Jinja2-style template using the following syntax:

Interpolations
:   `<< section.name >>`

Tags
:   `<! if queue.isSplit !>`

Comments
:   `<# comment #>`

The context contains a single value:

sections
:   list of sections

A section is an object with the following properties:

name
:   name of the section (string)

queues
:   list of queues

A queue is an object with the following properties:

name
:   name of the queue (string)

isSplit
:   *true* to display items on separate lines (boolean)

isPartial
:   *true* if the partial tag is set (boolean)

isComplete
:   *true* if the complete tag is set (boolean)

date
:   date or empty string if no date (string)

prevItem
:   previous item or empty string if not set (string)

nextItems
:   list of next items (list of strings)

## `QUEUES.pdf`

The output is a file with the same base name as the queues file, but with a
`.pdf` extension.

# EXIT CODES

0
:   no error

1
:   execution error

2
:   command line error

# PROJECT

GitHub:
:   <https://github.com/ExtremaIS/queue-sheet-haskell>

Reporting issues:
:   GitHub: <https://github.com/ExtremaIS/queue-sheet-haskell/issues>

    Email: <bugs@extrema.is>

Copyright
:   Copyright (c) 2020 Travis Cardwell

License
:   The MIT License <https://opensource.org/licenses/MIT>
