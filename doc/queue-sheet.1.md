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

-t, \--template *TEMPLATE.tex*
:   template file (default: template.tex)

-o, \--output *QUEUES.pdf*
:   output file

# ARGUMENTS

*QUEUES.yaml*
:   YAML file specifying queue information

# FILES

## `QUEUES.yaml`

This file is an object with two properties.

The *sections* property is a list of section names (strings).  Note that the
order determines the order in the output.

The *queues* property is a list of queue objects with the following
properties:

*name*
:   name of the queue (string, required)

*section*
:   name of the section (string, required)

*split*
:   *true* to display items on separate lines (boolean, default *false*)

*tags*
:   list of tags (list of string, optional)

*date*
:   date of last update (string, optional)

*prev*
:   previous (complete) item (string, optional)

*next*
:   list of next items (list of string, optional)

If both *prev* and *next* are specified, then *prev* is ignored.

The following tags are supported:

*complete*
:   no new items will be added to the queue

*partial*
:   not all items of the source queue are added to the queue

## Template

A LaTeX template is used to build the PDF, using XeTeX.  Unless specified
otherwise, `template.tex` is used.

It is a Jinja2-style template using the following syntax:

Interpolations
:   `<< section.name >>`

Tags
:   `<! if queue.isSplit !>`

Comments
:   `<# comment #>`

The context contains a single value:

*sections*
:   list of sections

A section is an object with the following properties:

*name*
:   name of the section (string)

*queues*
:   list of queues

A queue is an object with the following properties:

*name*
:   name of the queue (string)

*isSplit*
:   *true* to display items on separate lines (boolean)

*isPartial*
:   *true* if the partial tag is set (boolean)

*isComplete*
:   *true* if the complete tag is set (boolean)

*date*
:   date or empty string if no date (string)

*prevItem*
:   previous item or empty string if not set (string)

*nextItems*
:   list of next items (list of strings)

## `QUEUES.pdf`

Unless specified otherwise, the built PDF is output to a file with the same
base name as the queues file but with a `.pdf` extension.

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
