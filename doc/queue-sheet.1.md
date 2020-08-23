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

Queue Sheet is a utility that builds PDFs of lists.  Printed PDFs can be used
to track progress through queues when offline.

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
:   queue file

# FILES

## `QUEUES.yaml`

A queue is a named list of items.  For example, a podcast can be represented
as a queue where the queue name is the name of the podcast and each item in
the queue is an episode of the podcast.  Queues can optionally be organized
into sections.  For example, sections can be used to organize podcast queues
by theme.

Queues are specified in YAML format.  They may be specified in a few different
ways, depending on how you want to organize them.

To create a queue sheet of queues without sections, the YAML file consists of
a list of queue objects, which have the following properties:

*name*
:   queue name (string, required)

*url*
:   queue URL (string, optional)

*date*
:   date of last update (string, optional)

*tags*
:   list of tags (list of string, optional)

*prev*
:   previous item (item, optional)

*next*
:   list of next items (list of items, optional)

The only required property is *name*.

The *tags* property is a list of string tags that are associated with the
queue.  A tag must consist of at least one ASCII letter, number, period,
underscore, or dash.  For example, tag "complete" can be used to indicate that
there will be no new episodes of a podcast that is complete.

The *next* property is a list of next items in the queue.  When the list is
exhausted, the previous item can be specified using the *prev* property.  If
both *prev* and *next* are specified, *prev* is ignored.

Items can be specified by name only, using a string or a number.  To associate
a URL with an item, use an object with the following properties:

*name*
:   name of the item (string, required)

*url*
:   item URL (string, optional)

To organize queues into sections, the YAML file should be written as an object
with two properties:

*sections*
:   list of sections names (optional)

*queues*
:   list of queue objects (required)

Sections names are specified using strings.  The order that the sections are
listed determines the order that they are displayed on the queue sheet.

Queue objects are as above, with an additional property to specify the
section:

*section*
:   name of the section (string, optional)

Queues that are not explicitly associated with a section are associated with
an implicit default section.

To make it easier to share queue files, imports are also supported.  Import
another queue file using an import object instead of a queue object in a list
of queues.  An import object has the following properties:

*import*
:   path to the queue file to import (string, required)

*section*
:   section to associate all of the imported queues with (string, optional)

Paths are relative to the current file.  For example, simply specify the
filename of the file to import when the file is in the same directory.

When you specify *section*, the section must be defined in the current file.
When you do not specify *section*, the sections of the imported queues are
used, but they must also be defined in the current file.

## Template

YAML files specify the data, and templates determine how that data is
displayed.  A LaTeX template is used to build the PDF, using XeTeX.  Unless
specified otherwise, `template.tex` is used.

It is a Jinja2-style template using the following syntax:

Interpolations
:   `<< section.name >>`

Tags
:   `<! if section.name !>`

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

Only sections that contain queues are passed to the template.  The *name*
property of the default section is empty.

A queue is an object with the following properties:

*name*
:   name of the queue (string)

*url*
:   queue URL or empty string if no URL (string)

*date*
:   date or empty string if no date (string)

*prev_item*
:   previous item or empty string if not set (item)

*next_items*
:   list of next items (list of items)

Queue tags are exposed as boolean properties prefixed with "tag_".  For
example, a tag named "complete" is exposed as "tag_complete".

An item is an object with the following properties:

*name*
:   name of the item (string)

*url*
:   item URL or empty string if no URL (string)

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
