# `queue-sheet-haskell` `0.3.0.0` Release Notes

Date
: 2020-08-23

## Overview

This release of Queue Sheet introduces a number of new features:

* Queues not explicitly associated with a section are implicitly associated
  with a default section.  This provides a convenient way to create queue
  sheets without sections.  One can specify the queue list as a top-level
  array to simplify queue files in this case.

* Queues from other queue files can be imported, either inheriting sections
  or overriding them.  This allows users to easily create a custom queue sheet
  that includes queues defined in separate files.

* User-defined tags can now be associated with queues.  They can be handled in
  any way that the user wants in the template.

See the updated man page and examples for details.

## Migration

This release includes a number of breaking changes, in both queue files and
templates.  Any existing files must be migrated to use the new version.

### Queue Files

The `split` property of queues is removed.  Use a `split` tag instead.

### Templates

Tags are now exposed in templates with a `tag_` prefix.  When using a
`complete` tag, `isComplete` should be changed to `tag_complete` in templates.
When using a `partial` tag, `isPartial` should be changed to `tag_partial` in
templates.  When using a `split` tag, `isSplit` should be changed to
`tag_split` in templates.

For consistency, all template properties now use snake case.  `prevItem`
should be changed to `prev_item`, and `nextItems` should be changed to
`next_items` in templates.
