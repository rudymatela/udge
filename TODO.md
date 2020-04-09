TODO list for Udge
==================

* move `bin/compile` to `lib/udge/compile`

* move `bin/utils/*` to `lib/udge/*`

* move `bin/markdown` to `lib/udge/markdown`

* move `bin/*-html` to `lib/udge/*-html`

* rename `bin/*` to `bin/udge-*`

* localize `compile-and-test` inside `judge`

* add `update-rank-html` script

* add `update-user-html` script

* add `update-users-html` script

* submissions are overwritten if they are created within the same second for the same user, rate limit to fix this

* allow parsing of problem name and language from file name (instead of fields)

Future
======

* installation script

* development setup script

* sandbox before running submissions

* fix weird edge cases on tests

* remove partial form filling on errors (for simplicity)

* support command line arguments (`args` file aditionally to `in` and `out`)

* support `err` as well (specific error output)
