TODO list for Udge
==================

* complete the `bin/latest-results-html` script

* redirect to results page after submission

* submissions are overwritten if they are created withing the same second for the same user, rate limit to fix this

* allow parsing of problem name and language from file name (instead of fields)

* organize scripts, perhaps:

	- `bin/utils/bash`: errxit() and the like
	- `bin/utils/html`: html-header(), the like and all of the above
	- `bin/utils/cgi`: http-status(), the like and all of the above
	- move `cgi-create-data-files` to just `bin/`

Future
======

* installation script

* development setup script

* sandbox before running submissions

* fix weird edge cases on tests

* remove partial form filling on errors (for simplicity)

* support command line arguments (`args` file aditionally to `in` and `out`)

* support `err` as well (specific error output)
