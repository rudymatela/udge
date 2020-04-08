TODO list for Udge
==================

* add `bin/user-html` that generate user pages
  (using `latest-results-html`)

* submissions are overwritten if they are created withing the same second for the same user, rate limit to fix this

* allow parsing of problem name and language from file name (instead of fields)

* organize scripts, perhaps:

	- `bin/utils/bash`: errxit() and the like
	- `bin/utils/html`: html-header(), the like and all of the above
	- `bin/utils/cgi`: http-status(), the like and all of the above
	- move `cgi-create-data-files` to just `bin/`

* make so that I don't need to use `index.html` with nginx.  `/path/to/page`
  should display the HTML given in `/path/to/page.html`

Future
======

* installation script

* development setup script

* sandbox before running submissions

* fix weird edge cases on tests

* remove partial form filling on errors (for simplicity)

* support command line arguments (`args` file aditionally to `in` and `out`)

* support `err` as well (specific error output)

* reuse the generated binary to avoid double compilation.  Maybe name `exe` and
  `exe-lib` for when we are using the `lib` type of compilation.
