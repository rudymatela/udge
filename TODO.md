TODO list for Udge
==================

* remove partial form filling on `cgi-bin/udge-new-user`
  (and see how it looks and feels)

* add `bin/udge-add-user` (non-CGI)

* add `bin/udge-submit` (non-CGI)

* add crontab entries:
	- every minute `pick-and-judge`
	- every minute `udge-update-all-users-html`
	- every minute `udge-update-rank-html`
	- create lockfiles where necessary to avoid multiple instances
	  as cron still triggers the executable even if it is already running

* submissions are overwritten if they are created within the same second for the same user, rate limit to fix this

* allow parsing of problem name and language from file name (instead of fields)

Future
======

* installation script

* sandbox before running submissions

* fix weird edge cases on tests

* remove partial form filling on errors (for simplicity)

* support command line arguments (`args` file aditionally to `in` and `out`)

* support `err` as well (specific error output)

* add `results/rank` with a plaintext rank, and use it instead when generating the html
