TODO list for Udge
==================

* add `udge-update-all-problem-htmls`

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

* support command line arguments (`args` file aditionally to `in` and `out`)

* support `err` as well (specific error output)

* add `results/rank` with a plaintext rank, and use it instead when generating the html

* add generic handling of scores:

	- on `etc/udge/config`

		```
		SCORING=udge-type-1
		```

	- on `udge-user-stats <user> short`, use the variable value instead of the
	  custom scoring

* use flock for a proper hanling of locks (as scripts are still succeptible to
  race conditions currently)
