TODO list for Udge
==================

* `cat` problem:

	- proper description
	- test case involving `-`
	- example `cat` with wrong error output (e.g.: missing)
	- examples passing and not passing each of the test cases

* automatically install crontab entries

* sandbox before running submissions:
	- protect disk access (chroot ?)
	  (use `ldd file` to find out what to copy from `/lib/`)
	- protect from disk usage DOSs (new user + disk quota + ulimit ?)
	- protect from programs that catch SIGTERM
	- protect from dangling child processes with different names

* sandbox compilation

* use flock for a proper hanling of locks (as scripts are still succeptible to
  race conditions currently).  The affected scripts are:

	- `udge-pick-and-judge`;
	- `udge-update-all-user-htmls`;
	- `udge-update-rank-html`.

* `cgi-bin/submit`: allow parsing of problem name and language from file name (instead of fields)

* configure rate limiting on nginx
  https://www.nginx.com/blog/rate-limiting-nginx/

* Let users configure if they would be appear anonymized on the ranking and
  have a private user page.  (All this as a single option.)
  The private user page would be accessed on `/p/<user_sha1_base64_etc>`

* Add a user's submission page?  `u/<user_name>/submissions`

* Languages to support later (in no particular order):

	- Lua
	- Ruby
	- R
	- Perl
	- PHP
