TODO list for Udge
==================

* add C++ support

* automatically install crontab entries

* rename `desc` to `desc.md`

* validation:

	- `cgi-bin/new-user`: minimum password length and complexity

* sandbox before running submissions:
	- protect disk access (chroot ?)
	  (use `ldd file` to find out what to copy from `/lib/`)
	- protect from disk usage DOSs (new user + disk quota + ulimit ?)
	- protect from programs that catch SIGTERM
	- protect from dangling child processes with different names

* sandbox compilation

* support command line arguments (`args` file aditionally to `in` and `out`)

* support `err` as well (specific error output)

* use flock for a proper hanling of locks (as scripts are still succeptible to
  race conditions currently).  The affected scripts are:

	- `udge-pick-and-judge`;
	- `udge-update-all-user-htmls`;
	- `udge-update-rank-html`.

* `cgi-bin/submit`: allow parsing of problem name and language from file name (instead of fields)

* review `mcat` uses (split into two different types?)

* Test what happens if I submit a 128MB file and a 1GB file.
  Can we block that on nginx itself?

* configure rate limiting on nginx
  https://www.nginx.com/blog/rate-limiting-nginx/

* add support to names
	- including tests with characters of a few different languages

* Let users configure if they would be appear anonymized on the ranking and
  have a private user page.  (All this as a single option.)
  The private user page would be accessed on `/p/<user_sha1_base64_etc>`

* Add a user's submission page?  `u/<user_name>/submissions`

* Add test to assure that there are no broken links (use `wget -r`?)

* Languages to support later (in no particular order):

	- Lua
	- Ruby
	- R
	- Perl
	- PHP
