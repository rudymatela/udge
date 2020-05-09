TODO list for Udge
==================

* add installation instructions
	- including instructions on how to set up an `udge` user
	- update installation script accordingly

* review Makefile variables: consider not using some of them
  (e.g.: BINS)

* document why permissions are set the way they are in the README

* document current sandboxing limitations on README

* add `addition` problem which is
  to `add` as `hello-world` is to `hello`

* review docs and notices of each file (examples, etc.)

* publish on github?


Future
------

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

* run stuff as `nobody` where applicable

* `cgi-bin/submit`: allow parsing of problem name and language from file name (instead of fields)

* review `mcat` uses (split into two different types?)

* Test what happens if I submit a 128MB file and a 1GB file.
  Can we block that on nginx itself?

* configure rate limiting on nginx
  https://www.nginx.com/blog/rate-limiting-nginx/

* add support to names
	- including tests with characters of a few different languages
