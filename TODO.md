TODO list for Udge
==================

* installation script

* add `results/rank` with a plaintext rank, and use it instead when generating the html

* validation:

	- `cgi-bin/new-user`: minimum password length and complexity

Future
------

* sandbox before running submissions:
	- protect disk access (chroot ?)
	  (use `ldd file` to find out what to copy from `/lib/`)
	- protect from disk usage DOSs (new user + disk quota + ulimit ?)
	- protect from programs that catch SIGTERM
	- protect from dangling child processes with different names

* support command line arguments (`args` file aditionally to `in` and `out`)

* support `err` as well (specific error output)

* use flock for a proper hanling of locks (as scripts are still succeptible to
  race conditions currently).  The affected scripts are:

	- `udge-pick-and-judge`;
	- `udge-update-all-users-html`;
	- `udge-update-rank-html`.

* add instructions on how to set up an `udge` user

* run stuff as `nobody` where applicable

* `cgi-bin/submit`: allow parsing of problem name and language from file name (instead of fields)

* Test what happens if I submit a 128MB file and a 1GB file.
  Can we block that on nginx itself?
