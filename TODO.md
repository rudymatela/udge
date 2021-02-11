TODO list for Udge
==================

* add diagram of how Udge works

* test that all programs are described in the README

* on `dia2svg` also "pop'y" license to metadata fields.

* install README and diagrams somewhere in the system

* fix ownership of `YYYYMMDD-HHMMDD` folders and `problem.lang` files
  in the `results/` folder.
  They should be owned by `udge.udge` instead of `www-data.udge`.
  Recreate instead of moving?

* add `udge-rejudge` script

* remove the requirement of setting most options on udgerc

* update the README video

* configure rate limiting on nginx
  https://www.nginx.com/blog/rate-limiting-nginx/


For later
---------

* Let users configure if they would be appear anonymized on the ranking and
  have a private user page.  (All this as a single option.)
  The private user page would be accessed on `/p/<user_sha1_base64_etc>`

* Add a user's submission page?  `u/<user_name>/submissions`

* Languages to support later (in no particular order):

	- R
	- Perl
	- PHP

* On the user page, only show problems with tries (for now, keep as it is)
