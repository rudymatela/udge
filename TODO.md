TODO list for Udge
==================

* make so that `disk-usage-files.c` is OLE'd

* add `examples/sandbox/stdout-1024.c` example (OK)

* add `examples/sandbox/stdout-1025.c` example (Fail)

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
