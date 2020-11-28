TODO list for Udge
==================

* make users folder permissions less restrictive
  (we just need to forbid reading the email, password and salt files)
  the rest is publicly available information

* make `best` file permissions less restrictive
  as this is public information

* default to Python 3, but see https://legacy.python.org/dev/peps/pep-0394/
	note that on Ubuntu `/usr/bin/python` points to python2.
	Let submitters use python2 explicitly if they want

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
