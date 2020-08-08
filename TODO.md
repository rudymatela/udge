TODO list for Udge
==================

* configure rate limiting on nginx
  https://www.nginx.com/blog/rate-limiting-nginx/

* User page changes

	- problems appearing on a specific order

	- problems separated by chapter/volume

	- only show problems with tries


For later
---------

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

* allow parallel execution of `make test-web` somehow

	- problem: we will not be able to test that solutions get "queued",
	           just that they are ultimately judged.
