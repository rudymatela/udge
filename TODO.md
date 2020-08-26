TODO list for Udge
==================

* remove self-closing `<tags />`

* remove double-slashes from "-v" and error output:

	+diff: /var/lib/udge/problem/<problem>//out: No such file or directory
	+No - wrong output
	+0/1

  i.e.: `ls-problems` should avoid producing output with `/`

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
