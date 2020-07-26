TODO list for Udge
==================

* `udge-passwd` command, to reset the password of a user

* compute and record runtime and memory

* automatically install crontab entries

* sandbox compilation
	- access a separate user through ssh to compile?
	- udge-c: to compile in C
	- udge-py: to "compile" in Python
	- udge-hs: to compile in Haskell
	- udge-java: to compile Java
	- ...
	- advantage: separating the web and runner servers becomes trivial
	- advantage: ^ easy to put up an extra iptables restriction on outgoing connections
	- advantage: easy to terminate in case of hanging processes
	- disadvantage: a little processing overhead

* configure rate limiting on nginx
  https://www.nginx.com/blog/rate-limiting-nginx/

* use flock for a proper hanling of locks (as scripts are still succeptible to
  race conditions currently).  The affected scripts are:

	- `udge-pick-and-judge`.

	- use `lockfile -r0` from procmail.

* User page changes

	- for each problem, add extra column showing the best scores for Python, C,
	  Haskell and the last submitted language

	- for each problem, add extra column showing the best scores for the last
	  submitted languages

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
