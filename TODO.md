TODO list for Udge
==================

* create `/run/udge` automatically upon boot somehow
  perhaps use `/etc/tmpfiles.d`

* make sure `/var/run/udge/1/.ssh/authorized_keys` is neither owned or writable
  by `udge-1`.  An `udge-?` user should not be able to change its
  `authorized_keys`.  The folder shouldn't be movable as well.

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
