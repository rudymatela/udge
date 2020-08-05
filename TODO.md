TODO list for Udge
==================

* improve process clearing (do it on compile-and-run)

* compute and record runtime and memory

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

* sweep "TODOs" scattered throughout the code

* test leaving junk flat on `/run/1/`

	1. `fopen("/run/1/junk.txt", "w")`
	2. `fopen("/run/1/junk", "w")`


For later
---------

* `touch` problem that requires files to appear

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
