TODO list for Udge
==================

* add `udge-pick` -- use a `/var/lib/udge/slot` directory

* add `udge-compile-and-run`

* add `udge-check`

* reimplement `udge-pick-and-judge` using the above

* on timeout, stop running (if we timeout on set 3, don't run 4, 5 and 6)

* compute and record runtime and memory

* automatically install crontab entries

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
