TODO list for Udge
==================

* add "pipeline.txt" to test all steps in the pipeline
  (file creation, moving and whatnot)

* update the README with the new submission pileline

* update the README video

* review the README

* configure rate limiting on nginx
  https://www.nginx.com/blog/rate-limiting-nginx/


For later
---------

* Let users configure if they would be appear anonymized on the ranking and
  have a private user page.  (All this as a single option.)
  The private user page would be accessed on `/p/<user_sha1_base64_etc>`

* Add a user's submission page?  `u/<user_name>/submissions`

* Languages to support later (in no particular order):

	- Ruby
	- R
	- Perl
	- PHP

* allow parallel execution of `make test-web` somehow

	- problem: we will not be able to test that solutions get "queued",
	           just that they are ultimately judged.

	- the old runtimes:

		test-scripts  -j7    28.23
		test-scripts  -j1   113.28
		test-web     (-j1)   53.69
		fastest              81.92

	- the new runtimes

		make fastest         49.79
		make test           168.34

* On the user page, only show problems with tries (for now, keep as it is)
