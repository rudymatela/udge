TODO list for Udge
==================

* fix the following potential race condition:

	$ make test-web-parallel test-happy -j7
	...
	#13	udge-latest-results $user1 | sed -e "s/^[0-9-]* [0-9:]* /YYYY-MM-DD HH:MM:SS /g"
	------------------------------------------------------------------------------------------------------
	[FAILED #13, line 50] udge-latest-results $user1 | sed -e "s/^[0-9-]* [0-9:]* /YYYY-MM-DD HH:MM:SS /g"
	@@ -1,2 +1,2 @@
	-YYYY-MM-DD HH:MM:SS  add.c  4/6  0ms  1MB  No - wrong output
	-YYYY-MM-DD HH:MM:SS  add.c  6/6  0ms  1MB
	+YYYY-MM-DD HH:MM:SS  add.c  1/6  -  -  No - runtime error
	+YYYY-MM-DD HH:MM:SS  add.c  3/6  -  -  3/6
	------------------------------------------------------------------------------------------------------
	make: *** [Makefile:152: happy-day-3.clitest] Error 1
	make: *** Waiting for unfinished jobs....

	The 3/6 shown above in place of the reason makes this seems that udge-check
	is reading from an unfinished folder.  What happened in this case?

	Here's further info after implementing `ferrxit`:

	[FAILED #13, line 50] udge-latest-results $user1 | sed -e "s/^[0-9-]* [0-9:]* /YYYY-MM-DD HH:MM:SS /g"
	@@ -1,2 +1,2 @@
	 YYYY-MM-DD HH:MM:SS  add.c  4/6  0ms  1MB  No - wrong output
	-YYYY-MM-DD HH:MM:SS  add.c  6/6  0ms  1MB
	+YYYY-MM-DD HH:MM:SS  add.c  1/6    -    -  output folder not found


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

* On the user page, only show problems with tries (for now, keep as it is)
