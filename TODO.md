TODO list for Udge
==================

* replace the procmail dependency

* carry on with the implementation of `udge-health`,
  see `TODO:` comments over there

* remove the requirement of setting most options on `/etc/udgerc`

* support multiple sites by multiple udgerc files (`UDGERC=/etc/blahrc`)
	- problem: needs multiple `udge` users, would be tricky to configure


For later
---------

* (help needed) Ubuntu / Debian package

* (help needed) Dockerfile and docker container

* (quasi-stateless) Password reset

		udge.example.com/reset-password
		# Reset your password
		username: ______
		email: ______

		udge.example.com/reset-password/username?t=0123456789ABCDEF
		# Reset your password
		new password: ______
		new password confirmation: ______

* (quasi-stateless) User settings

		(see git logs for an attempt)

		POST udge.example.com/settings
		# User settings
		Username: ______
		Password: ______

		POST udge.example.com/settings
		# User settings for user
		Name: ______
		Email: ______
		Email confirmation: ______
		Password: ______
		To change your username please contact admin@udge.example.com

* do not allow two users with the same email (needs email confirmation)

* cleanup any files created at `tmp` somehow
  `udge-health` needs to be completed first

* Add a user's submission page?  `u/<user_name>/submissions`

* Languages to support later (in no particular order):

	- Perl
	- PHP
	- `https://www.tiobe.com/tiobe-index/`
	- `https://pypl.github.io/`
	- `https://en.wikipedia.org/wiki/Measuring_programming_language_popularity`
	- `https://insights.stackoverflow.com/survey/2021#technology-most-popular-technologies`
	- `https://spectrum.ieee.org/top-programming-languages/`

* Remove `(display "")` hack in Racket example programs and main files

* Find a way to check the syntax of Racket files

* On the user page, only show problems with tries (for now, keep as it is)

* Fix Lua lib compilation (or wait for upstream fix)

	As of 2022-03-16, lua lib compilation is not working on my Arch box with Lua
	5.4.4 and glibc 2.35-2.  I managed to pin the problem down to building with
	multiple arguments:

		$ luac -o exe-as-lib examples/add/add.lua problem/add/6/main.lua
		free(): double free detected in tcache 2
		Aborted (core dumped)

	The following actually works:

		$ cat examples/add/add.lua problem/add/6/main.lua >tmp.lua
		$ luac -o exe-as-lib tmp.lua

	For now, I'll keep compilation as is, if upstream is not fixed I can change
	to the temporary file approach.

	On Ubuntu, the issue seems not to be present.
