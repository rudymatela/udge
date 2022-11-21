TODO list for Udge
==================

* remove python2 support (and references to it)
	- default to python3!

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

* Fix Erlang/escript hackyness of copying the beam file.

* Fix Erlang/escript slowness (cf. `ERL_EXTRA_TIME=3`).

	My model solution for `add` in Erlang takes a whopping 3.4 seconds to
	process test set #4:

		$ /usr/bin/time -f%e escript examples/add/add.erl <problem/add/4/in >/dev/null
		3.40
		$ /usr/bin/time -f%e python examples/add/add.py <problem/add/4/in >/dev/null
		0.07

	For reference, my Python solution takes 0.07 seconds!
	Maybe my solution is not good enough or maybe I am not using `escript` right...

	Compiling Erlang programs seems to make them run even slower, see:

		$ cp examples/hello-world/hello-world.erl helloworld.erl
		$ erlc helloworld.erl
		$ /usr/bin/time -f%e erl -noshell -s helloworld main [] -s init stop
		Hello, World!
		1.12
		$ /usr/bin/time -f%e escript helloworld.erl
		0.15

	Weirdly, the `Hello, World!` message appears almost instantly.
	It seems Erlang takes a second to call `init:stop()` or
	`init:stop()` takes a full second to run.

	Maybe this is not the proper way to run Erlang,
	is it designed to have the VM running all the time
	and programs being called from it?

* Remove `(display "")` hack in Racket example programs and main files

* Find a way to check the syntax of Racket files

* Fix Erlang/escript running under 1MiB `-f` (revert udge-sandbox's increase)

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
