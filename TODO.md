TODO list for Udge
==================

* submissions are overwritten if they are created withing the same second for the same user, rate limit to fix this

* allow parsing of problem name and language from file name (instead of fields)

* make hello be tolerant "HellO World", "hello world" and "hello World!"
  should be acceptable for a partial score of 2/3 or something like that.

Initial Goals
=============

* initial problems:
	- hello
	- sum2

* initial supported languages:
	- C
	- Python
	- Haskell

* compilation types:
	- regular compilation
	- special compilation (when functions are required)

* test types:
	- regular test
	- special test (heLlo WoRlD)


Future
======

* installation script

* development setup script

* sandbox before running submissions

* fix weird edge cases on tests

* remove partial form filling on errors (for simplicity)

* support command line arguments (`args` file aditionally to `in` and `out`)

* support `err` as well (specific error output)

* support running `sum2.clitest` in parallel (`clitest -j1`?)

* separate `compile-and-test` in files:

	```
	bin/compile/c
	bin/compile/python
	bin/compile/lib/c
	bin/compile/lib/python
	...
	bin/test
	```

* ~instead of symlinking test, simply have a file `type` with `lib` as contents
  when we want to activate the lib compiling type.~
  UPDATE: the above is not really needed, I just need to check for `main.lang`

* allow `in` not to exist: `[ -f $dir/in ] || in=/dev/null` or something like
  that

* add `hello` problem alongside `hello-world`.  Hello is more forgiving and you
  get a score out of 6.  `hello-world` you get 0 or 1 (what is currently set as
  `hello`)
