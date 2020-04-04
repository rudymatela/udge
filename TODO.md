TODO list for Udge
==================

* rename sum2 to add

* rename `hello` to `hello-world`

* add `hello` problem alongside `hello-world`.  Hello is more forgiving and you
  get a score out of 6.  `hello-world` you get 0 or 1 (what is currently set as
  `hello`)

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
