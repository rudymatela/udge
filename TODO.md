TODO list for Udge
==================

* rename `check` to `check-stdout`

* add `check-exit-code`

* use `-v` from `timeout` to actually check for timeouts
  instead of using the 124 exit code.

* submissions are overwritten if they are created withing the same second for the same user, rate limit to fix this

* allow parsing of problem name and language from file name (instead of fields)

* make hello be tolerant "HellO World", "hello world" and "hello World!"
  should be acceptable for a partial score of 2/3 or something like that.

Initial Goals
=============

* initial problems:
	- hello
	- add

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

* support running `add.clitest` in parallel (`clitest -j1`?)
