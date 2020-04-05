TODO list for Udge
==================

* avoid generating Haskell temporary files (`*.hi`, `*.o`) on the problem folder

* double-check that the initial goals are completed

* submissions are overwritten if they are created withing the same second for the same user, rate limit to fix this

* allow parsing of problem name and language from file name (instead of fields)

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
