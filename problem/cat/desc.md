cat -- concatenate files on stdout
----------------------------------

Write a program concatenates files and prints them on standard output.
Your program should be a feature-limited clone of POSIX's [`cat`].

[`cat`]: https://linux.die.net/man/1/cat

Below is an example session with `cat`
(assuming there are two files named `numbers.txt` and `text.txt`).

With an individual file, cat simply prints its content:

	$ cat text.txt
	These are the contents of text.txt.
	$ cat numbers.txt
	123
	456
	789
	000

When there are multiple files, cat prints them in respective order:

	$ cat text.txt numbers.txt text.txt
	These are the contents of text.txt.
	123
	456
	789
	000
	These are the contents of text.txt.

When `-` is passed as argument, `cat` treats it as standard input:

	$ cat text.txt - numbers.txt
	These are the contents of text.txt.
	a
	a
	b
	b
	c
	c
	^D
	123
	456
	789
	000

When a file is not found `cat` prints the following error message on the
standard error output.

	$ cat notfound.txt
	cat: notfound.txt: could not open file

There is _no need_ to support command line options for the purposes of this exercise.
(`-v`, `--version`, `-n`, etc.)
Simply concatenate files and print them on standard output
and, when you see `-`, copy from standard input.


Copyright © 2020-2021  Rudy Matela
This text is available under the CC BY-SA 4.0 license
or (at your option) the GFDL 1.3 license
