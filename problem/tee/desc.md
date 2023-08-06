tee -- read stdin and write to stdout and files
-----------------------------------------------

Write a program that reads from standard input and
and replicates it on standard output and a file.
Your program should be a feature-limited clone of POSIX's [`tee`].

[`tee`]: https://linux.die.net/man/1/tee

The `tee` program is useful for creating log files
while printing output of system [services/daemons]:

	$ daemon | tee log.txt
	... logs ...
	... logs ...
	... logs ...

[services/daemons]: https://en.wikipedia.org/wiki/Daemon_(computing)

Below is an example session with `tee`.

	$ echo 'some message' | tee log.txt
	some message
	$ cat log.txt
	some message

There is _no need_ to support command line options or multiple log files
for the purposes of this exercise.
(`-v`, `--version`, `-c`, etc.)
Expect a single filename argument and
write output to the created file as well as standard output.


Copyright © 2020-2023  Rudy Matela
This text is available under the CC BY-SA 4.0 license
or (at your option) the GFDL 1.3 license
