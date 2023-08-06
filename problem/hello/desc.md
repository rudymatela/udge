hello -- Hello, World!
----------------------

Write a program that prints "Hello, World!" on the standard output device.
Normally, the output device is the screen of a command line session:

	$ ./hello
	Hello, World!

Hint: On some systems (Windows / Linux / OS X), it is possible to redirect the
output of your program to a file, like so:

	$ ./hello >outputfile.txt


### Input and output

No input should be read.

The output should contain a single line with the `Hello, World!` message.
This line should be terminated in a line break.


### Scoring

* 1/6: compiles
* 2/6: prints something
* 3/6: prints "hello world" somehow
* 4/6: exits successfully
* 5/6: punctuation is correct (`,` and `!`)
* 6/6: prints "Hello, World!" exactly


### Hints

1. __Automated judge:__
	Remember that when your program is submitted
	it will not be run by a human
	but instead by an automated judge.
	Instructions should be followed exactly
	or the judge will not give you a full score.
	Watch out for correct punctuation and the required line break.

1. __Exit immediately!__
	Your program should print `Hello, World!` then exit immediately.
	Do not use `system("pause")`, `sleep(1)` or anything of sorts.

1. __Windows users:__
	On Windows, you should not use `./` to run a program in the current directory,
	do instead:

		C:\> hello.exe
		Hello, World!


Copyright © 2020-2023  Rudy Matela
This text is available under the CC BY-SA 4.0 license
or (at your option) the GFDL 1.3 license
