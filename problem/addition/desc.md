addition - Addition
-------------------

Write a program that
	reads several pairs of numbers and,
	for each pair, prints the sum.
The standard input and output devices
	are usually the keyboard and screen of a command line session.
Here is an example session with this program:

	$ ./add
	0 0
	0
	3 7
	10
	12 21
	33
	-123 321
	198
	1234 4321
	5555

### Input and Output

Each line of input contains two numbers _x_ and _y_ where

-2 000 000 000 ≤ x, y ≤ 2 000 000 000

For each line of input there should be a line of output
with the result of adding _x_ to _y_.

#### Example input

	0 0
	3 7
	12 21
	-123 321
	1234 4321


#### Example output

	0
	10
	33
	198
	5555

This is one of the default examples of Udge.  This differs from [add](/add)
in that this version provides a simple 0/1 or 1/1 scoring and does not require
the implementation of an `add()` function.

Copyright © 2020 Rudy Matela
This text is available under the CC BY-SA 4.0 license
or (at your option) the GFDL 1.3 license
