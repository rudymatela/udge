add -- Addition
---------------

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

### The `add` function

In order to get a full score,
your program should be implemented using an `add` function
that receives two integers as arguments and returns an integer.
Please refer to the information for the chosen language:

* C prototype:           `int add(int x, int y);`
* Python definition:     `def add(x,y):`
* Haskell type:          `add :: Int -> Int -> Int`
* C++ prototype:         `int add(int x, int y);`
* C# definition:         `public static int Add(int x, int y)` inside a class `Program`
* Java definition:       `public static int add(int x, int y)` inside a public class `Add`
* JavaScript definition: `function add(x, y)`
* Lua definition:        `function add(x, y)`
* Ruby definition:       `def add(x,y)`
* R definition:          `add <- function(x, y)`
* Racket definition:     `(define (add x y) ...)`
* Scheme definition:     `(define (add x y) ...)`

If you are confused by the above,
try earning a partial score first.

### Scoring

* 1/6: works for the above example but produces output in an incorrect format
* 2/6: works for the above example and produces output in the correct format
* 3/6: works for 100 sums
* 4/6: works for 10 000 sums
* 5/6: works for edge cases
* 6/6: implements the `add` function

### Hints

1. __Redirecting input:__
	On most systems (Windows / Linux / OS X),
	it is possible to redirect the standard input and output
	of your program to files, like so:

       $ ./add <inputfile.txt >outputfile.txt

	If you create a _plain_ text file with the "example input",
	the above command should produce
	a plain text file with the "example output".

2. __Produce output as you go:__
	You do not need to accumulate numbers and then produce everything at the end.
	It is enough to produce output as you go.
	As soon as you read a pair of numbers,
	produce the corresponding sum.

3. __Detecting the end of file.__
	In this problem, input is terminated by the end-of-file (EOF).
	Here are ways to detect EOF in C, Python and Haskell:

	- _In C._
		The `scanf` function returns the numbers of elements read from `stdin`.
		Since this problem requires you to read two numbers each line,
		you can compare `scanf`'s result to two as a `while` condition:

			while (scanf(...)==2) {
				...
			}

		Which translates to, _"while you're able to read 2 items from standard input, do ..."_

	- _In Python._
		The pattern `for line in sys.stdin:` can be used
		to create a loop where a file is processed line by line
		until the end-of-file.

	- _In Haskell._
		You can use `interact` to declare the main function
		and implement your solution as a function from `String` to `String`:

			io :: String -> String
			io = ...

			main :: IO
			main = interact io

		EOF is then represented as the nil list constructor (`""` or `[]`)
		at the end of the argument `String`.

4. __Simulating the end of file__
	You can simulate the end-of-file in the command line
	by holding Ctrl and pressing D
	at the beginning of a newline:

		$ ./add
		1 1
		2
		^D
		$

	You can use this to exit back to the command line
	when you are finished testing your program.


Copyright © 2020-2023  Rudy Matela
This text is available under the CC BY-SA 4.0 license
or (at your option) the GFDL 1.3 license
