rectangle -- Rectangle
----------------------

Write a program that reads a pair of numbers
representing the width and height of a rectangle
and prints its area an perimeter.
Here is an example of
how a console session with this program should look like:

	$ ./rectangle
	1 2
	1x2 rectangle, area = 2, perimeter = 6
	3 6
	3x6 rectangle, area = 18, perimeter = 18
	10 1
	10x1 rectangle, area = 10, perimeter = 22

_Tip:_ use control+D (^D) to simulate the end of file and exit the program.

### Input and Output

Each line of input contains two numbers _w_ and _h_ where

-1 000 ≤ w, h ≤ 1 000

For each line of input there should be one line of output in the following format:

	<W>x<H> rectangle, area = <AREA>, perimeter = <PERIMETER>

Replace `<W>`, `<H>`, `<AREA>` and `<PERIMETER>` by
the width, height, area and perimeter of the given rectangle respectively.

#### Example input

	1 2
	3 6
	10 1

#### Example output

	1x2 rectangle, area = 2, perimeter = 6
	3x6 rectangle, area = 18, perimeter = 18
	10x1 rectangle, area = 10, perimeter = 22

### The `area` and `perimeter` functions

In order to get a full score,
your program should be implemented using functions `area` and `perimeter`.
They should receive a `rectangle` object.
Please refer to the information for your chosen language.

#### C

	struct rectangle {
		int width;
		int height;
	};
	int area(struct rectangle rectangle);
	int perimeter(struct rectangle rectangle);

#### Python

	class Rectangle:
		def __init__(self, width, height):
			self.width = width
			self.height = height

		def area(self):
			...

		def perimeter(self):
			...

#### Haskell

	data Rectangle = Rectangle { width :: Int
	                           , height :: Int
	                           }
	area :: Rectangle -> Int
	perimeter :: Rectangle -> Int

Alternatively, you can declare the rectangle type as:

	data Rectangle = Rectangle Int Int

#### C++

	class rectangle
	{
		public:
		int width;
		int height;
		int area();
		int perimeter();
	};

#### C♯

	class Rectangle
	{
		public int Width { get; set; }
		public int Height { get; set; }
		public Rectangle(int width, int height);
		public int Area();
		public int Perimeter();
	}

#### Java

	public class Rectangle
	{
		public int width;
		public int height;
		public Rectangle(int width, int height);
		public int area();
		public int perimeter();
	}

#### JavaScript

	rectangle = {width: ..., height: ...}
	function area(rectangle)
	function perimeter(rectangle)

#### Lua

	rectangle = {width = ..., height = ...}
	function area(rectangle)
	function perimeter(rectangle)

#### Ruby

	class Rectangle
	  attr_reader :width
	  attr_reader :height

	  def initialize(width, height)
	    @width = width
	    @height = height
	  end

	  def area
	    ...
	  end

	  def perimeter
	    ...
	  end
	end

#### Racket

	(define (area rectangle) ...)
	(define (perimeter rectangle) ...)
	; where a rectangle is represented by (cons w h)

#### Scheme

	(define (area rectangle) ...)
	(define (perimeter rectangle) ...)
	;;; where a rectangle is represented by (cons w h)

#### R

	rectangle = list(width = ..., height = ...)
	area <- function (rectangle)
	perimeter <- function (rectangle)


### Scoring

* 1/3: works for the above example
* 2/3: works for other test cases
* 3/3: implements the `area` and `perimeter` functions
       over the required `rectangle` type


Copyright © 2020-2023  Rudy Matela
This text is available under the CC BY-SA 4.0 license
or (at your option) the GFDL 1.3 license
