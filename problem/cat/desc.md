cat - concatenate files and print on the standard output
--------------------------------------------------------

TODO: add a proper description for the cat problem


	$ cat text.txt
	These are the contents of text.txt.
	$ cat numbers.txt
	123
	456
	789
	000
	$ cat text.txt numbers.txt text.txt
	These are the contents of text.txt.
	123
	456
	789
	000
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


Copyright © 2020 Rudy Matela
This text is available under the CC BY-SA 4.0 license
or (at your option) the GFDL 1.3 license
