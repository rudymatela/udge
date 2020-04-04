#!/usr/bin/env python
import sys

for line in sys.stdin:
	x,y = [int(x) for x in line.split()]
	print(x+y)
