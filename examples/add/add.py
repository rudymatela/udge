#!/usr/bin/env python
import sys

def add(i, j):
	return i+j

for line in sys.stdin:
	x,y = [int(x) for x in line.split()]
	print(add(x,y))
