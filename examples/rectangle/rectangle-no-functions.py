#!/usr/bin/env python
#
# examples/rectangle/rectangle-no-functions.py: solution to the "rectangle" example problem
#
# This program is an example solution to the "rectangle" example problem
# that gets a 2/3 score.
#
# This file is part of Udge.
#
#
# Copyright (C) 2020-2023  Rudy Matela
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import sys

class Rectangle:
	def __init__(self, width, height):
		self.width = width
		self.height = height

for line in sys.stdin:
	w,h = line.split()
	rectangle = Rectangle(int(w),int(h));
	print("%dx%d rectangle, area = %d, perimeter = %d" %
		(rectangle.width,
		 rectangle.height,
		 rectangle.width * rectangle.height,
		 2 * (rectangle.width + rectangle.height)))
