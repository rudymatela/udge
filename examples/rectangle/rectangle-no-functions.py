#!/usr/bin/env python
#
# examples/rectangle/rectangle.py: solution to the "rectangle" example problem
#
# This program is an example solution to the "rectangle" example problem
# that gets a 2/3 score.
#
# This file is part of Udge.
#
#
# Copyright (C) 2020  Rudy Matela
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
	def __init__(self, width, height, unit=''):
		self.width = width
		self.height = height
		self.unit = unit

for line in sys.stdin:
	w,h,u = line.split()
	rectangle = Rectangle(int(w),int(h),u);
	print("The area is %d square %s and the perimeter is %d %s." %
		(rectangle.height * rectangle.width,
		 rectangle.unit,
		 2 * (rectangle.height + rectangle.width),
		 rectangle.unit))