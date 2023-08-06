#!/usr/bin/env Rscript
#
# examples/rectangle/rectangle.r: solution to the "rectangle" example problem
#
# This program is an example solution to the "rectangle" example problem
# that gets a full 3/3 score.
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

area <- function(rectangle) {
	return (rectangle$width*rectangle$height)
}

perimeter <- function(rectangle) {
	return (2 * (rectangle$height + rectangle$width))
}

input <- file("stdin", "r")
while (length(l <- readLines(input, n=1)) > 0) {
	l <- strsplit(l, " +")[[1]]
	rectangle = list(height = strtoi(l[1], 10), width = strtoi(l[2], 10))
	cat(rectangle$height)
	cat("x")
	cat(rectangle$width)
	cat(" rectangle, area = ")
	cat(area(rectangle))
	cat(", perimeter = ")
	cat(perimeter(rectangle))
	cat("\n")
}
