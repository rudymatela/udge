-- examples/rectangle/rectangle.lua: solution to the "rectangle" example problem
--
-- This program is an example solution to the "rectangle" example problem
-- that gets a full 3/3 score.
--
-- This file is part of Udge.
--
--
-- Copyright (C) 2020-2023  Rudy Matela
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

function area(rectangle)
  return rectangle.height * rectangle.width
end

function perimeter(rectangle)
  return 2 * (rectangle.height + rectangle.width)
end

rectangle = {}
rectangle.height, rectangle.width = io.read("*n", "*n")
while rectangle.height and rectangle.width do
  print(string.format(
    "%dx%d rectangle, area = %d, perimeter = %d",
    rectangle.height,
    rectangle.width,
    area(rectangle),
    perimeter(rectangle)
  ))
  rectangle.height, rectangle.width = io.read("*n", "*n")
end
