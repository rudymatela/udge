-- examples/rectangle/rectangle-no-functions.lua: example solution to the "rectangle" example problem
--
-- This program is an example solution to the "rectangle" example problem
-- that gets a 2/3 score due to missing functions.
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

h, w = io.read("*n", "*n")
while h and w do
  print(string.format("%dx%d rectangle, area = %d, perimeter = %d", h, w, h*w, 2*(h+w)))
  h, w = io.read("*n", "*n")
end
