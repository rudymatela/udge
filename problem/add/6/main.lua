-- main.lua: main file for an "add" solution
--
-- This is appended to the submitted Python program and tests the add function.
--
-- The submitted file processes the standard input and this processes the
-- "in.txt" file.
--
--
-- Copyright (C) 2020  Rudy Matela
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

-- standard input is already processed
-- as the submitted programmed is run when imported
io.input("in.txt")
x, y = io.read("*n", "*n")
while x and y do
  print(add(x,y))
  x, y = io.read("*n", "*n")
end