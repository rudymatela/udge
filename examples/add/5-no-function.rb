#!/usr/bin/env ruby
#
# examples/add/5-no-function.rb: solution to the "add" example problem
#
# This program is an example solution to the "add" example problem
# that gets a 5/6 score as it fails to implement the required "add" function.
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
STDIN.each do |line|
  x, y = line.split.map { |s| s.to_i }
	puts(x+y)
end
