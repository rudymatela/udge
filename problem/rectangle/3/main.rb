# main.rb: main file for an "rectangle" solution
#
# This is appended to the submitted Ruby program and tests the area and
# perimeter functions.
#
# The submitted file processes the standard input and this processes the
# "in.txt" file.
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

# standard input is already processed
# as the submitted programmed is run when imported
File.foreach("in.txt") do |line|
  w, h = line.split.map { |s| s.to_i }
  rectangle = Rectangle.new(w,h);
  puts "#{rectangle.width}x#{rectangle.height} rectangle, area = #{rectangle.area}, perimeter = #{rectangle.perimeter}"
end
