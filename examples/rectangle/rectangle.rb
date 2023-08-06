#!/usr/bin/env ruby
#
# examples/rectangle/rectangle.rb: solution to the "rectangle" example problem
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

class Rectangle
  attr_reader :width
  attr_reader :height

  def initialize(width, height)
    @width = width
    @height = height
  end

  def area
    height * width
  end

  def perimeter
    2 * (height + width)
  end
end

STDIN.each do |line|
  w, h = line.split.map { |s| s.to_i }
  rectangle = Rectangle.new(w,h);
  puts "#{rectangle.width}x#{rectangle.height} rectangle, area = #{rectangle.area}, perimeter = #{rectangle.perimeter}"
end
