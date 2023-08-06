// examples/rectangle/rectangle.js: example correct solution to "rectangle"
//
// This is a correct solution to the "rectangle" problem.
// It should get a full 3/3 score.
//
//
// This file is part of Udge.
//
//
// Copyright (C) 2020-2023  Rudy Matela
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

var rl = require('readline').createInterface({input: process.stdin, output: process.stdout});

function area(rectangle) {
  return rectangle.width * rectangle.height;
}

function perimeter(rectangle) {
  return 2 * (rectangle.width + rectangle.height);
}

rl.on('line', function(line){
  words = line.split(/ /);
  rectangle = {
    width:  parseInt(words[0]),
    height: parseInt(words[1])
  }
  console.log(rectangle.width + "x" + rectangle.height + " rectangle, " +
              "area = " + area(rectangle) + ", " +
              "perimeter = " + perimeter(rectangle));
})
