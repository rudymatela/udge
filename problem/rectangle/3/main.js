// main.js: main file for an "rectangle" solution
//
// It processes values from standard input then from the "in.txt" file.
// This is appended to the submitted JavaScript program and tests the add function.
//
// The submitted file processes the standard input and this processes the
// "in.txt" file.
//
// Copyright (C) 2020  Rudy Matela
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

var fs = require('fs').createReadStream('in.txt');
var rl = require('readline').createInterface({input: fs, output: process.stdout});

rl.on('line', function(line){
  words = line.split(/ /);
  rectangle = {
    width:  parseInt(words[0]),
    height: parseInt(words[1])
  }
  console.log(rectangle.width + "x" + rectangle.height + " rectangle, " +
              "area = " + area(rectangle) + ", " +
              "perimeter = " + perimeter(rectangle));
});
