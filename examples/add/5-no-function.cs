// examples/add/5-no-function.cs: almost correct solution to "add"
//
// This program is an example solution to the "add" example problem.  Although
// it implements the solution correctly, it does not include the required "add"
// function.  It should get a score of 5/6 with a "compile error" message.
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

using System;

class Add
{
	static void Main()
	{
		string line;
		while ((line = Console.ReadLine()) != null) {
			string[] inputs = line.Split(null);
			int x = Convert.ToInt32(inputs[0]);
			int y = Convert.ToInt32(inputs[1]);
			Console.WriteLine(x+y);
		}
	}
}
