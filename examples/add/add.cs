// examples/add/add.cs: example correct solution to "add"
//
// This is a correct solution to the "add" problem.
// It should get a full 6/6 score.
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

using System;

class Program
{
	static void Main()
	{
		string line;
		while ((line = Console.ReadLine()) != null) {
			string[] inputs = line.Split(null);
			int x = Convert.ToInt32(inputs[0]);
			int y = Convert.ToInt32(inputs[1]);
			Console.WriteLine(Add(x,y));
		}
	}

	public static int Add(int x, int y)
	{
		return x + y;
	}
}
