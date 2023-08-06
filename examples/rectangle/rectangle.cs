// examples/rectangle/rectangle.cs: example correct solution to "rectangle"
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

using System;

class Rectangle
{
	public int Width { get; set; }
	public int Height { get; set; }

	public Rectangle(int width, int height)
	{
		this.Width  = width;
		this.Height = height;
	}

	public int Area()
	{
		return this.Width * this.Height;
	}

	public int Perimeter()
	{
		return 2 * (this.Width + this.Height);
	}
}

class Program
{
	static void Main()
	{
		string line;
		while ((line = Console.ReadLine()) != null) {
			string[] inputs = line.Split();
			int w = Convert.ToInt32(inputs[0]);
			int h = Convert.ToInt32(inputs[1]);
			Rectangle rectangle = new Rectangle(w, h);
			Console.WriteLine(rectangle.Width + "x" + rectangle.Height + " rectangle, " +
			                  "area = " + rectangle.Area() + ", " +
			  				  "perimeter = " + rectangle.Perimeter());
		}
	}
}
