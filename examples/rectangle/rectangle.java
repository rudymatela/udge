// examples/rectangle/rectangle.java: example correct solution to "rectangle"
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

import java.util.Scanner;

public class Rectangle {
	public int width;
	public int height;

	public Rectangle(int width, int height) {
		this.width  = width;
		this.height = height;
	}

	public int area() {
		return this.width * this.height;
	}

	public int perimeter() {
		return 2 * (this.width + this.height);
	}

    public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		while (in.hasNext()) {
			int w = in.nextInt();
			int h = in.nextInt();
			Rectangle rectangle = new Rectangle(w, h);
			System.out.println(rectangle.width + "x" + rectangle.height + " rectangle, " +
			                   "area = " + rectangle.area() + ", " +
			  				   "perimeter = " + rectangle.perimeter());
		}
    }
}
