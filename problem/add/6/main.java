// main.java: main file for an "add" solution
//
// This is to be linked to the submitted file.
// It processes values from standard input then from the "in.txt" file.
//
//
// Copyright (C) 2020-2021  Rudy Matela
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

import java.io.*;
import java.util.Scanner;

public class AltMain {
    public static void main(String[] args) throws IOException
	{
		doit(new Scanner(System.in));
		File file = new File("in.txt");
		doit(new Scanner(file));
    }

	public static void doit(Scanner in)
	{
		while (in.hasNext()) {
			int x = in.nextInt();
			int y = in.nextInt();
			System.out.println(Add.add(x,y));
		}
	}
}
