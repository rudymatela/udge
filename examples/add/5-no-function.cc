/*
 * examples/add/5-no-function.cc: incorrect solution to the "add" example problem
 *
 * This program is an example solution to the "add" example problem
 * that gets a 5/6 score due to the missing "add" function.
 *
 * This file is part of Udge.
 *
 *
 * Copyright (C) 2020-2023  Rudy Matela
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <iostream>
using namespace std;
int main()
{
	int i,j;
	while (1) {
		cin >> i >> j;
		if (cin.fail())
			break;
		cout << (i + j) << endl;
	}
	return 0;
}
