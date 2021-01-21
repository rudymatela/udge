/*
 * examples/add/0-re2.c: prints the example solution in a single line
 *
 * This program is an example solution to the "add" example problem.
 * It should get a score of 0/6 with a "runtime error" message.
 *
 * Even though this is an otherwise correct solution, this fails as the program
 * ends with "return 1".
 *
 * This file is part of Udge.
 *
 *
 * Copyright (C) 2020-2021  Rudy Matela
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
#include <stdio.h>
int main()
{
	int i,j;
	while (scanf("%i %i",&i,&j)==2)
		printf("%i\n",i+j);
	return 1;
}
