/*
 * examples/add/add.c: solution to the "add" example problem
 *
 * This program is an example solution to the "add" example problem
 * that gets a full 6/6 score.
 *
 * This file is part of Udge.
 *
 *
 * Copyright (C) 2020  Rudy Matela
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
int add(int i, int j)
{
	return i+j;
}
int main()
{
	int i,j;
	while (scanf("%d %d",&i,&j)==2)
		printf("%d\n",add(i,j));
	return 0;
}
