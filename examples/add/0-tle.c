/*
 * examples/add/0-tle.c: solves "add" then waits a second
 *
 * This program is an example solution to the "add" example problem.
 * It should get a score of 0/6 with a "time limit exceeded" message.
 *
 * Even though this is an otherwise corrrect solution,
 * this fails because it takes longer than the time limit.
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
#include <stdio.h>
#include <unistd.h>
int main()
{
	int i,j;
	while (scanf("%i %i",&i,&j)==2)
		printf("%i\n",i+i);
	while (1)
		sleep(1);
	return 0;
}
