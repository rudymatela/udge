/*
 * example/sandbox/sol-searcher.c: cheats by reading the solution file
 *
 * This currently gets a lot of points for most example problems.
 * It shouldn't.
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

int main()
{
	int c;
	FILE *sol = fopen("/var/lib/udge/problem/:problem:/out","r");
	if (!sol) {
		printf("Could not open the solution file.\n");
		return 1;
	}
	while ((c=fgetc(sol)) != EOF)
		putchar(c);
	return 0;
}
