/*
 * examples/cat/3-wrong-stderr.c: example solution to the "cat" example problem
 *
 * This program is an example solution to the "cat" example problem.
 * It should get a score of 3/4 due to failing to print to stderr.
 *
 * This is a feature-limited reimplementation of the POSIX cat program.
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
#include <stdlib.h>

int filecopy(FILE *in, FILE *out)
{
	int c;
	while ((c=fgetc(in)) != EOF)
		fputc(c, out);
}

int main(int argc, char **argv)
{
	int i;
	FILE *f;
	for (i=1; i<argc; i++) {
		f = fopen(argv[i], "r");
		if (!f)
			continue;
		filecopy(f, stdout);
		fclose(f);
	}
	return 0;
}
