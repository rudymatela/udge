/*
 * examples/sandbox/openfiles.c: tries to open 256 files at the same time
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
#include <signal.h>
#include <unistd.h>

#define BUFSIZE 100
#define NFILES 256

int main(void)
{
	int i;
	char filename[BUFSIZE];
	FILE *f[NFILES];

	for (i=0; i<NFILES; i++) {
		sprintf(filename, "file-%d.txt", i);
		f[i] = fopen(filename, "a");
		if (!f[i]) {
			fprintf(stderr,"could not open file for writing: %s\n", filename);
			return 1;
		}
	}

	for (i=0; i<NFILES; i++)
		fprintf(f[i], "%d\n", i);

	for (i=0; i<NFILES; i++)
		fclose(f[i]);

	return 0;
}
