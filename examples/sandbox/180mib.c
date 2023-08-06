/*
 * examples/sandbox/disk-usage-files.c: creates 180MiB across multiple files
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
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

#define SIXTYFOUR "A string that is 64 characters long including the line break...\n"

/* writes 1KiB to the given file */
int kputs(FILE *f)
{
	int i;
	for (i=0; i<16; i++)
		fputs(SIXTYFOUR, f);
}

int main(void)
{
	int i, j;
	FILE *f;
	char filename[360];
	assert(strlen(SIXTYFOUR)==64);
	printf("Generating 360 files each 512KiB totaling 180MiB.\n");
	for (i=0; i<512; i++) for (j=0; j<360; j++) {
		sprintf(filename,"big-file-%d.txt",j);
		f = fopen(filename,"a");
		if (!f) {
			fprintf(stderr, "Could not open %s", filename);
			return 1;
		}
		kputs(f);
		fclose(f);
	}
	printf("Done!\n");
	return 0;
}
