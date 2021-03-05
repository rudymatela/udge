/*
 * examples/sandbox/tmp60mib.c: creates a 60MB file on /tmp
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
#include <string.h>
#include <assert.h>

#define SIXTYFOUR "A string that is 64 characters long including the line break...\n"

/* writes 1KiB to the given file */
void kputs(FILE *f)
{
	int i;
	for (i=0; i<16; i++)
		fputs(SIXTYFOUR, f);
}

void mputs(FILE *f)
{
	int i;
	for (i=0; i<1024; i++)
		kputs(f);
}

int main(void)
{
	int i;
	FILE *f;
	for (i=0; i<60; i++) {
		f = fopen("/tmp/udge-tmp60mib.txt", "a");
		mputs(f);
		fclose(f);
	}
	return 0;
}
