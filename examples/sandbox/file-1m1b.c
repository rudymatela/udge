/*
 * examples/sandbox/file-1m1b.c: creates a 2MiB+1B file
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

int main(void)
{
	int i;
	FILE *f = fopen("file-1m1b.txt","a");
	if (!f) {
		fprintf(stderr,"could not create file-1m1b.txt");
		return 1;
	}
	for (i=0; i<2*1024*1024; i++)
		putc('\0',f);
	putc('\0',f);
	fclose(f);
	return 0;
}
