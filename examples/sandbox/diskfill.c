/*
 * examples/sandbox/disk-usage.c: tries to fill up the disk
 *
 *
 * Copyright (C) 2020-2022  Rudy Matela
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
	FILE *f;
	while (1) {
		f = fopen("big-file.txt","a");
		if (!f) {
			fprintf(stderr, "Could not open big-file.txt");
			return 1;
		}
		fputs("This is a very very very very long string. 01234567890ABCDEF\n", f);
		fputs("This is a very very very very long string. 01234567890ABCDEF\n", f);
		fputs("This is a very very very very long string. 01234567890ABCDEF\n", f);
		fputs("This is a very very very very long string. 01234567890ABCDEF\n", f);
		fputs("This is a very very very very long string. 01234567890ABCDEF\n", f);
		fputs("This is a very very very very long string. 01234567890ABCDEF\n", f);
		fclose(f);
	}
	return 0;
}
