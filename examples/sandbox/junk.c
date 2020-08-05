/*
 * examples/sandbox/junk.c: creates junk files on /run/udge/...
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
#include <signal.h>
#include <unistd.h>

int main(void)
{
	int i;
	FILE *f;
	const char *filenames[] = {
		"/run/udge/1/junk.txt",
		"/run/udge/2/junky junk.txt",
		"/run/udge/3/junky\njunk.txt",
		"/run/udge/4/junky-junk.txt",
		"/run/udge/5/junk\tjunk.txt",
		"/run/udge/6/junk.txt"
	};
	for (i=0; i<6; i++) {
		f = fopen(filenames[i], "w");
		if (!f)
			continue;
		fputs("This is a junk file.\n", f);
		fputs("Udge should remove it.\n", f);
		fclose(f);
	}
	return 0;
}
