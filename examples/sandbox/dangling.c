/*
 * examples/sandbox/dangling.c: creates a dangling child
 *
 * After terminating,
 * this program leaves a dangling child
 * with a different name.
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
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

int main(int argc, char **argv)
{
	int r;
	if (fork()) {
		setsid();
		execl("/usr/bin/sleep","/usr/bin/sleep","60",NULL);
	} else {
		setsid();
		execl("/usr/bin/sleep","/usr/bin/sleep","66",NULL);
	}
	return 0;
}
