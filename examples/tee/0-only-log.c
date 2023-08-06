/*
 * examples/tee/0-only-log.c: example solution to the "tee" example problem
 *
 * This program is an example incorrect solution to the "tee" example problem.
 * It should get a score of 0/3.
 *
 * This just copies stdin to to the provided argument.
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
#define BUFSZ 0x1000

int main(int argc, char **argv)
{
	int i;
	char buffer[BUFSZ];
	FILE *log = argv[1] ? fopen(argv[1], "w") : NULL;
	if (!log)
		fprintf(stderr,"Could not open log file: %s\n", argv[0]);
	while (fgets(buffer, BUFSZ, stdin)) {
		if (log)
			fputs(buffer, log);
	}
	return 0;
}
