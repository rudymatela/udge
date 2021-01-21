/*
 * examples/pinfo.c: prints information about the program environment
 *
 * This file is part of Udge.
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
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>

void pwd()
{
	char cwd[PATH_MAX];
	printf("cwd: %s\n", getcwd(cwd, sizeof(cwd)));
}

void whoami()
{
	printf("whoami: %s\n", getlogin());
}

int main(int argc, char **argv, char **env)
{
	int i;

	for (i=0; env[i]; i++)
		printf("%s\n",env[i]);
	printf("\n");

	printf("stdout is working\n");
	fprintf(stdout,"stdout is working\n");
	fprintf(stderr,"stderr is working\n");

	printf("%s",argv[0]);
	for (i=1; i<argc; i++)
		printf(" %s",argv[i]);
	printf("\n");

	pwd();
	whoami();
	printf("pid: %i\n",getpid());
	return 0;
}
