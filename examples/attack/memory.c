/*
 * examples/attack/memory.c: allocates 1GB of memory
 *
 * This program allocates memory progressively up to 1GB of memory.
 * If Udge's sandbox functionality is working correctly then:
 *
 * $ udge-judge hello-world examples/attack/memory.c
 * No - runtime error (non-zero exit code)
 * 0/1
 *
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

#define K (1024)
#define M (1024*1024)
#define G (1024*1024*1024)

void errxit(char *msg)
{
	perror(msg);
	exit(1);
}

int main()
{
	int i;
	char *kilo;
	char *mega;
	char *giga;

	kilo = malloc(K);
	if (!kilo)
		errxit("Failed to allocate 1Kb.");
	puts("Allocated 1Kb.");

	mega = malloc(M);
	if(!mega)
		errxit("Failed to allocate 1Mb.");
	puts("Allocated 1Mb.");

	mega = malloc(256*M);
	if(!mega)
		errxit("Failed to allocate 256Mb.");
	puts("Allocated 256Mb.");

	giga = malloc(G);
	if(!giga)
		errxit("Failed to allocate 1Gb.");
	puts("Allocated 1Gb.");

	/* Linux (by default) does not actually allocate until memory is used. */
	for (i=0; i<K; i++)
		kilo[i] = 0;
	puts("Used 1Kb.");

	for (i=0; i<M; i++)
		mega[i] = 0;
	puts("Used 1Mb.");

	for (i=0; i<256*M; i++)
		mega[i] = 0;
	puts("Used 1Mb.");

	for (i=0; i<G; i+=K)
		giga[i] = 0;
	puts("Used 1Gb.");

	return 0;
}
