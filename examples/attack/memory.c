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
