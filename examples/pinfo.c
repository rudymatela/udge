/* Copyright 2020 Rudy Matela */
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
