#include <stdio.h>
#include <unistd.h>
int main()
{
	int i,j;
	while (scanf("%i %i",&i,&j)==2)
		printf("%i\n",i+i);
	while (1)
		sleep(1);
	return 0;
}
