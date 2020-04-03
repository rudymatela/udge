#include <stdio.h>
int sum(int, int);
int main()
{
	int i, j;
	while (scanf("%i %i", &i, &j)==2)
		printf("%i\n", sum(i,j));
	return 0;
}
