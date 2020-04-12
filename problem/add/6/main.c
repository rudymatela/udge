#include <stdio.h>
int add(int, int);
int main()
{
	int i, j;
	while (scanf("%i %i", &i, &j)==2)
		printf("%i\n", add(i,j));
	return 0;
}
