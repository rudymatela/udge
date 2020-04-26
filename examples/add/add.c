#include <stdio.h>
int add(int i, int j)
{
	return i+j;
}
int main()
{
	int i,j;
	while (scanf("%d %d",&i,&j)==2)
		printf("%d\n",add(i,j));
	return 0;
}
