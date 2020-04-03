#include <stdio.h>
int sum(int i, int j)
{
	return i+j;
}
int main()
{
	int i,j;
	while (scanf("%d %d",&i,&j)==2)
		printf("%d\n",sum(i,j));
	return 0;
}
