#include <stdio.h>
#include <stdlib.h>

struct rectangle {
    char unit[100];
    int height;
    int width;
};
int area(struct rectangle rectangle);
int perimeter(struct rectangle rectangle);

static int read_rectangle(FILE *pf, struct rectangle *pr)
{
	return fscanf(pf, " %d %d %s", &pr->height, &pr->width, &pr->unit) == 3;
}

static int main_for(FILE *in)
{
	struct rectangle rectangle;
	while (read_rectangle(in, &rectangle)) {
		printf("The area is %d square %s and the perimeter is %d %s.\n",
			area(rectangle),
			rectangle.unit,
			perimeter(rectangle),
			rectangle.unit
		);
	}
	return 0;
}

int main()
{
	FILE *in = fopen("in.txt","r");
	main_for(stdin);
	main_for(in);
	fclose(in);
	return 0;
}
