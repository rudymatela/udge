#include <stdio.h>
#include <stdlib.h>

struct rectangle {
    int width;
    int height;
};
int area(struct rectangle rectangle);
int perimeter(struct rectangle rectangle);

static int read_rectangle(FILE *pf, struct rectangle *pr)
{
	return fscanf(pf, " %d %d", &pr->width, &pr->height) == 2;
}

static int main_for(FILE *in)
{
	struct rectangle rectangle;
	while (read_rectangle(in, &rectangle)) {
		printf("%dx%d rectangle, area = %d, perimeter = %d\n",
		    rectangle.width,
			rectangle.height,
			area(rectangle),
			perimeter(rectangle)
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
