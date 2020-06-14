#include <stdio.h>
#include <stdlib.h>

struct rectangle {
    char unit[100];
    int height;
    int width;
};
int area(struct rectangle rectangle);
int perimeter(struct rectangle rectangle);

int read_rectangle(FILE *pf, struct rectangle *pr)
{
	return fscanf(pf, " %d %d %s", &pr->height, &pr->width, &pr->unit) == 3;
}

int main()
{
	struct rectangle rectangle;
	while (read_rectangle(stdin, &rectangle)) {
		printf("The area is %d square %s and the perimeter is %d %s.\n",
			area(rectangle),
			rectangle.unit,
			perimeter(rectangle),
			rectangle.unit
		);
	}
	return 0;
}

int area(struct rectangle rectangle)
{
	return rectangle.height * rectangle.width;
}

int perimeter(struct rectangle rectangle)
{
	return 2 * (rectangle.height + rectangle.width);
}
