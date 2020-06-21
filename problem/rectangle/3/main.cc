#include <stdio.h>
#include <stdlib.h>

class rectangle {
	public:
	int width;
	int height;
	int area();
	int perimeter();
};

static int read_rectangle(FILE *pf, rectangle *pr)
{
	return fscanf(pf, " %d %d", &pr->width, &pr->height) == 2;
}

static int main_for(FILE *in)
{
	rectangle rectangle;
	while (read_rectangle(in, &rectangle)) {
		printf("%dx%d rectangle, area = %d, perimeter = %d\n",
			rectangle.width,
			rectangle.height,
			rectangle.area(),
			rectangle.perimeter()
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
