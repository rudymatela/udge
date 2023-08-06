/*
 * examples/rectangle/rectangle.cc: solution to the "rectangle" example problem
 *
 * This program is an example solution to the "rectangle" example problem
 * that gets a full 3/3 score.
 *
 * This file is part of Udge.
 *
 *
 * Copyright (C) 2020-2023  Rudy Matela
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>

class rectangle
{
	public:
    int width;
    int height;

	int area()
	{
		return this->width * this->height;
	}

	int perimeter()
	{
		return 2 * (this->width + this->height);
	}
};

int read_rectangle(FILE *pf, struct rectangle *pr)
{
	return fscanf(pf, " %d %d", &pr->width, &pr->height) == 2;
}

int main()
{
	class rectangle rectangle;
	while (read_rectangle(stdin, &rectangle)) {
		printf("%dx%d rectangle, area = %d, perimeter = %d\n",
			rectangle.width,
			rectangle.height,
			rectangle.area(),
			rectangle.perimeter()
		);
	}
	return 0;
}
