/*
 * problem/salaries/4-6/main.c: main file for a "salaries" solution
 *
 * This is to be linked to the submitted library file.
 *
 * This file is part of Udge.
 *
 *
 * Copyright (C) 2022-2023  Rudy Matela
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
#include "employee.h"

int main()
{
    int n;
    struct employee employee[1000];
	struct employee *phigh, *plow;
    int i;
    int h, l, t;
    scanf("%d", &n);
    for (i=0; i<n; i++)
        scanf("%s %d", employee[i].name, &employee[i].salary);
    phigh = highest_salary(employee, n);
    plow = lowest_salary(employee, n);
    t = total_salary(employee, n);
    printf("highest salary: %s with $%d\n", phigh->name, phigh->salary);
    printf("lowest salary: %s with $%d\n", plow->name, plow->salary);
    printf("total cost: $%d\n", t);
    return 0;
}
