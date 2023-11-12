/*
 * examples/salaries/main.c: part of an incorrect correct solution to "salaries"
 *
 * This file is part of a partly incorrect solution
 * to the "salaries" example problem.
 * It should get a full 3/6 score
 * due to the required functions not being placed in the right file.
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

int total_salary(struct employee employees[], int n_employees)
{
	int i;
	int total = 0;
	for (i=0; i<n_employees; i++)
		total += employees[i].salary;
	return total;
}


struct employee *highest_salary(struct employee employees[], int n_employees)
{
    int i;
    struct employee *pe = &employees[0];
    for (i=0; i<n_employees; i++) if (employees[i].salary > pe->salary)
		pe = &employees[i];
	return pe;
}


struct employee *lowest_salary(struct employee employees[], int n_employees)
{
    int i;
    struct employee *pe = &employees[0];
    for (i=0; i<n_employees; i++) if (employees[i].salary < pe->salary)
		pe = &employees[i];
	return pe;
}

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
    printf("%s has the highest salary: $%d\n", phigh->name, phigh->salary);
    printf("%s has the lowest salary: $%d\n", plow->name, plow->salary);
    printf("The total monthly cost is $%d\n", t);
    return 0;
}
