/*
 * examples/salaries/employee.c: part of a correct solution to "salaries"
 *
 * This file is part of a correct solution to the "salaries" example problem.
 * It should get a full 3/3 score.
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
#include "employee.h"


int total_salary(const struct employee employees[], size_t n_employees)
{
	int i;
	int total = 0;
	for (i=0; i<n_employees; i++)
		total += employees[i].salary;
	return total;
}


struct employee *highest_salary(const struct employee employees[], size_t n_employees)
{
    int i;
    struct employee *pe = &employees[0];
    for (i=0; i<n_employees; i++) if (employees[i].salary > pe->salary)
		pe = &employees[i];
	return pe;
}


struct employee *lowest_salary(const struct employee employees[], size_t n_employees)
{
    int i;
    struct employee *pe = &employees[0];
    for (i=0; i<n_employees; i++) if (employees[i].salary < pe->salary)
		pe = &employees[i];
	return pe;
}
