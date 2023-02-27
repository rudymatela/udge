// employee.h: reference header file for the salaries problem
//
// Lines not beginning with "//" are compared against
// the submitted header file to assure that it is the same.
//
// This file is part of Udge.
//
//
// Copyright (C) 2022-2023  Rudy Matela
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
#ifndef _EMPLOYEE_H_
#define _EMPLOYEE_H_

struct employee {
	char name[60];
	int salary;
};

int total_salary(struct employee employees[], int n_employees);
struct employee *highest_salary(struct employee employees[], int n_employees);
struct employee *lowest_salary(struct employee employees[], int n_employees);

#endif /* _EMPLOYEE_H_ */
