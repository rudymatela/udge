// examples/salaries/employee.h: part of a incorrect solution to "salaries"
//
// This file is part of an incorrect solution to the "salaries" example problem.
// It should get a 0/3 score.
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

// functions are provided in the reverse order
// should yield "header file has been tampered with"
struct employee *lowest_salary(struct employee employees[], int n_employees);
struct employee *highest_salary(struct employee employees[], int n_employees);
int total_salary(struct employee employees[], int n_employees);

#endif /* _EMPLOYEE_H_ */
