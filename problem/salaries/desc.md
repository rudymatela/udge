salaries -- Salaries
--------------------

You have been hired by a company to develop its HR system.
Your first task is to implement a program that receives
the names and salaries of employees and computes:

* the employee with the highest salary;
* the employee with the lowest salary;
* the total monthly cost to the company.

There are both I/O and nonfunctional requirements.
Please read and try to understand
the exercise statement entirely before starting.
The nonfunctional requirements are as important
as the functional requirements in this exercise.


### Input and Output  #io

The first line of input will contain a number _n_
indicating the number of employees in the company.

The following _n_ lines will contain each a name and a salary of an employee.

For the purpose of this exercise,
the name will be just a single first/given name
of less than 60 characters.
The salary will be given as an integer no higher
than a million.
The company shall have no more than 100 employees.

#### Example input

	3
	Alice 3100
	Bob   2500
	Eve   6500

#### Example output

	Eve has the highest salary: $6500
	Bob has the lowest salary: $2500
	The total monthly cost is $12100


### Non-functional requirements

Your program should be modular and composed of three files:

* `employee.h`: header file with an `employee` struct and function prototypes.
* `employee.c`: program file with the implementation of functions
                that manipulate employees.
				This file should __not__ contain a `main` function.
* `main.c`: program with with the `main` function and I/O operations
            using the functions declared in `employee.h` and `employee.c`.

With all three files placed in the same directory,
your program should be compilable with:

	gcc employee.c main.c -o main.exe

The `employee.h` file is already prepared with
the interfaces you have to follow:

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

Use the above `employee.h` file and create your own `employee.c` and `main.c` files.
The `employee.h` file should be used as-is without changes.

The `highest_salary` and `lowest_salary` functions should
return the pointer to the employee `struct`
with the highest and lowest salaries respectiely.

The function `total_salary` should return the total sum of salaries.


### Submission #submission

For this exercise, you should [submit] a zip or tar archive
with (only) the `employee.h`, `employee.c` and `main.c` files.
There should be no directories in this archive
and all letters in the filenames should be lowercase.
The marking scripts will specifically check for this.

[submit]: /submit


Copyright © 2020-2023  Rudy Matela
This text is available under the CC BY-SA 4.0 license
or (at your option) the GFDL 1.3 license