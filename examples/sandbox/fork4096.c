/*
 * examples/sandbox/fork4096.c: forks into 4096 processes
 *
 * This program forks into 4096 processes.
 * If Udge's sandbox functionality is working correctly then:
 *
 * $ udge-judge hello-world examples/sandbox/fork4096.c
 * time limit exceeded
 * 0/1
 *
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
#include <stdlib.h>
#include <unistd.h>
int main()
{
	int i, r;
	fork(); /*    2 processes */
	fork(); /*    4 processes */
	fork(); /*    8 processes */
	fork(); /*   16 processes */
	fork(); /*   32 processes */
	fork(); /*   64 processes */
	fork(); /*  128 processes */
	fork(); /*  256 processes */
	fork(); /*  512 processes */
	fork(); /* 1024 processes */
	fork(); /* 2048 processes */
	fork(); /* 4096 processes */
	sleep(1);
	return 0;
}
