/*
 * examples/sandbox/forkbomb.c: forks indefinitely
 *
 * This program is a forkbomb that halts the system by forking indefinitely.
 * If Udge's sandbox functionality is working correctly then:
 *
 * $ udge-judge hello-world examples/sandbox/forkbomb.c
 * No - time limit exceeded
 * 0/1
 *
 * WARNING: DO NOT RUN THIS UNSANDBOXED OR YOUR WILL FREEZE YOUR SYSTEM
 *
 *
 * Copyright (C) 2020  Rudy Matela
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
/* WARNING: DO NOT RUN THIS UNSANDBOXED OR YOUR WILL FREEZE YOUR SYSTEM */
#include <unistd.h>
int main()
{
	for (;;)
		fork();
	return 0;
}
/* WARNING: DO NOT RUN THIS UNSANDBOXED OR YOUR WILL FREEZE YOUR SYSTEM */
