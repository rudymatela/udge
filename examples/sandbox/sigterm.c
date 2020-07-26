/*
 * examples/sandbox/sigterm.c: refuses to terminate, forever
 *
 * Hint: use SIGKILL to end this process. (^\ on the terminal)
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
#include <stdio.h>
#include <signal.h>
#include <unistd.h>

void sigint_handler(int signo)  { fprintf(stderr, "Got SIGINT, refusing to terminate.\n");  }
void sigterm_handler(int signo) { fprintf(stderr, "Got SIGTERM, refusing to terminate.\n"); }
void sigkill_handler(int signo) { fprintf(stderr, "Got SIGKILL, refusing to terminate.\n"); }
void sigabrt_handler(int signo) { fprintf(stderr, "Got SIGABRT, refusing to terminate.\n"); }
void sighup_handler(int signo)  { fprintf(stderr, "Got SIGHUP, refusing to terminate.\n"); }

int main(void)
{
	if (signal(SIGINT,  sigint_handler)  == SIG_ERR) fprintf(stderr, "Cannot catch SIGINT.\n");
	if (signal(SIGTERM, sigterm_handler) == SIG_ERR) fprintf(stderr, "Cannot catch SIGTERM.\n");
	if (signal(SIGKILL, sigkill_handler) == SIG_ERR) fprintf(stderr, "Cannot catch SIGKILL.\n");
	if (signal(SIGABRT, sigabrt_handler) == SIG_ERR) fprintf(stderr, "Cannot catch SIGABRT.\n");
	if (signal(SIGHUP,  sighup_handler)  == SIG_ERR) fprintf(stderr, "Cannot catch SIGHUP.\n");
	while (1) sleep(1);
	return 0;
}
