/*
 * examples/sandbox/dns.c: resolves the example.com hostname
 *
 * This program exists successfully when the hostname is resolved
 * otherwise it exits with an error.
 *
 * If "udge-judge hello-world examples/sandbox/dns.c" returns
 *
 *     No - runtime error (non-zero exit code)
 *     0/1
 *
 * it means that network sandboxing is working and
 * successfully blocking DNS calls.
 *
 * Now, if it returns:
 *
 *     No - wrong answer
 *     0/1
 *
 * then it means the network sandboxing is not working
 * as it did not block the DNS call.
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
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int main()
{
	struct addrinfo *addrinfo, *a;
	int error;
	char hostname[NI_MAXHOST];
	error = getaddrinfo("example.com", NULL, NULL, &addrinfo);
	if (error) {
		fprintf(stderr, "could not resolve hostname (%i)\n", error);
		exit(1);
	}
	for (a = addrinfo; a != NULL; a = a->ai_next) {
		error = getnameinfo(a->ai_addr, a->ai_addrlen, hostname, NI_MAXHOST, NULL, 0, 0);
		if (error != 0)
			fprintf(stderr, "could not get hostname info (%i)\n", error);
		else
			printf("hostname: %s\n", hostname);
	}
	freeaddrinfo(addrinfo);
	return 0;
}