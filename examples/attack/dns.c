/*
 * examples/attack/dns.c: resolves the example.com hostname
 *
 * This program exists successfully when the hostname is resolved
 * otherwise it exits with an error.
 *
 * If "udge-judge hello-world examples/attack.dns.c" returns
 *
 *     No - runtime error (non-zero exit code)
 *
 * it means that network sandboxing is working and
 * successfully blocking DNS calls.
 *
 * Now, if it returns:
 *     No - wrong answer
 *
 * then it means the network sandboxing is not working
 * as it did not block the DNS call.
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
