/*
 * examples/attack/socket.c: performs an HTTP GET on 127.0.0.1
 *
 * If this program is able to connect,
 * it prints whatever the HTTP server sends as response.
 *
 * If this program is not able to connect,
 * it fails with an error.
 *
 * If udge-sandbox is working, udge-judge should return:
 *
 *     No - runtime error (non-zero exit code)
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <arpa/inet.h>

int errxit(const char *msg)
{
	puts(msg);
	exit(1);
}

/* like write but with a zero terminated string */
int writes(int fd, const char *buf)
{
	return write(fd, buf, strlen(buf));
}

int inet_connect(const char *ip_address, uint16_t port)
{
	int sock;
	int error;
	struct sockaddr_in addr;

	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0)
		return -1;

	addr.sin_family = AF_INET;
	error = inet_pton(AF_INET, "127.0.0.1", &addr.sin_addr);
	if (error <= 0)
		return -2;
	addr.sin_port = htons(port);

	error = connect(sock, (struct sockaddr *)&addr, sizeof(addr));
	if (error < 0)
		return -3;

	return sock;
}

int main()
{
	int sock;
	char buf[1024];
	int nread;

	sock = inet_connect("127.0.0.1", 80);
	if (sock < 0)
		errxit("could not connect to 127.0.0.1:80");

	writes(sock, "GET / HTTP/1.1\n");
	writes(sock, "Host: udge\n");
	writes(sock, "Connection: close\n");
	writes(sock, "\n");

	while (nread = read(sock, buf, 1024))
		write(1, buf, nread);

	close(sock);

	return 0;
}
