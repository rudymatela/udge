/*
 * examples/sandbox/socket.c: does an HTTP GET on 127.0.0.1
 *
 * This program performs an HTTP GET on 127.0.0.1.
 *
 * If this program is able to connect,
 * it prints whatever the HTTP server sends as response.
 *
 * If this program is not able to connect,
 * it fails with an error.
 *
 * If Udge's sandbox functionality is working correctly then:
 *
 * $ udge-judge hello-world examples/sandbox/socket.c
 * No - runtime error
 * 0/1
 *
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
