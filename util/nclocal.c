/*
//  nclocal - copy stdin to local-domain UNIX socket and output response
//            to std.out
//
//  to make:
//    gcc nclocal.c -o nclocal
//
//  to test, start server:
//    newlisp -c -d /tmp/mysocket &
//
//  verify functioning:
//    newlisp -e '(net-eval "/tmp/mysocket" 0 "(symbols)")'
//
//  then use nclocal:
//    echo '(symbols)(exit)' | ./nclocal /tmp/mysocket
//
//  for multiline send a [cmd] before and a [/cmd] after the code
//  each on an extra line.
//      
// Copyright (C) 2007-2010 Lutz Mueller <lutz@nuevatec.com>
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 2, 1991,
// as published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

int main(int argc, char * argv[])
{
int s, t, len;
struct sockaddr_un remote_sun;
char str[102];
char * sock_path;

if(argc < 2)
	{
	printf("nclocal - (c) Lutz Mueller, 2009\n");
	printf("Send stdin to <socket-path> and output response to stdout\n\n");
	printf("USAGE: nclocal <socket-path> < message-file\n");
	exit(0);
	}

sock_path = argv[1];

if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
	{
	perror("socket");
	exit(1);
	}

remote_sun.sun_family = AF_UNIX;
strncpy(remote_sun.sun_path, sock_path, sizeof(remote_sun.sun_path) - 1);
remote_sun.sun_path[sizeof (remote_sun.sun_path) - 1] = '\0';

if (connect(s, (struct sockaddr *)&remote_sun, SUN_LEN(&remote_sun)) == -1)
	{
	perror("connect");
	exit(1);
	}

while(fgets(str, 100, stdin), !feof(stdin)) 
	{
	if (send(s, str, strlen(str), 0) == -1) 
		{
		perror("send");
		exit(1);
		}
	}

while((t = recv(s, str, 100, 0)) > 0)
	{
	str[t] = '\0';
	printf("%s", str);
	}

if(t < 0) perror("recv");

close(s);
return 0;
}

