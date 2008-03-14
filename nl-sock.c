/* nl-sock.c

    Copyright (C) 2008 Lutz Mueller

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/* this file is in the process of beeing ifdeffed to IPv6, but not completed yet

in the makefile_xxx add -DIPV6 in the CC compile flags

*/

#ifdef IPV6
#define ADDR_TYPE AF_INET6
#define ICMP_TYPE IPPROTO_ICMPV6
#else
#define ADDR_TYPE AF_INET
#define ICMP_TYPE IPPROTO_ICMP
#endif

#include "newlisp.h"
#include <string.h>

#ifdef WIN_32
#include <winsock2.h>
#include <ws2tcpip.h>
#define fdopen win32_fdopen
#define SHUT_RDWR 2
#define gethostbyname2(A, B) gethostbyname(A)
#else
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <netinet/icmp6.h>
#include <netdb.h>
#include <arpa/inet.h>
#endif

#ifdef SOLARIS
#include <stropts.h>
#include <sys/conf.h>
#include <netinet/in_systm.h>
#define gethostbyname2(A, B) gethostbyname(A)
#ifndef TRU64
#define FIONREAD I_NREAD
#endif
#endif

#ifdef OS2 
#define socklen_t int 
#define SHUT_RDWR 2 
#endif 

#ifndef INADDR_NONE
#define INADDR_NONE (unsigned) -1
#endif

#ifndef AF_UNIX
#define AF_UNIX PF_UNIX
#endif

#ifndef SUN_LEN
#define SUN_LEN(su) \
    (sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path))
#endif

#include "protos.h"

#define MAX_PENDING_CONNECTS 128
#define NO_FLAGS_SET 0

#ifdef WIN_32
#define socklen_t int
#define close closesocket
#else
#define SOCKET_ERROR -1
#define INVALID_SOCKET -1
#endif


#define ERR_INET_OPEN_SOCKET 1
#define ERR_INET_HOST_UNKNOWN 2
#define ERR_INET_INVALID_SERVICE 3
#define ERR_INET_CONNECT_FAILED 4
#define ERR_INET_ACCEPT 5
#define ERR_INET_CONNECTION_DROPPED 6
#define ERR_INET_CONNECTION_BROKEN 7
#define ERR_INET_READ 8
#define ERR_INET_WRITE 9
#define ERR_INET_CANNOT_BIND 10
#define ERR_INET_TOO_MUCH_SOCKETS 11
#define ERR_INET_LISTEN_FAILED 12
#define ERR_INET_BAD_FORMED_IP 13
#define ERR_INET_SELECT_FAILED 14
#define ERR_INET_PEEK_FAILED 15
#define ERR_INET_NOT_VALID_SOCKET 16
#define ERR_INET_TIMEOUT 17
#define MAX_NET_ERROR 17

#define isnum(A) ((A)>= '0' && (A) <= '9')

typedef struct
    {
    int socket;
	int family;
    void * next;
    } INET_SESSION;

INET_SESSION * netSessions = NULL;

int deleteInetSession(int sock);
int getSocketFamily(int sock);

#define READY_READ 0
#define READY_WRITE 1

int errorIdx = 0;

extern int logTraffic;
extern int noPromptMode;

/* with MinGW gcc 3.4.5 not needed
#ifdef WIN_32
struct timezone {
       int     tz_minuteswest;
       int     tz_dsttime;
};
int gettimeofday(struct timeval * tv, struct timezone * tz);
#endif
*/


/********************** session functions *******************/

int createInetSession(int sock, int family)
{
INET_SESSION * iSession;

iSession = (INET_SESSION *)malloc(sizeof(INET_SESSION));

iSession->socket = sock;
iSession->family = family;

if(netSessions == NULL)
    {
    netSessions = iSession;
    iSession->next = NULL;
    }
else
    {
    iSession->next = netSessions;
    netSessions = iSession;
    }
return(TRUE);
}

int deleteInetSession(int sock)
{
INET_SESSION * session;
INET_SESSION * previous;

if(netSessions == NULL)
    return(0);
else
    session = previous = netSessions;

while(session)
    {
    if(session->socket == sock)
        {
        if(session == netSessions)
            netSessions = session->next;
        else
            previous->next = session->next;
        free((char *)session);
        return(TRUE);
        }
    previous = session;
    session = session->next;
    }

return(FALSE);
}


int getSocketFamily(int sock)
{
INET_SESSION * session;

session = netSessions;

while(session)
    {
	if(session->socket == sock)
		return(session->family);
    session = session->next;
    }

return(-1);
}

/********************* user functions **************************/


CELL * p_netClose(CELL * params) 
{
UINT sock; 
 
getInteger(params, &sock); 
deleteInetSession((int)sock);

if(!getFlag(params->next))
	shutdown(sock, SHUT_RDWR);

if(close((int)sock) == SOCKET_ERROR)
    return(netError(ERR_INET_NOT_VALID_SOCKET));

errorIdx = 0;
return(trueCell);
}


CELL * p_netSessions(CELL * params)
{
INET_SESSION * session;
INET_SESSION * sPtr;
CELL * sList;
CELL * last;

session = netSessions;
sList = getCell(CELL_EXPRESSION);
last = NULL;

while(session)
    {
    sPtr = session;
    session = session->next;
    if(last == NULL)
        {
        last = stuffInteger(sPtr->socket);
        sList->contents = (UINT)last;
        }
    else
        {
        last->next = stuffInteger(sPtr->socket);
        last = last->next;
        }
    }

return(sList);
}



/*********************************************************************/

CELL * p_netService(CELL * params) 
{
struct servent * pSe; 
char * service; 
char * protocol; 
int port; 
 
params = getString(params, &service); 
getString(params, &protocol); 
 
if((pSe = getservbyname(service, protocol)) == NULL) 
    return(netError(ERR_INET_INVALID_SERVICE)); 
 
port = (int)ntohs(pSe->s_port); 
 
errorIdx = 0; 
return(stuffInteger((UINT)port)); 
} 


CELL * p_netConnect(CELL * params)
{
UINT ttl = 3;
char * remoteHostName; 
int type; 
UINT portNo;
char * protocol = NULL;
int sock;

params = getString(params, &remoteHostName); 
#ifndef WIN_32
if(params == nilCell)
	{
	if((sock = netConnectLocal(remoteHostName)) == SOCKET_ERROR)
		return(netError(errorIdx));
	else	
		return(stuffInteger((UINT)sock));
	}
#endif

params = getInteger(params, &portNo);

type = SOCK_STREAM;
if(params != nilCell)
    {
    params = getString(params, &protocol);
    *protocol = toupper(*protocol);
    type = SOCK_DGRAM;
    if(*protocol == 'M')
        {
        if(params != nilCell)
            getInteger(params, &ttl);
        }
    }

if((sock = netConnect(remoteHostName, (int)portNo, type, protocol, (int)ttl)) == SOCKET_ERROR)
    return(netError(errorIdx));

createInetSession(sock, AF_INET);

errorIdx = 0;
return(stuffInteger((UINT)sock)); 
}


#ifndef WIN_32
/* create local domain UNIX socket */
int netConnectLocal(char * path)
{
int sock;
struct sockaddr_un remote_sun;

if((sock = socket(AF_UNIX, SOCK_STREAM, 0)) == SOCKET_ERROR)
	{
	errorIdx = ERR_INET_OPEN_SOCKET;
	return(SOCKET_ERROR);
	}
	
remote_sun.sun_family = AF_UNIX;
strncpy(remote_sun.sun_path, path, sizeof(remote_sun.sun_path) - 1);
remote_sun.sun_path[sizeof (remote_sun.sun_path) - 1] = '\0';
if (connect(sock, (struct sockaddr *)&remote_sun, SUN_LEN(&remote_sun)) == -1) 
	{
	close(sock);
	errorIdx = ERR_INET_CONNECT_FAILED;
	return(SOCKET_ERROR);
    }

createInetSession(sock, AF_UNIX);

errorIdx = 0;
return(sock); 
}
#endif

/* create internet socket */
int netConnect(char * remoteHostName, int portNo, int type, char * prot, int ttl)
{
#ifdef IPV6
struct sockaddr_in6 dest_sin;
struct in6_addr iaddr;
#else
struct sockaddr_in dest_sin;
struct in_addr iaddr;
#endif
struct hostent * pHe;
int sock, idx;
/* char opt; */
int opt;

/* create socket */
if((sock = socket(ADDR_TYPE, type, 0)) == INVALID_SOCKET)
    {
    errorIdx = ERR_INET_OPEN_SOCKET;
    return(SOCKET_ERROR);
    }

if(prot != NULL) if(*prot == 'M' || *prot == 'B')
    {
    memset(&iaddr, 0, sizeof(iaddr));
#ifdef IPV6
	/* iaddr = IN6ADDR_ANY_INIT; */
	iaddr = in6addr_any;
#else
    iaddr.s_addr = INADDR_ANY;
#endif

    if(*prot == 'M')
        {
        setsockopt(sock, 0, IP_MULTICAST_IF, (const void *)&iaddr, sizeof(iaddr));
        opt = ttl;
        setsockopt(sock, 0, IP_MULTICAST_TTL, (const void *)&opt, sizeof(opt));
        }

    if(*prot == 'B')
        {
        opt = 1;
        setsockopt(sock, SOL_SOCKET, SO_BROADCAST, (const void *)&opt, sizeof(opt));
        }
    }

if((pHe = gethostbyname2(remoteHostName, ADDR_TYPE)) == NULL)
    {
    errorIdx = ERR_INET_HOST_UNKNOWN;
    return(SOCKET_ERROR);
    }

for(idx = 0; ; idx++)
    {
#ifdef IPV6
    memcpy((char *)&(dest_sin.sin6_addr), pHe->h_addr_list[idx], pHe->h_length);
    dest_sin.sin6_port = htons((u_short)portNo);
    dest_sin.sin6_family = AF_INET6;
#else
    memcpy((char *)&(dest_sin.sin_addr), pHe->h_addr_list[idx], pHe->h_length);
    dest_sin.sin_port = htons((u_short)portNo);
    dest_sin.sin_family = AF_INET;
	memset(&(dest_sin.sin_zero), '\0', sizeof(dest_sin.sin_zero));
#endif
    if(connect(sock,(struct sockaddr *)&dest_sin, sizeof(dest_sin)) == 0)
        break;

    if(pHe->h_addr_list[idx+1] != NULL)
        continue;

    close(sock);
    errorIdx = ERR_INET_CONNECT_FAILED;
    return(SOCKET_ERROR);
    }

errorIdx = 0;
return(sock);
}

/********* should be called after listen/accept notification **********/

CELL * p_netAccept(CELL * params) 
{
int sock;
UINT listenSock;

getInteger(params, &listenSock); 

if((sock = netAccept((int)listenSock)) == INVALID_SOCKET)
    return(netError(ERR_INET_ACCEPT));

return(stuffInteger(sock)); 
}


int netAccept(int listenSock)
{
int sock, family;
#ifdef IPV6
struct sockaddr_in6 dest_sin;
#else
struct sockaddr_in dest_sin;
#endif
#ifndef WIN_32
struct sockaddr_un dest_sun;
#endif
socklen_t dest_slen;

family = getSocketFamily(listenSock);

#ifndef WIN_32
if(family == AF_UNIX)
	{
	dest_slen = sizeof(struct sockaddr_un);
	sock = accept(listenSock, (struct sockaddr *) &dest_sun,  &dest_slen);
	}
else
#endif
	{
#ifdef IPV6
	dest_slen = sizeof(struct sockaddr_in6);
#else
	dest_slen = sizeof(struct sockaddr_in);
#endif
	sock = accept(listenSock, (struct sockaddr *) &dest_sin, (void *)&dest_slen);
	}

if(sock != INVALID_SOCKET) 
    {
    createInetSession(sock, family);
    errorIdx = 0;
    }

return(sock);
}


/******************* returns remote IP and port number *************/

#define LOCAL_INFO 0
#define PEER_INFO 1


int getPeerName(int sock, int peerLocalFlag, char * IPaddress)
{
socklen_t address_sin_len; 
#ifdef IPV6
struct sockaddr_in6 address_sin; 
#else
struct sockaddr_in address_sin; 
#endif

if(getSocketFamily(sock) == AF_UNIX)
	{
	snprintf(IPaddress, 6, "local");
	return(0);
	}

*IPaddress = 0;
address_sin_len = sizeof(address_sin); 
if(peerLocalFlag == LOCAL_INFO) 
    { 
    if(getsockname(sock,  
        (struct sockaddr *)&address_sin, (void *)&address_sin_len) 
            == SOCKET_ERROR) 
        return(0); 
    } 
else 
    { 
    if(getpeername(sock,  
        (struct sockaddr *)&address_sin, (void *)&address_sin_len) 
            == SOCKET_ERROR) 
        return(0); 
    } 
 
/* return address IP number  */
#ifdef IPV6
inet_ntop(AF_INET6, &address_sin.sin6_addr, IPaddress, 40); 
return(ntohs(address_sin.sin6_port));
#else
snprintf(IPaddress, 16, inet_ntoa(address_sin.sin_addr)); 
return(ntohs(address_sin.sin_port));
#endif
}


CELL * p_netLocal(CELL * params) 
{
return(netPeerLocal(params, LOCAL_INFO));
}

CELL * p_netPeer(CELL * params)
{
return(netPeerLocal(params, PEER_INFO));
}


CELL * netPeerLocal(CELL * params, int peerLocalFlag) 
{ 
CELL * result; 
CELL * cell; 
#ifdef IPV6
char name[40];
#else
char name[16];
#endif
UINT addressPort, sock;

getInteger(params, &sock); 
if((addressPort = getPeerName((int)sock, peerLocalFlag, name)) == SOCKET_ERROR)
    return(nilCell);
 
result = getCell(CELL_EXPRESSION); 
result->contents = (UINT)stuffString(name); 
cell = (CELL *)result->contents; 
cell->next = stuffInteger((UINT)addressPort); 
 
errorIdx = 0; 
return(result); 
} 
 

CELL * p_netLookup(CELL * params)
{
#ifdef IPV6
struct sockaddr_in6 address;
#else
union ipSpec 
    {
    unsigned int no;
    unsigned char chr[4];
    } ip;

struct sockaddr_in address;
#endif
struct hostent * pHe;
char * hostString;
char IPaddress[16];
int forceByName = 0;

params = getString(params, &hostString);
forceByName = getFlag(params);

/* get hostname from ip-number */
#ifdef IPV6
if((isDigit((unsigned char)*hostString) || (hostString[0] == ':')) && !forceByName)
	{
	if(inet_pton(AF_INET6, hostString, &address.sin6_addr) == 0)
        return(netError(ERR_INET_BAD_FORMED_IP));

	if((pHe = gethostbyaddr(&address.sin6_addr, AF_INET6, PF_INET6)) == NULL)
		return(nilCell);

    return(stuffString((char *)pHe->h_name));
	}
#else
if(isDigit((unsigned char)*hostString) && !forceByName)
    {
    if((ip.no = inet_addr(hostString)) == INADDR_NONE)
        return(netError(ERR_INET_BAD_FORMED_IP));

    if((pHe = gethostbyaddr((char *) &ip.no,4,PF_INET)) == NULL)
        return(nilCell);

    return(stuffString((char *)pHe->h_name));
    }
#endif

/* get ip-number from hostname */
if((pHe = gethostbyname2(hostString, ADDR_TYPE)) == NULL)
    return(nilCell);

#ifdef IPV6
memcpy((char *)&(address.sin6_addr), pHe->h_addr_list[0], pHe->h_length);
inet_ntop(AF_INET6, &address.sin6_addr, IPaddress, 40); 
#else
memcpy((char *)&(address.sin_addr), pHe->h_addr_list[0], pHe->h_length);
snprintf(IPaddress, 16, inet_ntoa(address.sin_addr));
#endif

errorIdx = 0;
return(stuffString(IPaddress));
}

CELL * netReceive(int sock, SYMBOL * readSymbol, size_t readSize, CELL * params);

CELL * p_netReceive(CELL * params) 
{ 
UINT sock;
SYMBOL * readSymbol;
size_t readSize;

params = getInteger(params, &sock);
params = getSymbol(params, &readSymbol);
params = getInteger(params, (UINT *)&readSize);

return(netReceive((int)sock, readSymbol, readSize, params));
}

CELL * netReceive(int sock, SYMBOL * readSymbol, size_t readSize, CELL * params)
{
char * waitFor;
ssize_t bytesReceived;
size_t length;
int found;
STREAM netStream;
char chr;
CELL * cell;

if(isProtected(readSymbol->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(readSymbol)));

memset(&netStream, 0, sizeof(netStream));

if(params == nilCell)
    {
    openStrStream(&netStream, readSize, 0);
    found = 1;
    bytesReceived  = recv(sock, netStream.buffer, readSize, NO_FLAGS_SET);
    }
else
    {
    getString(params, &waitFor);
    openStrStream(&netStream, MAX_LINE, 0);
    found = bytesReceived = 0;
    length = strlen(waitFor);
    while(bytesReceived < (int)readSize)
        {
        if(recv(sock, &chr, 1, NO_FLAGS_SET) <= 0)
            {
            bytesReceived = 0;
            break;
            }
        writeStreamChar(&netStream, chr); 
         if(++bytesReceived < length) continue;
        if(strncmp(waitFor,  netStream.ptr - length, length) == 0)
             {
            found = 1;
            break;
            }        
        }
            
    }

if(bytesReceived == 0 || found == 0) 
    { 
    closeStrStream(&netStream); 
    deleteInetSession(sock); 
    close(sock); 
    return(netError(ERR_INET_CONNECTION_DROPPED)); 
    } 

if(bytesReceived == SOCKET_ERROR) 
    { 
    closeStrStream(&netStream);         
    deleteInetSession(sock); 
    close(sock); 
    return(netError(ERR_INET_READ)); 
    } 
  
cell = stuffStringN(netStream.buffer, bytesReceived);
closeStrStream(&netStream);

deleteList((CELL *)readSymbol->contents); 
readSymbol->contents = (UINT)cell; 
 
errorIdx = 0; 
return(stuffInteger(bytesReceived)); 
} 



CELL * netReceiveFrom(int sock, size_t readSize, int closeFlag)
{
int portNo;
char * buffer;
ssize_t bytesReceived;
#ifdef IPV6
struct sockaddr_in6 remote_sin;
#else
struct sockaddr_in remote_sin;
#endif
CELL * cell;
CELL * result;
#ifdef WIN_32
int remote_sin_len;
#else
#ifdef TRU64
unsigned long remote_sin_len;
#else
socklen_t remote_sin_len;
#endif
#endif
char IPaddress[16];

buffer = (char *)allocMemory(readSize + 1);
remote_sin_len = sizeof(remote_sin);
memset(&remote_sin, 0, sizeof(remote_sin));

bytesReceived = recvfrom(sock, buffer, readSize, 0, 
    (struct sockaddr *)&remote_sin, &remote_sin_len);

if(bytesReceived == SOCKET_ERROR) 
    {
    freeMemory(buffer);
    close(sock); 
    return(netError(ERR_INET_READ)); 
    }

#ifdef IPV6
inet_ntop(AF_INET6, &remote_sin.sin6_addr, IPaddress, 40); 
portNo = ntohs(remote_sin.sin6_port);
#else
snprintf(IPaddress, 16, inet_ntoa(remote_sin.sin_addr)); 
portNo = ntohs(remote_sin.sin_port);
#endif


cell = result = getCell(CELL_EXPRESSION);
cell->contents = (UINT)stuffStringN(buffer, bytesReceived);
cell = (CELL *)cell->contents;
cell->next = stuffString(IPaddress);
((CELL*)cell->next)->next = stuffInteger(portNo);

freeMemory(buffer);

if(closeFlag) close(sock);

errorIdx = 0;
return(result);
}


CELL * p_netReceiveUDP(CELL * params) 
{ 
UINT portNo;
int sock;
size_t readSize;
INT64 wait = 0;
INT64 elapsed;
char * ifaddr = NULL;

params = getInteger(params, &portNo);
params = getInteger(params, (UINT *)&readSize);
if(params != nilCell)
    {
    params = getInteger64(params, &wait);
    if(params != nilCell)
        getString(params, &ifaddr);
    }

if((sock = netListenOrDatagram((int)portNo, SOCK_DGRAM, ifaddr, NULL)) == SOCKET_ERROR)
    return(nilCell);

/* if timeout parameter given wait for socket to be readable */
if(wait > 0)
        {
        if((elapsed = wait_ready(sock, wait, READY_READ)) <= 0)
                {
                close(sock);
				if(elapsed == 0) return(netError(ERR_INET_TIMEOUT));
				else netError(ERR_INET_SELECT_FAILED);
                }
        }

return(netReceiveFrom(sock, readSize, TRUE));
}


CELL * p_netReceiveFrom(CELL * params)
{
UINT sock;
size_t readSize;

params = getInteger(params, &sock);
getInteger(params, (UINT*)&readSize);

return(netReceiveFrom((int)sock, readSize, FALSE));
}


/**********************************************************************/

CELL * p_netSend(CELL * params) 
{
UINT sock; 
size_t size, size2; 
char * buffer; 
ssize_t bytesSent; 

params = getInteger(params, &sock); 
params = getStringSize(params, &buffer, &size, TRUE);

if(params->type != CELL_NIL) 
	{
	getInteger(params, (UINT *)&size2); 
	if(size2 < size) size = size2;
	}

if((bytesSent = sendall((int)sock, buffer, size))  == SOCKET_ERROR) 
    { 
    deleteInetSession((int)sock); 
    close((int)sock); 
    return(netError(ERR_INET_WRITE)); 
    }

errorIdx = 0; 
return(stuffInteger(bytesSent)); 
}

#define SEND_TO_UDP 0
#define SEND_TO_SOCK 1

CELL * netSendTo(CELL * params, int type)
{
char * remoteHost;
UINT remotePort;
#ifdef IPV6
struct sockaddr_in6 dest_sin;
#else
struct sockaddr_in dest_sin;
#endif
struct hostent * pHe;
size_t size;
char * buffer;
ssize_t bytesSent;
UINT sock;
/* char one = 1; */
int one = 1;

params = getString(params, &remoteHost);
params = getInteger(params, &remotePort);
params = getStringSize(params, &buffer, &size, TRUE);

if((pHe = gethostbyname2(remoteHost, ADDR_TYPE)) == NULL)
        return(netError(ERR_INET_HOST_UNKNOWN));

if(type == SEND_TO_UDP) /* for 'net-send-udp' */
	{
    if((sock = socket(ADDR_TYPE, SOCK_DGRAM, 0)) == INVALID_SOCKET)
        return(netError(ERR_INET_OPEN_SOCKET));

     if(getFlag(params))
		setsockopt(sock, SOL_SOCKET, SO_BROADCAST, (const void *)&one, sizeof(one));
	}
else /* SEND_TO_SOCK , socket may or may not be UDP, for 'net-send-to' */
    {
    params = getInteger(params, &sock);
    }

#ifdef IPV6
memcpy((char *)&(dest_sin.sin6_addr), pHe->h_addr_list[0], pHe->h_length);
dest_sin.sin6_port = htons((u_short)remotePort);
dest_sin.sin6_family = AF_INET6;
#else
memcpy((char *)&(dest_sin.sin_addr), pHe->h_addr_list[0], pHe->h_length);
dest_sin.sin_port = htons((u_short)remotePort);
dest_sin.sin_family = AF_INET;
memset(&(dest_sin.sin_zero), '\0', 8);
#endif

bytesSent = sendto((int)sock, buffer, size, NO_FLAGS_SET,
        (struct sockaddr *)&dest_sin, sizeof(dest_sin));

if(type == SEND_TO_UDP) close((int)sock);

if(bytesSent == SOCKET_ERROR)
        return(netError(ERR_INET_WRITE));

errorIdx = 0;
return(stuffInteger(bytesSent));
}


CELL * p_netSendUDP(CELL * params)
{
return(netSendTo(params, SEND_TO_UDP));
}


CELL * p_netSendTo(CELL * params)
{
return(netSendTo(params, SEND_TO_SOCK));
}


/************************* listen **************************************/

CELL * p_netListen(CELL * params) 
{ 
UINT portNo;
char * ifAddr = NULL;
char * option = NULL;
char * mcAddr = NULL;
int sock, type; 
CELL * cell;
 
type = SOCK_STREAM;
cell = evaluateExpression(params);
params = params->next;

#ifndef WIN_32
if(cell->type == CELL_STRING)
	{
	if((sock = netListenLocal((char *)cell->contents)) == SOCKET_ERROR)
		return(netError(errorIdx));
	else
		return(stuffInteger((UINT)sock));
	}
#endif

getIntegerExt(cell, &portNo, FALSE); 

if(params != nilCell)
    {
    params = getString(params, &ifAddr);
    if(*ifAddr == 0) ifAddr = NULL;
    if(params != nilCell)
        {
        params = getString(params, &option);
        if(*option == 'u' || *option == 'U')
            type = SOCK_DGRAM;
        else if(*option == 'm' || *option == 'M')
            {
            type = SOCK_DGRAM;
            mcAddr = ifAddr;
            ifAddr = NULL;
            }
        }
    }
    

if((sock = netListenOrDatagram((int)portNo, type, ifAddr, mcAddr)) == SOCKET_ERROR)
    return(nilCell);

return(stuffInteger(sock));
} 

#ifndef WIN_32
int netListenLocal(char * name)
{
int sock;
struct sockaddr_un local_sun;

sock  = socket(AF_UNIX, SOCK_STREAM, 0);
local_sun.sun_family = AF_UNIX;
strncpy(local_sun.sun_path, name, sizeof(local_sun.sun_path) - 1);
local_sun.sun_path[sizeof (local_sun.sun_path) - 1] = '\0';
unlink(local_sun.sun_path);

#ifdef OS2
if(bind(sock, (struct sockaddr *)&local_sun, sizeof(struct sockaddr_un)) == -1)
#else
if(bind(sock, (struct sockaddr *)&local_sun, SUN_LEN(&local_sun)) != 0)
#endif
	{
	close(sock);
	errorIdx = ERR_INET_CANNOT_BIND;
	return(SOCKET_ERROR);
	}

if(listen(sock, MAX_PENDING_CONNECTS) == SOCKET_ERROR)
	{
	close(sock);
	errorIdx = ERR_INET_LISTEN_FAILED;
	return(SOCKET_ERROR);
	}

createInetSession(sock, AF_UNIX);

errorIdx = 0;
return(sock);
}
#endif


int netListenOrDatagram(int portNo, int type, char * ifAddr, char * mcAddr)
{
int sock; 
int one = 1;
/* char one = 1; */
#ifdef IPV6
struct sockaddr_in6 local_sin; 
#else
struct sockaddr_in local_sin; 
#endif
struct hostent * pHe;
struct ip_mreq mcast;

if((sock = socket(ADDR_TYPE, type, 0)) == INVALID_SOCKET)
    {
    errorIdx = ERR_INET_OPEN_SOCKET; 
    return SOCKET_ERROR;
    }

memset(&local_sin, 0, sizeof(local_sin));

if(ifAddr != NULL && *ifAddr != 0)
	{
	if((pHe = gethostbyname2(ifAddr, ADDR_TYPE)) == NULL)
		{
		errorIdx = ERR_INET_HOST_UNKNOWN;
		return(SOCKET_ERROR);
		}
#ifdef IPV6
	memcpy((char *)&(local_sin.sin6_addr), pHe->h_addr_list[0], pHe->h_length);
	}

else 
	local_sin.sin6_addr = in6addr_any;

#else
	memcpy((char *)&(local_sin.sin_addr), pHe->h_addr_list[0], pHe->h_length);
	}
else 
	local_sin.sin_addr.s_addr = INADDR_ANY; 
#endif

#ifdef IPV6
local_sin.sin6_port = htons((u_short)portNo); 
local_sin.sin6_family = AF_INET6; 
#else
local_sin.sin_port = htons((u_short)portNo); 
local_sin.sin_family = AF_INET; 
#endif

setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const void*)&one, sizeof(one));
 
if(bind(sock, (struct sockaddr *) &local_sin, sizeof(local_sin)) == SOCKET_ERROR) 
    { 
    close(sock);
    errorIdx = ERR_INET_CANNOT_BIND; 
    return(SOCKET_ERROR);
    } 

if(mcAddr != NULL)
    {
    memset(&mcast, 0, sizeof(mcast));
    mcast.imr_multiaddr.s_addr = inet_addr(mcAddr);
#ifdef IPV6
    /* mcast.imr_interface.s_addr = IN6ADDR_ANY_INIT;     */
#else
    mcast.imr_interface.s_addr = INADDR_ANY;    
#endif
    setsockopt(sock, 0, IP_ADD_MEMBERSHIP, (const void *)&mcast, sizeof(mcast));
    }

if(type == SOCK_STREAM)
	{
	if(listen(sock, MAX_PENDING_CONNECTS) == SOCKET_ERROR)  
		{ 
		close(sock); 
		errorIdx = ERR_INET_LISTEN_FAILED;
		return(SOCKET_ERROR);
		} 
	}

createInetSession(sock, AF_INET); 

errorIdx = 0;
return(sock);
}


/* returns number of bytes ready to read */
CELL * p_netPeek(CELL * params)
{
UINT sock;
#ifdef WIN_32
unsigned long result;
#else
int result;
#endif

getInteger(params, &sock);

if(ioctl((int)sock, FIONREAD, &result) == SOCKET_ERROR)
    return(netError(ERR_INET_PEEK_FAILED));

errorIdx = 0;
return(stuffInteger((UINT)result));
} 


typedef struct
    {
    int sock;
    void * next;
    } SOCKLIST;


/* checks a socket for readability/writeability  */
CELL * p_netSelect(CELL * params)
{
long value;
INT64 wait;
char * mode;
struct timeval timeOut;
fd_set socketSet;
SOCKLIST * sockPtr;
SOCKLIST * sockList = NULL;
CELL * cell;
CELL * list = NULL;
struct timeval* tmvPtr;

errorIdx = 0;

FD_ZERO(&socketSet);

cell = evaluateExpression(params);
if(isNumber(cell->type))
    getIntegerExt(cell, (UINT*)&value, FALSE);
else if(isList(cell->type))
    {
    cell = (CELL*)cell->contents;
    if(cell == nilCell) return(getCell(CELL_EXPRESSION));
    sockList = sockPtr = allocMemory(sizeof(SOCKLIST));
    sockPtr->sock = cell->contents;
    sockPtr->next = NULL;
    FD_SET(sockPtr->sock, &socketSet);
    value = 1;
    while((cell = cell->next) != nilCell)
        {
        sockPtr->next = allocMemory(sizeof(SOCKLIST));
        sockPtr = sockPtr->next;
        sockPtr->sock = cell->contents;        
        sockPtr->next = NULL;
        if(value == FD_SETSIZE)
            return(netError(ERR_INET_TOO_MUCH_SOCKETS));
        else value++;
        FD_SET(sockPtr->sock, &socketSet);
        }
    }
else return(errorProcExt(ERR_LIST_OR_NUMBER_EXPECTED, params));

params = getString(params->next, &mode);
getInteger64(params, &wait);

tmvPtr = (wait == -1) ? NULL : &timeOut;
timeOut.tv_sec = wait/1000000;
timeOut.tv_usec = wait - timeOut.tv_sec * 1000000;

if(sockList == NULL)
    {
    FD_SET((int)value, &socketSet);
    value = 1;
    }

/* printf("%d %d %d\n", timeOut.tv_sec, timeOut.tv_usec, sizeof(timeOut.tv_sec));  */

if(*mode == 'r')
    value = select(FD_SETSIZE, &socketSet, NULL, NULL, tmvPtr);
else if(*mode == 'w')
    value = select(FD_SETSIZE, NULL, &socketSet, NULL, tmvPtr);
else if(*mode == 'e')
    value = select(FD_SETSIZE, NULL, NULL, &socketSet, tmvPtr);

if(value >= 0)
    {
    if((sockPtr = sockList) == NULL)
        {
        if(value == 0) return(nilCell);
        else return(trueCell);
        }    

    cell = getCell(CELL_EXPRESSION);
    while(sockPtr != NULL)
        {
        if(FD_ISSET(sockPtr->sock, &socketSet))
            {
            if(list == NULL)
                {
                list = cell;
                cell->contents = (UINT)stuffInteger(sockPtr->sock);
                cell = (CELL *)cell->contents;
                }
            else
                {
                cell->next = stuffInteger(sockPtr->sock);
                cell = cell->next;
                }
            }
        sockPtr = sockPtr->next;
        free(sockList);
        sockList = sockPtr;
        }

    if(list == NULL) return(cell);
    else return(list);
    }

netError(ERR_INET_SELECT_FAILED);

if(sockList == NULL) return(nilCell);
return(getCell(CELL_EXPRESSION));
}

extern char logFile[];

void writeLog(char * text, int newLine)
{
int handle;

#ifdef WIN_32
handle = open(logFile, O_RDWR | O_APPEND | O_BINARY | O_CREAT, S_IREAD | S_IWRITE);
#else
handle = open(logFile, O_RDWR | O_APPEND | O_BINARY | O_CREAT,
          S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR | S_IWGRP | S_IWOTH); /* rw-rw-rw */
#endif

write(handle, text, strlen(text));
if(newLine) write(handle, &LINE_FEED, strlen(LINE_FEED));
close(handle);
}


FILE * serverFD(int port, char * domain, int reconnect)
{
static int sock, connection;
char name[18];
char text[80];
time_t t;

if(!reconnect)
	{
#ifndef WIN_32
	if(port != 0)
		sock = netListenOrDatagram(port, SOCK_STREAM, NULL, NULL);
	else
		sock = netListenLocal(domain);
#else
	sock = netListenOrDatagram(port, SOCK_STREAM, NULL, NULL);
#endif

	if(sock == SOCKET_ERROR)
		{
		snprintf(text, 78, "newLISP v.%d listening on %s", version, domain);
		writeLog(text, TRUE);
		return(NULL);
		}
	}
else
	{
	deleteInetSession(connection);
	close(connection); 
	}

if((connection = netAccept(sock)) == SOCKET_ERROR)
	return NULL;

createInetSession(connection, (port != 0) ? AF_INET : AF_UNIX);

/* print log */
getPeerName(connection, PEER_INFO, name);
t = time(NULL);
snprintf(text, 78, "Connected to %s on %s", name, ctime(&t));
/* printf(text); */
writeLog(text, 0);

return(fdopen(connection, "r+")); 
}

/******************************* distributed computing ***********************/

#define MAX_BUFF 1024
CELL * netEvalError(int errNo);

typedef struct
	{
	char * host;
	int port;
	int sock;
	int timeOut;
	STREAM * netStream;
	CELL * result;
	void * next;
	} NETEVAL;

void freeSessions(NETEVAL * base);

CELL * p_netEval(CELL * params)
{
CELL * cell = NULL;
CELL * list = NULL;
NETEVAL * session = NULL;
NETEVAL * base = NULL;
char * host;
char * prog;
UINT port;
int ready;
int sock;
size_t size, count = 0;
ssize_t bytes;
long timeOut = MAX_LONG;
int start, elapsed = 0;
CELL * result;
STREAM * netStream;
CELL * netEvalIdle = NULL;
char buffer[MAX_BUFF];
STREAM evalStream;
int rawMode = FALSE;
int singleSession = FALSE;
jmp_buf errorJumpSave;
int errNo;
int resultStackIdxSave;

list  = evaluateExpression(params);
if(list->type == CELL_STRING)
	{
	host = (char *)list->contents;
	params = getIntegerExt(params->next, &port, TRUE);
	params = getStringSize(params, &prog, &size, TRUE);
	list = nilCell;
	singleSession = TRUE;
	goto SINGLE_SESSION;
	}
else if(list->type == CELL_EXPRESSION)
	{
	list = (CELL*)list->contents;
	params = params->next;
	}
else return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, params));

CREATE_SESSION:
if(!isList(list->type))
    return(errorProcExt(ERR_LIST_EXPECTED, list));
cell = (CELL *)list->contents;

/* get node parameters, since 8.9.8 evaluated */
memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
if((errNo = setjmp(errorJump)) != 0)
	{
	memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
	freeSessions(base);
	longjmp(errorJump, errNo);
	}
cell = getStringSize(cell, &host, &size, TRUE);
cell = getInteger(cell, &port);
cell = getStringSize(cell, &prog, &size, TRUE);
rawMode = getFlag(cell);

memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));

SINGLE_SESSION:
if(base == NULL)
    {
    base = session = allocMemory(sizeof(NETEVAL));
    memset(base, 0, sizeof(NETEVAL));
    }
else
    {
    session->next = allocMemory(sizeof(NETEVAL));
    session = session->next;
    memset(session, 0, sizeof(NETEVAL));
    }

#ifndef WIN_32
if(port != 0)
	sock = netConnect(host, (int)port, SOCK_STREAM, NULL, 3);
else
	sock = netConnectLocal(host);
#else
sock = netConnect(host, (int)port, SOCK_STREAM, NULL, 3);
#endif

if(sock == SOCKET_ERROR)
		{
		session->result = netEvalError(errorIdx);
		goto CONTINUE_CREATE_SESSION;
		}
	
session->host = host;
session->port = port;
session->sock = sock;

if( sendall(sock, "[cmd]\n", 6)  == SOCKET_ERROR ||
    sendall(sock, prog, size) == SOCKET_ERROR ||
    sendall(sock, "(exit)\n[/cmd]\n", 14) == SOCKET_ERROR )
    { 
    close(sock); 
    session->result = netEvalError(ERR_INET_WRITE); 
    goto CONTINUE_CREATE_SESSION;
    }

session->netStream = (void *)allocMemory(sizeof(STREAM));
memset(session->netStream, 0, sizeof(STREAM));
openStrStream(session->netStream, MAX_BUFF, 0);
/* prepend quote for evaluation */
writeStreamChar(session->netStream, '\'');
createInetSession(sock, AF_INET);
count++;
CONTINUE_CREATE_SESSION:
list = list->next;
if(list != nilCell) goto CREATE_SESSION;

/* get timeout and optional handler symbol */
session = base;
if(params != nilCell)
	params = getInteger(params, (UINT *)&timeOut);
if(params != nilCell)
	netEvalIdle = params;
   
/* printf("timeout %d idle-loop %X\n", timeOut, netEvalIdle); */
 
/* collect data from host in each active session */
while(count)
    {
    resultStackIdxSave = resultStackIdx;
    if( (netStream = session->netStream) == NULL) 
        {
        session = session->next;
        if(session == NULL) session = base;
        continue;
        }
        
    start = milliSecTime();

    if(netEvalIdle) 
        {
        cell = getCell(CELL_EXPRESSION);
        cell->contents = (UINT)copyCell(netEvalIdle);
        pushResult(cell);
        if(!evaluateExpressionSafe(cell, &errNo))
			{
			freeSessions(base);
			longjmp(errorJump, errNo);
			}
        }

    bytes = -1;
    ready = wait_ready(session->sock, 100, READY_READ);
    if(ready > 0)
        {
        memset(buffer, 0, MAX_BUFF);
        bytes = recv(session->sock, buffer, MAX_BUFF, NO_FLAGS_SET);
        if(bytes) writeStreamStr(netStream, buffer, bytes);
        }
    if(ready < 0 || bytes == 0 || elapsed >= timeOut)
        {
/*        printf("count=%d ready=%d bytes=%d elapsed=%d\n", count, ready, bytes, elapsed); */
        if(elapsed >= timeOut) result = copyCell(nilCell); 
        else if(rawMode) /* get raw buffer without the quote */
            result = stuffStringN(netStream->buffer + 1, netStream->position - 1);
        else 
            {
            makeStreamFromString(&evalStream, netStream->buffer);
            memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
            if((errNo = setjmp(errorJump)) != 0)
                {
                memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
                freeSessions(base);
                longjmp(errorJump, errNo);
                }
            result = evaluateStream(&evalStream, 0, TRUE);
            memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
            }

        if(netEvalIdle)
            {
            session->result = cell = getCell(CELL_EXPRESSION);
            cell->contents = (UINT)stuffString(session->host);
            cell = (CELL *)cell->contents;
            cell->next = stuffInteger(session->port);
            cell = cell->next;
            cell->next = result;
            }
        else
            session->result = result;
            
        closeStrStream(netStream);
        deleteInetSession(session->sock);
        close(session->sock);
        free(netStream);
        session->netStream = NULL;
            
        if(netEvalIdle)
            {
            list = getCell(CELL_EXPRESSION);
            list->contents = (UINT)copyCell(netEvalIdle);
            cell = getCell(CELL_QUOTE);
            cell->contents = (UINT)session->result;
            ((CELL*)list->contents)->next = cell;
            pushResult(list);
        	if(!evaluateExpressionSafe(list, &errNo))
				{
				freeSessions(base);
				longjmp(errorJump, errNo);
				}
            }

        count--;
        }
        
    /* check for rollover at midnight */
    if(milliSecTime() >= start) 
        elapsed += milliSecTime() - start;
    else 
        elapsed += milliSecTime();

    session = session->next;
    if(session == NULL) session = base;

    cleanupResults(resultStackIdxSave);
  }

/* free all sessions and configure result */
result = NULL;
while(base != NULL)
    {
    if(netEvalIdle == NULL)
        {
        if(result == NULL)
            {
			if(singleSession)
				result = base->result;
			else
				{
            	result = getCell(CELL_EXPRESSION);
            	result->contents = (UINT)base->result;
            	cell = base->result;
            	}
			}
        else
            {
            cell->next = base->result;
            cell = cell->next;
            }
        }
    session = base;
    base = base->next;
    free(session);
    }

if(elapsed > timeOut) 
	errorIdx = ERR_INET_TIMEOUT;
else errorIdx = 0;
if(netEvalIdle == NULL) return(result);
return(trueCell);
}    


void freeSessions(NETEVAL * base)
{
NETEVAL * session;

while(base != NULL)
    {
    if(base->netStream != NULL)
        {
        if(base->result != NULL)
          deleteList(base->result);
        closeStrStream(base->netStream);
        deleteInetSession(base->sock);
        close(base->sock);
        free(base->netStream);
        base->netStream = NULL;
        }
    session = base;
    base = base->next;
    free(session);
    }
}

int sendall(int sock, char * buffer, int len)
{
int bytesSend = 0;
int n;

while(bytesSend < len)
	{
 	if((n = send(sock, buffer + bytesSend, len - bytesSend, NO_FLAGS_SET)) == SOCKET_ERROR)
		return(SOCKET_ERROR);
	bytesSend += n;
	}

return(bytesSend);
}

/*********************** error handling ***************************************/

char * netErrorMsg[] =
    {
    "No error",
    "Cannot open socket",
    "Host name not known",
    "Not a valid service",
    "Connection failed",
    "Accept failed",
    "Connection closed",
    "Connection broken",
    "Socket recv failed",
    "Socket send failed",
    "Cannot bind socket",
    "Too much sockets in net-select",
    "Listen failed",
    "Badly formed IP",
    "Select failed",
    "Peek failed",
    "Not a valid socket",
	"Operation timed out"
    };


CELL * netError(int errorNo) 
{ 
errorIdx = errorNo; 
return(nilCell); 
} 

CELL * netEvalError(int errorNo)
{ 
errorIdx = errorNo; 
return(p_netLastError(NULL));
}

CELL * p_netLastError(CELL * params)
{
CELL * cell;
char str[64];

if(errorIdx == 0 || errorIdx > MAX_NET_ERROR) return(nilCell);

cell = getCell(CELL_EXPRESSION);
cell->contents = (UINT)stuffInteger(errorIdx);
snprintf(str, 63, "ERR: %s", netErrorMsg[errorIdx]);
((CELL*)cell->contents)->next = stuffString(str);

return(cell);
}


#ifdef NET_PING
/* net-ping */


CELL * p_netPing(CELL * params)
{
CELL * address;
UINT maxwait = 1000, listmode = 0;
UINT flag = 0;
UINT count = 0;

address = evaluateExpression(params);
if(address->type == CELL_EXPRESSION)
	{
	address = (CELL *)address->contents;
	listmode = 1;
	}
else if(address->type != CELL_STRING)
  return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, address));

params = params->next;
if(params != nilCell)  
	{
	params = getInteger(params, &maxwait);
	if(params != nilCell)
	params = getInteger(params, &count);
	flag = getFlag(params);
	}

return(ping(address, (int)maxwait, (int)listmode, (int)count, (int)flag));
}

#if defined(IPV6)
/* datalen 56 + iplen 60 + icmplen 76 */
#define PLEN 192 /* 192 or 64 ? */
#else
#define PLEN 64
#endif

CELL * ping(CELL * address, int maxwait, int listmode, int maxCount, int flag)
{
char * host;
char * ptr;
char * hostaddr = NULL;
struct hostent *hp;
unsigned char packet[PLEN];
struct ip *ip;
#ifdef IPV6
struct icmp6_hdr *icp = (struct icmp6_hdr *) packet;
struct sockaddr_in6 whereto;
struct sockaddr_in6 from;
struct icmp6_filter filter;
char IPaddress[40];
#else
struct sockaddr_in whereto;
struct sockaddr_in from;
struct icmp *icp = (struct icmp *) packet;
#endif
int s, sockopt;
#ifdef TRU64
unsigned long fromlen;
#else
#ifdef OS2
int fromlen;
#else
unsigned int fromlen;
#endif
#endif
int broadcast = 0;
int size, ipNo, startIp = 0, endIp = 0;
int timeout = 0, tdiff;
int sendCount = 0, receiveCount = 0, ping_sequence = 0;
size_t len;
struct timeval tv, tp;
CELL * result = NULL;
CELL * link = NULL;
char buff[64];

if ((s = socket(ADDR_TYPE, SOCK_RAW, ICMP_TYPE)) < 0) 
    return(netError(ERR_INET_OPEN_SOCKET));
   
#ifdef IPV6
ICMP6_FILTER_SETPASS (ICMP6_ECHO_REPLY, &filter);
setsockopt (s, IPPROTO_ICMPV6, ICMP6_FILTER, &filter, sizeof (filter));
sockopt = 1;
/* setsockopt (s, IPPROTO_IPV6, IPV6_RECVHOPLIMIT, &sockopt, sizeof (sockopt)); */
#endif

gettimeofday(&tv, NULL );

/* for each list-IP */
while(address != nilCell)
	{
	mySleep(1);
	if(address->type != CELL_STRING)
		{
		shutdown(s, SHUT_RDWR);
		return(errorProcExt(ERR_STRING_EXPECTED, address));
		}

	host = (char *)address->contents;
	len = address->aux - 1;

#ifndef IPv6
	if(strncmp(host + len - 2, ".*", 2) == 0)
		{
		startIp = 1;
		endIp = 254;
		len--;
		}
	else
		{
		startIp = endIp = 0;
		ptr = host + len - 1;
		while(isdigit((int)*ptr)) --ptr;
		if(*ptr == '-')
			{
			endIp = atoi(ptr + 1);
			--ptr;
			while(isdigit((int)*ptr)) --ptr;
			if(*ptr == '.')
				{
				startIp = atoi(ptr + 1);
				len = ptr - host + 1;
				}
			else endIp = startIp = 0;
			if(endIp < startIp) endIp = startIp;	
			}
		}
#endif

	/* ping ip range */
	for(ipNo = startIp; ipNo <= endIp; ipNo++)
        {
		if(startIp)
			{
        	if(hostaddr == NULL) hostaddr = alloca(len + 4);
        	memcpy(hostaddr, host, len);
        	snprintf(hostaddr + len, 4, "%d", ipNo);
			}
    	else
       		hostaddr = host;
        
		/* target host address info */
		/* printf("->%s\n", hostaddr); */
#ifdef IPV6
		memset((char *)&whereto, 0, sizeof(struct sockaddr_in6));
		whereto.sin6_family = AF_INET6;
		if(!(hp = gethostbyname2(hostaddr, AF_INET6)))
			{
			shutdown(s, SHUT_RDWR);
			return(netError(ERR_INET_HOST_UNKNOWN));
			}
		memcpy((char *)&(whereto.sin6_addr), hp->h_addr_list[0], hp->h_length);
#else
		memset((char *)&whereto, 0, sizeof(struct sockaddr_in));
		whereto.sin_family = AF_INET;
		if(!(hp = gethostbyname2(hostaddr, AF_INET)))
			{
			shutdown(s, SHUT_RDWR);
			return(netError(ERR_INET_HOST_UNKNOWN));
			}
		memcpy((void *)&whereto.sin_addr, hp->h_addr, hp->h_length);
		broadcast = ((whereto.sin_addr.s_addr & 0x000000ff) == 255);
#endif
		if(broadcast)
			{
			sockopt = 1;
			setsockopt(s, SOL_SOCKET, SO_BROADCAST, (void *) &sockopt, sizeof(sockopt));
			}

		/* ping */
		memset(icp, 0, PLEN);
#ifdef IPV6
		icp->icmp6_type = ICMP6_ECHO_REQUEST;
		icp->icmp6_code = 0;
		icp->icmp6_cksum = 0;
		icp->icmp6_seq = ping_sequence;
		icp->icmp6_id = getpid() & 0xFFFF; 
		gettimeofday((struct timeval *)&icp->icmp6_data8[4], NULL);
/*
		sockopt = offsetof(struct icmp6_hdr, icmp6_cksum);
		setsockopt(s, SOL_RAW, IPV6_CHECKSUM, (char *)&sockopt, sizeof(sockopt)); 
		sockopt = 1;
 		setsockopt(pingsock, SOL_IPV6, IPV6_HOPLIMIT, (char *)&sockopt, sizeof(sockopt));
*/

#else
		icp->icmp_type = ICMP_ECHO;
		icp->icmp_code = 0;
		icp->icmp_cksum = 0;
		icp->icmp_seq = ping_sequence;
		icp->icmp_id = getpid() & 0xFFFF; 
		gettimeofday((struct timeval *)&icp[1], NULL);
		icp->icmp_cksum = in_cksum((unsigned short *) icp, PLEN );
#endif
		while(wait_ready(s, 10000, READY_WRITE) <= 0)
			{
        	gettimeofday(&tp, NULL);
        	if((timeout = (timediff(tp, tv) > maxwait))) break;
        	continue;
			}
			
		size = sendto(s, packet, PLEN, 0,(struct sockaddr *)&whereto, sizeof(struct sockaddr));
		if(size != PLEN)
			{
			if(errno == EHOSTUNREACH || errno == EHOSTDOWN) 
				{
				if(flag)
					{
					snprintf(buff, 16, "errno: %d", errno);
#ifdef IPV6
					memset(IPaddress, 0, 40);
    	  			inet_ntop(AF_INET6, &whereto.sin6_addr, IPaddress, 40); 
					link = addResult(&result, link, 
						makePair(stuffString(IPaddress), stuffString(buff)));
#else
					link = addResult(&result, link, 
						makePair(stuffString(inet_ntoa(whereto.sin_addr)), stuffString(buff)));
#endif
					}
    			if( !(listmode || startIp) ) break;
				continue;
				}
			shutdown(s, SHUT_RDWR);    
			return(netError(ERR_INET_WRITE));
			}

		sendCount++;
		}
	if(!listmode) break;
	address = address->next;
	}

/* wait for response(s) */
if(maxCount == 0) maxCount = sendCount;
while(sendCount) 
	{
    memset(packet, 0, PLEN);
    fromlen = sizeof(from);

    if(wait_ready(s, 1000, READY_READ) <= 0)
        {
        gettimeofday(&tp, NULL);
       	if((timeout = (timediff(tp, tv) > maxwait))) break;
        continue;
        }    

    if ( (len = recvfrom(s, packet, PLEN, 0, (struct sockaddr *)&from, &fromlen)) < 0)
        continue;
    
    ip = (struct ip *) packet;
	gettimeofday(&tp, NULL);
#ifdef IPV6
    icp = (struct icmp6_hdr *)packet;
	if(icp->icmp6_type != ICMP6_ECHO_REPLY) continue;
    if(icp->icmp6_id != (getpid() & 0xFFFF)) continue; 
	tdiff = timediff64(tp, *(struct timeval *)&icp->icmp6_data8[4]);
#else
    icp = (struct icmp *)(packet + (ip->ip_hl << 2));
    if(icp->icmp_id != (getpid() & 0xFFFF)) continue; 
	tdiff = timediff64(tp, *(struct timeval *)&icp[1]);
#endif

#ifdef IPV6
	inet_ntop(AF_INET6, &from.sin6_addr, IPaddress, 40); 
	link = addResult(&result, link, 
			makePair(stuffString(IPaddress), stuffInteger(tdiff)));
#else
	link = addResult(&result, link, 
			makePair(stuffString(inet_ntoa(from.sin_addr)), stuffInteger(tdiff)));
#endif

    if(++receiveCount == maxCount) break;
    if( !(broadcast || listmode || startIp) ) break;
	} 

shutdown(s, SHUT_RDWR); 
ping_sequence++;
if(timeout) errorIdx = ERR_INET_TIMEOUT;
else errorIdx = 0;
return(result == NULL ? getCell(CELL_EXPRESSION) : result);
}

CELL * addResult(CELL * * result, CELL * cell, CELL * new)
{
if(*result == NULL)
	{
	*result = getCell(CELL_EXPRESSION);
	((CELL *)(*result))->contents = (UINT)new;
	}
else
	cell->next = new;	

return(new);
}

CELL * makePair(CELL * left, CELL * right)
{
CELL * cell = getCell(CELL_EXPRESSION);

cell->contents = (UINT)left;
left->next = right;

return(cell);
}

int in_cksum(unsigned short * addr, int len)
{
int nleft = len;
unsigned short *w = addr;
unsigned short  answer;
int sum = 0;

while( nleft > 1 )  {
    sum += *w++;
    nleft -= 2;
    }

if( nleft == 1 ) {
    u_short    u = 0;
    *(unsigned char *)(&u) = *(unsigned char *)w ;
    sum += u;
    }

sum = (sum >> 16) + (sum & 0xffff);
sum += (sum >> 16);
answer = ~sum;
return (answer);
}

#endif /* NET_PING */


/* check socket for readability or error 
   return 0 if the time limit expires or -1
   on error 
*/
int wait_ready(int sock, INT64 wait, int mode)
{
struct timeval timeOut;
fd_set socketSet;

FD_ZERO(&socketSet);
FD_SET(sock, &socketSet);

timeOut.tv_sec = wait/1000000;
timeOut.tv_usec = wait - timeOut.tv_sec * 1000000;

if(mode == READY_READ)
    return(select(FD_SETSIZE, &socketSet, NULL, &socketSet, &timeOut));
else
    return(select(FD_SETSIZE, NULL, &socketSet, &socketSet, &timeOut));
}


/* ----------------------------- socket->filestream stuff for win32 ------------------------*/

#ifdef WIN_32

/*
These functions use the FILE structure to store the raw file handle in '->_file' and
set ->_flag to 0xFFFF, to identify this as a faked FILE structure.
*/

FILE * win32_fdopen(int handle, const char * mode)
{
FILE * fPtr;

if((fPtr = (FILE *)malloc(sizeof(FILE))) == NULL)
    return(NULL);

memset(fPtr, 0, sizeof(FILE));


#ifdef WIN_32
fPtr->_file = handle;
fPtr->_flag = 0xFFFF;
#endif

return(fPtr);
}

int win32_fclose(FILE * fPtr)
{
if(isSocketStream(fPtr))
   return(close(getSocket(fPtr)));


return(fclose(fPtr));
}


/* for a full fprintf with format string and parameters
   see version previous to 9.0.2
*/
int win32_fprintf(FILE * fPtr, char * buffer)
{
int pSize;

if(!isSocketStream(fPtr))
    return(fprintf(fPtr, buffer));

pSize = strlen(buffer);

if((pSize = sendall(getSocket(fPtr), buffer, pSize)) == SOCKET_ERROR)
     {
     close(getSocket(fPtr));
     return(-1);
     }

return(pSize);
}

int win32_fgetc(FILE * fPtr)
{
char chr;

if(!isSocketStream(fPtr))
    return(fgetc(fPtr));

if(recv(getSocket(fPtr), &chr, 1, NO_FLAGS_SET) <= 0)
    {
    close(getSocket(fPtr));
    return(-1);
    }

return(chr);
}


char * win32_fgets(char * buffer, int  size, FILE * fPtr)
{
int bytesReceived = 0;
char chr;

if(!isSocketStream(fPtr))
    return(fgets(buffer, size - 1, fPtr));

while(bytesReceived < size)
    {
    if(recv(getSocket(fPtr), &chr, 1, NO_FLAGS_SET) <= 0)
        {
        close(getSocket(fPtr));
        return(NULL);
        }

    *(buffer + bytesReceived++) = chr;

    if(chr == '\n') break;
    }

*(buffer + bytesReceived) = 0;

return(buffer);
}

#endif

/* eof */