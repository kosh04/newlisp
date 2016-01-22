/* nl-sock.c

    Copyright (C) 2016 Lutz Mueller

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



#include "newlisp.h"
#include <string.h>

#ifdef WINDOWS

#include <winsock2.h>
#pragma push_macro("UINT") 
#undef UINT /* avoid clash with newLISP UINT */
#include <ws2tcpip.h>
#pragma pop_macro("UINT")
#include <ws2spi.h>

#define fdopen win_fdopen
#define SHUT_RDWR 2
#define gethostbyname2(A, B) gethostbyname(A)

#else /* UNIX */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#define __FAVOR_BSD
#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <netinet/ip_icmp.h>
#ifndef OS2
#include <netinet/icmp6.h> 
#endif
#include <netdb.h>
#include <arpa/inet.h>

#ifndef IPPROTO_DIVERT
#define IPPROTO_DIVERT 254
#endif

/* Android needs it */
#ifndef ICMP6_FILTER
#define ICMP6_FILTER 1
#endif

#endif /* end UNIX */

#ifdef CYGWIN
#include <netinet/icmp6.h> /* not on Cygwin, get from other OS */

#define ICMP_ECHO 8

struct icmp
{
   unsigned char icmp_type;
   unsigned char icmp_code;
   unsigned short icmp_cksum;
   unsigned short icmp_id;
   unsigned short icmp_seq;
};
#endif /* end CYGWIN */

#if defined(SOLARIS) || defined(TRU64) || defined(AIX)
#include <stropts.h>
#include <sys/conf.h>
#include <netinet/in_systm.h>
#define gethostbyname2(A, B) gethostbyname(A)
#endif

#ifdef SOLARIS
#define FIONREAD I_NREAD
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

#ifdef WINDOWS
#define close closesocket /* for file operations on Windows use _close */
#else
#define SOCKET_ERROR -1
#define INVALID_SOCKET -1
#endif

#define isnum(A) ((A)>= '0' && (A) <= '9')

IO_SESSION * ioSessions = NULL;

int isSessionSocket(int sock);
int getSocketFamily(int sock);

#ifndef EMSCRIPTEN
#define READY_READ 0
#define READY_WRITE 1

UINT netErrorIdx = 0;

extern int logTraffic;
extern int noPromptMode;

int ADDR_FAMILY = AF_INET; /* the default is IPv4 */
int ICMP_TYPE;
#define STRADDR_LEN 40


/*  set the default interface and select IPv4 or IPv6 mode */

struct sockaddr * defaultIn = NULL;
void * defaultInAddr = NULL; /* either (struct in6_addr *) or (struct in_adr *) */
char * defaultInterface = NULL;
socklen_t defaultInLen;

extern int opsys;
void initDefaultInAddr()
{
struct sockaddr_in6 * address6;
struct sockaddr_in * address;

if(defaultIn != NULL) free(defaultIn);
defaultInLen = (ADDR_FAMILY == AF_INET6) ? 
    sizeof(struct sockaddr_in6) : sizeof(struct sockaddr_in);

defaultIn = callocMemory(defaultInLen);

if(ADDR_FAMILY == AF_INET6)
    {
    address6 = (struct sockaddr_in6 *)defaultIn;
    address6->sin6_addr = in6addr_any;
    defaultInAddr = &address6->sin6_addr;
    address6->sin6_family = AF_INET6;
    ICMP_TYPE = IPPROTO_ICMPV6;
    opsys |= 0x200;
    }
else
    {
    address = (struct sockaddr_in *)defaultIn;
    address->sin_addr.s_addr = INADDR_ANY;
    defaultInAddr = &address->sin_addr;
    address->sin_family = AF_INET;
    ICMP_TYPE = IPPROTO_ICMP;
    opsys &= ~0x200;
    }

}
#endif /* ifndef EMSCRIPTEN */

/********************** IO session functions *******************/

IO_SESSION * createIOsession(int handle, int family)
{
IO_SESSION * iSession;

iSession = (IO_SESSION *)calloc(sizeof(IO_SESSION), 1);

iSession->handle = handle;
iSession->family = family;

if(ioSessions == NULL)
    {
    ioSessions = iSession;
    iSession->next = NULL;
    }
else
    {
    iSession->next = ioSessions;
    ioSessions = iSession;
    }

return(iSession);
}

int deleteIOsession(int handle)
{
IO_SESSION * session;
IO_SESSION * previous;

if(ioSessions == NULL)
    return(0);
else
    session = previous = ioSessions;

while(session)
    {
    if(session->handle == handle)
        {
        if(session == ioSessions)
            ioSessions = session->next;
        else
            previous->next = session->next;
        if(session->stream != NULL)
            fclose(session->stream);
        else close(handle);
        free((char *)session);
        return(TRUE);
        }
    previous = session;
    session = session->next;
    }

return(FALSE);
}


int isSessionSocket(int sock)
{
IO_SESSION * session;

if(ioSessions == NULL)
    return(FALSE);

session = ioSessions;
while(session)
    {
    if(session->handle == sock)
        return(TRUE);
    session = session->next;
    }

return(FALSE);
}

int getSocketFamily(int sock)
{
IO_SESSION * session;

session = ioSessions;

while(session)
    {
    if(session->handle == sock)
        return(session->family);
    session = session->next;
    }

return(-1);
}


FILE * getIOstream(int handle)
{
IO_SESSION * session;

session = ioSessions;

while(session)
    {
    if(session->handle == handle)
        return(session->stream);
    session = session->next;
    }

return(NULL);
}

/* ========================= IO session functions end ===================== */
#ifndef EMSCRIPTEN
#ifdef WINDOWS
int ipstrFromSockAddr(struct sockaddr * addr, char * host, int len)
{
WSAAddressToString(addr, defaultInLen, NULL, host, (LPDWORD)&len);
return(TRUE);
}
#else /* UNIX */
int ipstrFromSockAddr(struct sockaddr * addr, char * host, int len)
{
struct sockaddr_in6 * saddr6 = (struct sockaddr_in6 *)addr;
struct sockaddr_in * saddr =(struct sockaddr_in *)addr;

if(addr->sa_family == AF_INET6)
    inet_ntop(AF_INET6, &saddr6->sin6_addr, host, len); 
else
    strncpy(host, inet_ntoa(saddr->sin_addr), len);
return(TRUE);
}
#endif

/* ANDROID and WINDOWS need this */
#ifndef in_port_t
#define in_port_t short int
#endif

in_port_t portFromSockAddr(struct sockaddr * addr)
{
in_port_t * portPtr = (in_port_t *)&addr->sa_data[0];

return(ntohs(*portPtr));
}


void setSockaddrPort(struct sockaddr * addr, in_port_t port)
{
in_port_t * portPtr = (in_port_t *)&addr->sa_data[0];

*portPtr = htons(port);
}

/********************* user functions **************************/


CELL * p_netClose(CELL * params) 
{
UINT sock; 
 
getInteger(params, &sock); 
if(!deleteIOsession((int)sock))
    return(netError(ERR_INET_NOT_VALID_SOCKET));

netErrorIdx = 0;
return(trueCell);
}


CELL * p_netSessions(CELL * params)
{
IO_SESSION * session;
IO_SESSION * sPtr;
CELL * sList;

session = ioSessions;
sList = getCell(CELL_EXPRESSION);

while(session)
    {
    sPtr = session;
    session = session->next;
    if(sPtr->family != AF_UNSPEC || getFlag(params))
        addList(sList, stuffInteger(sPtr->handle));
    }

return(sList);
}


/*********************************************************************/

CELL * p_netService(CELL * params) 
{
struct servent * pSe; 
char * service = NULL; 
char * protocol = NULL; 
CELL * cell;
UINT port; 
 
params = getEvalDefault(params, &cell);
getString(params, &protocol); 
if(cell->type == CELL_STRING)
    {
    service = (char *)cell->contents;
 
    if((pSe = getservbyname(service, protocol)) == NULL) 
        return(netError(ERR_INET_INVALID_SERVICE)); 
 
    port = (int)ntohs(pSe->s_port); 
 
    netErrorIdx = 0; 
    return(stuffInteger((UINT)port)); 
    }
if(isNumber(cell->type))
    {
    getIntegerExt(cell, &port, FALSE);

    if((pSe = getservbyport(htons(port), protocol)) == NULL) 
        return(netError(ERR_INET_INVALID_SERVICE)); 

    netErrorIdx = 0;
    return(stuffString(pSe->s_name));
    }

return(errorProcExt(ERR_NUMBER_OR_STRING_EXPECTED, cell));
} 

CELL * p_netConnect(CELL * params)
{
CELL * cell;
char * remoteHostName; 
UINT portNo;
UINT topt = CONNECT_TIMEOUT; /* default ms timeout from newlisp.h */
int sock;
int type; 
int protocol = 0;

params = getString(params, &remoteHostName); 
#ifndef WINDOWS
if(params == nilCell)
    {
    if((sock = netConnectLocal(remoteHostName)) == SOCKET_ERROR)
        return(netError(netErrorIdx));
    else    
        return(stuffInteger((UINT)sock));
    }

#endif

params = getInteger(params, &portNo);
type = SOCK_STREAM;
if(params != nilCell)
    {
    cell = evaluateExpression(params);
    if(cell->type == CELL_STRING)
        {
        protocol = toupper(*(char *)cell->contents);
        if(!(protocol == 'M' || protocol == 'B' || protocol == 'U'))
            return(errorProc(ERR_INVALID_OPTION));
        type = SOCK_DGRAM;
        if(protocol == 'M') /* get ttl */
            {
            if(params->next != nilCell)
                /* topt is ttl in this case */
                getInteger(params->next, &topt);
            else topt = 3;
            }
        }
    else if(isNumber(cell->type))
        /* topt is ms timeout */
        getIntegerExt(cell, &topt, FALSE);
    else
        return(errorProcExt(ERR_NUMBER_OR_STRING_EXPECTED, cell));      
    }

if((sock = netConnect(remoteHostName, 
        (int)portNo, type, protocol, (int)topt)) == SOCKET_ERROR)
    return(netError(netErrorIdx));

createIOsession(sock, ADDR_FAMILY);

netErrorIdx = 0;
return(stuffInteger((UINT)sock)); 
}


#ifndef WINDOWS
/* create local domain UNIX socket */
int netConnectLocal(char * path)
{
int sock;
struct sockaddr_un remote_sun;

if((sock = socket(AF_UNIX, SOCK_STREAM, 0)) == SOCKET_ERROR)
    {
    netErrorIdx = ERR_INET_OPEN_SOCKET;
    return(SOCKET_ERROR);
    }
    
remote_sun.sun_family = AF_UNIX;
strncpy(remote_sun.sun_path, path, sizeof(remote_sun.sun_path) - 1);
remote_sun.sun_path[sizeof (remote_sun.sun_path) - 1] = '\0';
if (connect(sock, (struct sockaddr *)&remote_sun, SUN_LEN(&remote_sun)) == -1) 
    {
    close(sock);
    netErrorIdx = ERR_INET_CONNECT_FAILED;
    return(SOCKET_ERROR);
    }

createIOsession(sock, AF_UNIX);

netErrorIdx = 0;
return(sock); 
}
#endif


int blockSocket(int sock)
{
#ifdef WINDOWS
u_long arg = 0;
return(ioctlsocket(sock, FIONBIO, &arg));
#else /* UNIX */
int arg;
arg = fcntl(sock, F_GETFL, NULL);
arg &= (~O_NONBLOCK);
return(fcntl(sock, F_SETFL, arg));
#endif
}

#ifdef UNBLOCK
int unblockSocket(int sock)
{
#ifdef WINDOWS
u_long arg = 1;
return(ioctlsocket(sock, FIONBIO, &arg));
#else /* Unix */
int arg;
arg = fcntl(sock, F_GETFL, NULL);
arg &= (~O_NONBLOCK);
return(fcntl(sock, F_SETFL, arg));
#endif
}
#endif

/* create internet socket
   if prot = NULL then topt is timeout in ms, else topt is ttl (time to live)
*/
int netConnect(char * remoteHostName, int portNo, int type, int prot, int topt)
{
struct addrinfo hints, *res, *res0;
char portStr[10];
int sock, opt;
#if defined(WINDOWS) || defined(EMSCRIPTEN)
u_long arg = 1;
#else
int arg, value;
socklen_t socklen = sizeof(sock);
#endif
int result = -1;
int sinlen;

/* create socket */
if((sock = socket(ADDR_FAMILY, type, 0)) == INVALID_SOCKET)
    {
    netErrorIdx = ERR_INET_OPEN_SOCKET;
    return(SOCKET_ERROR);
    }

if(prot == 0) /* topt is timeout in millisecs */
    {
#ifdef WINDOWS
    if(ioctlsocket(sock, FIONBIO, &arg) != 0)
        {
        netErrorIdx = ERR_INET_CANNOT_CHANGE_SOCK_BLOCK;
        return(SOCKET_ERROR);
        }
#else /* UNIX */
    arg = fcntl(sock, F_GETFL, NULL);
    if(fcntl(sock, F_SETFL, arg | O_NONBLOCK) < 0)
        {
        netErrorIdx = ERR_INET_CANNOT_CHANGE_SOCK_BLOCK;
        return(SOCKET_ERROR);
        }
#endif
    }
else if(prot == 'M' || prot == 'B') 
    {
    sinlen = (ADDR_FAMILY == AF_INET6) ? sizeof(struct in6_addr) : sizeof(struct in_addr);

    if(prot == 'M')
        {
        setsockopt(sock, 0, IP_MULTICAST_IF, (const void *)defaultInAddr, sinlen);
        opt = topt; /* ttl time to live */
        setsockopt(sock, 0, IP_MULTICAST_TTL, (const void *)&opt, sizeof(opt));
        }

    if(prot == 'B')
        {
        opt = 1;
        setsockopt(sock, SOL_SOCKET, SO_BROADCAST, (const void *)&opt, sizeof(opt));
        }
    }

if(type == SOCK_DGRAM && *remoteHostName == 0)
    return(sock);

memset(&hints, 0, sizeof(hints));
hints.ai_family = (ADDR_FAMILY == AF_INET6) ? PF_INET6 : PF_INET;
hints.ai_socktype = SOCK_STREAM;
snprintf(portStr, 10, "%d", portNo);

if(getaddrinfo(remoteHostName, portStr, &hints, &res0) != 0)
    {
    netErrorIdx = ERR_INET_HOST_UNKNOWN;
    return(SOCKET_ERROR);
    }

for(res = res0; res; res = res->ai_next)
    {
    result = connect(sock, res->ai_addr, res->ai_addrlen);
    if(result < 0)
        {
#ifdef WINDOWS
        if(WSAGetLastError() == WSAEWOULDBLOCK)
#else
        if(errno == EINPROGRESS) 
#endif
            {
            if((result = wait_ready(sock, topt * 1000, READY_WRITE)) <= 0)
                {
                netErrorIdx = result < 0 ? ERR_INET_CONNECT_FAILED : ERR_INET_TIMEOUT;
                goto CONNECT_FAILED;
                }
#ifndef WINDOWS 
            getsockopt(sock, SOL_SOCKET, SO_ERROR, (void*)&value, &socklen); 
            if (value) 
                { 
                netErrorIdx =  ERR_INET_CONNECT_FAILED;
                goto CONNECT_FAILED;
                } 
#endif
            result = 0;
            break;
            }
        else
            {
            netErrorIdx =  ERR_INET_CONNECT_FAILED;
            goto CONNECT_FAILED;
            }
        break;
        }
    else break; /* result == 0 */
    }

if(result != 0)
    {
    netErrorIdx = ERR_INET_CONNECT_FAILED;
    goto CONNECT_FAILED;
    }

if(blockSocket(sock) != 0)
    {
    netErrorIdx = ERR_INET_CANNOT_CHANGE_SOCK_BLOCK;
    goto CONNECT_FAILED;
    }
    
freeaddrinfo(res0);
netErrorIdx = 0;
return(sock);

CONNECT_FAILED:
freeaddrinfo(res0);
close(sock);
return(SOCKET_ERROR);
}

/* take and empty sockaddr and fill in from portNo, socket type and interface
   if ifAddr is defined as NULL or and empty string assume sockaddr at defaultIn
   which is initialized to INADDR_ANY in initDefaultInAddr()
*/
int getHostAddr(struct sockaddr * address, int stype, char * ifAddr)
{
struct addrinfo hints, *res;
int error;

if(ifAddr != NULL && *ifAddr != '\0')
    {
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = (ADDR_FAMILY == AF_INET6) ? PF_INET6 : PF_INET;
    hints.ai_socktype = stype;

    if((error = getaddrinfo(ifAddr, NULL, &hints, &res)) != 0)
        return(error);

    memcpy(address, res->ai_addr, res->ai_addrlen);

    freeaddrinfo(res);
    return(0);
    }

memcpy(address, defaultIn, defaultInLen);
return(0);
}


CELL * p_netInterface(CELL * params)
{
char * ifAddr;
char IPaddress[STRADDR_LEN];

netErrorIdx = 0;

if(params != nilCell)
    {
    params = getString(params, &ifAddr);
    if(getHostAddr((struct sockaddr *)defaultIn, 0, ifAddr) != 0)
        return(netError(ERR_INET_HOST_UNKNOWN));
    }

getnameinfo((struct sockaddr *)defaultIn, defaultInLen, 
    IPaddress, STRADDR_LEN, NULL, 0, NI_NUMERICHOST);
return(stuffString(IPaddress));
}


CELL * p_netIpv(CELL * params)
{
UINT protocol = (ADDR_FAMILY == AF_INET6) ? 6 : 4;

if(params != nilCell)
    {
    getInteger(params, &protocol);
    if(protocol != 4 && protocol != 6)
        return(errorProc(ERR_INVALID_PARAMETER));
    if(protocol == 6) 
        {
        ADDR_FAMILY = AF_INET6;
        opsys |= 512;
        }
    else 
        {
        ADDR_FAMILY = AF_INET;
        opsys &= ~512;
        }
    initDefaultInAddr();
    }

return(stuffInteger(protocol));
}


/********* should be called after listen/accept notification **********/

CELL * p_netAccept(CELL * params) 
{
int sock;
UINT listenSock;

getInteger(params, &listenSock); 

if((sock = netAccept((int)listenSock)) == INVALID_SOCKET)
    return(netError(ERR_INET_ACCEPT));

netErrorIdx = 0;
return(stuffInteger(sock)); 
}


int netAccept(int listenSock)
{
int sock, family;
struct sockaddr * dest_sin;
socklen_t dest_slen;
#ifndef WINDOWS
struct sockaddr_un dest_sun;
#endif

family = getSocketFamily(listenSock);

#ifndef WINDOWS
if(family == AF_UNIX)
    {
    dest_slen = sizeof(struct sockaddr_un);
    sock = accept(listenSock, (struct sockaddr *) &dest_sun,  &dest_slen);
    }
else
#endif
    {
    dest_slen = (ADDR_FAMILY == AF_INET6) ? 
        sizeof(struct sockaddr_in6) : sizeof(struct sockaddr_in);

    dest_sin = alloca(dest_slen);

    sock = accept(listenSock, dest_sin, (void *)&dest_slen);
    }

if(sock != INVALID_SOCKET) 
    {
    createIOsession(sock, family);
    netErrorIdx = 0;
    }

return(sock);
}


/******************* returns IP and port number from socket *************/

#define LOCAL_INFO 0
#define PEER_INFO 1

int getIpPortFromSocket(int sock, int peerLocalFlag, char * IPaddress)
{
int family;
socklen_t address_sin_len; 
struct sockaddr * address_sin;

family = getSocketFamily(sock);
address_sin_len = (family == AF_INET6) ? 
        sizeof(struct sockaddr_in6) : sizeof(struct sockaddr_in);
address_sin = alloca(address_sin_len);

*IPaddress = 0;

if(family == AF_UNIX)
    {
    strncpy(IPaddress, "local", 6);
    return(0);
    }

if(peerLocalFlag == LOCAL_INFO) 
    {
    if(getsockname(sock, address_sin, (void *)&address_sin_len) == SOCKET_ERROR)
        return(SOCKET_ERROR);
    }
else 
    if(getpeername(sock, address_sin, (void *)&address_sin_len) == SOCKET_ERROR)
        return(SOCKET_ERROR);
 
/* return address and port, IP number, address family taken fron address_sin  */
ipstrFromSockAddr((struct sockaddr *)address_sin, IPaddress, STRADDR_LEN);
return(portFromSockAddr((struct sockaddr *)address_sin));
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
char name[STRADDR_LEN];
UINT addressPort, sock;

getInteger(params, &sock); 
if((addressPort = getIpPortFromSocket((int)sock, peerLocalFlag, name)) == SOCKET_ERROR)
    return(netError(ERR_INET_NOT_VALID_SOCKET));

result = makeCell(CELL_EXPRESSION, (UINT)stuffString(name));

cell = (CELL *)result->contents; 
cell->next = stuffInteger((UINT)addressPort); 
 
netErrorIdx = 0; 
return(result); 
} 

CELL * p_netLookup(CELL * params)
{
char * hostString;
int forceByName = 0;
struct addrinfo hints, *res;
char hbuf[NI_MAXHOST];
int flags = NI_NUMERICHOST;

netErrorIdx = 0;
params = getString(params, &hostString);
forceByName = getFlag(params);

if( ((ADDR_FAMILY == AF_INET6) && strstr(hostString, ":") && !forceByName) 
    ||
    ((ADDR_FAMILY == AF_INET) && isDigit((unsigned char)*hostString) && !forceByName) )
    flags = NI_NAMEREQD;

memset(&hints, 0, sizeof(hints));

hints.ai_family = (ADDR_FAMILY == AF_INET6) ? PF_INET6 : PF_INET;
hints.ai_socktype = SOCK_STREAM;
/* hints.ai_flags = AI_ADDRCONFIG | AI_CANONNAME; */

if(getaddrinfo(hostString, NULL, &hints, &res) != 0)
    return(netError(ERR_INET_HOST_UNKNOWN));

if(getnameinfo(res->ai_addr, res->ai_addrlen, hbuf, sizeof(hbuf), NULL, 0, flags) != 0)
    {
    freeaddrinfo(res);
    return(netError(ERR_INET_HOST_UNKNOWN));
    }

freeaddrinfo(res);
return(stuffString(hbuf));
}


CELL * netReceive(int sock, SYMBOL * readSymbol, size_t readSize, CELL * params);


CELL * p_netReceive(CELL * params) 
{ 
UINT sock;
SYMBOL * readSymbol;
size_t readSize;
CELL * cell;

params = getInteger(params, &sock);
params = getEvalDefault(params, &cell);

if(!symbolCheck)
    return(errorProcExt(ERR_SYMBOL_EXPECTED, cell));
if(symbolCheck->contents != (UINT)cell)
    return(errorProc(ERR_IS_NOT_REFERENCED));

readSymbol = symbolCheck;
params = getInteger(params, (UINT *)&readSize);

return(netReceive((int)sock, readSymbol, readSize, params));
}


CELL * netReceive(int sock, SYMBOL * readSymbol, size_t readSize, CELL * params)
{
char * waitFor;
ssize_t bytesReceived;
size_t length;
int found;
STREAM netStream = {NULL, NULL, 0, 0, 0};
char chr;
CELL * cell;

if(isProtected(readSymbol->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(readSymbol)));

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
    deleteIOsession(sock); 
    return(netError(ERR_INET_CONNECTION_DROPPED)); 
    } 

if(bytesReceived == SOCKET_ERROR) 
    { 
    closeStrStream(&netStream);         
    deleteIOsession(sock); 
    return(netError(ERR_INET_READ)); 
    } 
  
cell = stuffStringN(netStream.buffer, bytesReceived);
closeStrStream(&netStream);

deleteList((CELL *)readSymbol->contents); 
readSymbol->contents = (UINT)cell; 
 
netErrorIdx = 0; 
return(stuffInteger(bytesReceived)); 
} 


CELL * netReceiveFrom(int sock, size_t readSize, int closeFlag)
{
int portNo;
int family;
char * buffer;
ssize_t bytesReceived;
struct sockaddr * remote;
char IPaddress[STRADDR_LEN];
CELL * cell;
CELL * result;
#ifdef TRU64
unsigned long remote_sin_len;
#else
socklen_t remote_sin_len;
#endif

buffer = (char *)allocMemory(readSize + 1);

family = getSocketFamily(sock);
remote_sin_len = (family == AF_INET6) ? 
    sizeof(struct sockaddr_in6) : sizeof(struct sockaddr_in);
remote = alloca(remote_sin_len);
memset(remote, 0, remote_sin_len);

bytesReceived = recvfrom(sock, buffer, readSize, 0, 
    (struct sockaddr *)remote, &remote_sin_len);

if(bytesReceived == SOCKET_ERROR) 
    {
    freeMemory(buffer);
    deleteIOsession(sock);
    return(netError(ERR_INET_READ)); 
    }

ipstrFromSockAddr((struct sockaddr *)remote, IPaddress, STRADDR_LEN);
portNo = portFromSockAddr((struct sockaddr *)remote);

cell = result = makeCell(CELL_EXPRESSION, (UINT)stuffStringN(buffer, bytesReceived));

cell = (CELL *)cell->contents;
cell->next = stuffString(IPaddress);
((CELL*)cell->next)->next = stuffInteger(portNo);

freeMemory(buffer);

if(closeFlag) 
    {
    deleteIOsession(sock);
    }

netErrorIdx = 0;
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
    params = getInteger64Ext(params, &wait, TRUE);
    if(params != nilCell)
        getString(params, &ifaddr);
    }

if((sock = netListenOrDatagram((int)portNo, SOCK_DGRAM, ifaddr, NULL, 0)) == SOCKET_ERROR)
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
    deleteIOsession((int)sock); 
    return(netError(ERR_INET_WRITE)); 
    }

netErrorIdx = 0; 
return(stuffInteger(bytesSent)); 
}


#define SEND_TO_UDP 0
#define SEND_TO_SOCK 1

CELL * netSendTo(CELL * params, int type)
{
char * remoteHost;
UINT remotePort;
struct sockaddr * destination;
socklen_t destination_len;
size_t size;
char * buffer;
ssize_t bytesSent;
UINT sock;
/* char one = 1; */
int one = 1;

params = getString(params, &remoteHost);
params = getInteger(params, &remotePort);
params = getStringSize(params, &buffer, &size, TRUE);

destination_len = (ADDR_FAMILY == AF_INET6) ? 
        sizeof(struct sockaddr_in6) : sizeof(struct sockaddr_in);
destination = alloca(destination_len);

if(*remoteHost != 0) /* 10.0.1 */
    {
    if(getHostAddr(destination, 0, remoteHost) != 0)
        return(netError(ERR_INET_HOST_UNKNOWN));
    setSockaddrPort(destination, remotePort);
    }

if(type == SEND_TO_UDP) /* for 'net-send-udp' */
    {
    if((sock = socket(ADDR_FAMILY, SOCK_DGRAM, 0)) == INVALID_SOCKET)
        return(netError(ERR_INET_OPEN_SOCKET));

     if(getFlag(params))
        setsockopt(sock, SOL_SOCKET, SO_BROADCAST, (const void *)&one, sizeof(one));
    }
else /* SEND_TO_SOCK , socket may or may not be UDP, for 'net-send-to' */
    {
    params = getInteger(params, &sock);
    }


/* for socket opened with ip-address in (net-connect host port "udp") 
   in this case issue (net-send-to "" port message socket) 10.0.1 */
if(*remoteHost == 0) 
    bytesSent = sendto((int)sock, buffer, size, NO_FLAGS_SET, NULL, 0);
else
    bytesSent = sendto((int)sock, buffer, size, NO_FLAGS_SET,
        (struct sockaddr *)destination, destination_len);

if(type == SEND_TO_UDP) close((int)sock);

if(bytesSent == SOCKET_ERROR)
        return(netError(ERR_INET_WRITE));

netErrorIdx = 0;
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
int sockopt = 0;
int opt;
CELL * cell;
 
type = SOCK_STREAM;
cell = evaluateExpression(params);
params = params->next;

#ifndef WINDOWS
if(cell->type == CELL_STRING)
    {
    if((sock = netListenLocal((char *)cell->contents)) == SOCKET_ERROR)
        return(netError(netErrorIdx));
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
        opt = toupper(*option);
        if(opt == 'U')
            type = SOCK_DGRAM;
#ifndef WINDOWS
        else if(opt == 'D')
            {
            type = SOCK_RAW;
            sockopt = IPPROTO_DIVERT;
            }
#endif
        else if(opt == 'M')
            {
            type = SOCK_DGRAM;
            mcAddr = ifAddr;
            ifAddr = NULL;
            }
        else
            errorProc(ERR_INVALID_OPTION);
        }
            
    }
    

if((sock = netListenOrDatagram((int)portNo, type, ifAddr, mcAddr, sockopt)) 
        == SOCKET_ERROR)
    return(nilCell);

return(stuffInteger(sock));
} 

#ifndef WINDOWS
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
    netErrorIdx = ERR_INET_CANNOT_BIND;
    return(SOCKET_ERROR);
    }

if(listen(sock, MAX_PENDING_CONNECTS) == SOCKET_ERROR)
    {
    close(sock);
    netErrorIdx = ERR_INET_LISTEN_FAILED;
    return(SOCKET_ERROR);
    }

createIOsession(sock, AF_UNIX);

netErrorIdx = 0;
return(sock);
}
#endif


int netListenOrDatagram(int portNo, int stype, char * ifAddr, char * mcAddr, int option)
{
int sock, one = 1;
struct sockaddr * local;
struct ip_mreq mcast;
socklen_t local_len;

if((sock = socket(ADDR_FAMILY, stype, option)) == INVALID_SOCKET)
    {
    netErrorIdx = ERR_INET_OPEN_SOCKET; 
    return SOCKET_ERROR;
    }

local_len = (ADDR_FAMILY == AF_INET6) ?
    sizeof(struct sockaddr_in6) : sizeof(struct sockaddr_in);
local = alloca(local_len);
memset(local, 0, local_len);

if(getHostAddr((struct sockaddr *)local, stype, ifAddr) != 0)
    return(SOCKET_ERROR);

setSockaddrPort(local, portNo);

setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const void*)&one, sizeof(one));
 
if(bind(sock, (struct sockaddr *)local, local_len) == SOCKET_ERROR) 
    { 
    close(sock);
    netErrorIdx = ERR_INET_CANNOT_BIND; 
    return(SOCKET_ERROR);
    } 

if(mcAddr != NULL)
    {
    memset(&mcast, 0, sizeof(mcast));
    mcast.imr_multiaddr.s_addr = inet_addr(mcAddr);
    if(ADDR_FAMILY == AF_INET)
        mcast.imr_interface.s_addr = INADDR_ANY;    
/* not handled/debugged
    else
        mcast.imr_interface.s_addr = IN6ADDR_ANY_INIT;
*/
    setsockopt(sock, 0, IP_ADD_MEMBERSHIP, (const void *)&mcast, sizeof(mcast));
    }

if(stype == SOCK_STREAM)
    {
    if(listen(sock, MAX_PENDING_CONNECTS) == SOCKET_ERROR)  
        { 
        close(sock); 
        netErrorIdx = ERR_INET_LISTEN_FAILED;
        return(SOCKET_ERROR);
        } 
    }

createIOsession(sock, ADDR_FAMILY); 

netErrorIdx = 0;
return(sock);
}


/* returns number of bytes ready to read */
CELL * p_netPeek(CELL * params)
{
UINT sock;
#ifdef WINDOWS
u_long result;
#else
int result;
#endif

getInteger(params, &sock);

if(ioctl((int)sock, FIONREAD, &result) == SOCKET_ERROR)
    return(netError(ERR_INET_PEEK_FAILED));

netErrorIdx = 0;
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
INT64 wait;
char * mode;
struct timeval timeOut;
fd_set socketSet;
CELL * cell;
CELL * list;
CELL * sockListHead = NULL;
CELL * sockListPtr;
struct timeval* tmvPtr;
UINT socket;
int setSize = 0;

netErrorIdx = 0;

FD_ZERO(&socketSet);

cell = evaluateExpression(params);
if(isNumber(cell->type))
    getIntegerExt(cell, (UINT*)&socket, FALSE);
else if(isList(cell->type))
    {
    cell = (CELL*)cell->contents;
    sockListHead = cell;
    while(cell != nilCell)
        {
        cell = getIntegerExt(cell, (UINT*)&socket, FALSE);
        if(setSize == FD_SETSIZE)
            return(netError(ERR_INET_TOO_MUCH_SOCKETS));
        else setSize++;
        FD_SET((int)socket, &socketSet);
        }
    }
else return(errorProcExt(ERR_LIST_OR_NUMBER_EXPECTED, params));

params = getString(params->next, &mode);
getInteger64Ext(params, &wait, TRUE);

tmvPtr = (wait == -1) ? NULL : &timeOut;
timeOut.tv_sec = wait/1000000;
timeOut.tv_usec = wait - timeOut.tv_sec * 1000000;

if(sockListHead == NULL)
    FD_SET((int)socket, &socketSet);

/* printf("%d %d %d\n", timeOut.tv_sec, timeOut.tv_usec, sizeof(timeOut.tv_sec));  */

if(*mode == 'r')
    setSize = select(FD_SETSIZE, &socketSet, NULL, NULL, tmvPtr);
else if(*mode == 'w')
    setSize = select(FD_SETSIZE, NULL, &socketSet, NULL, tmvPtr);
else if(*mode == 'e')
    setSize = select(FD_SETSIZE, NULL, NULL, &socketSet, tmvPtr);
else return(errorProcExt2(ERR_INVALID_PARAMETER, stuffString(mode)));

if(setSize >= 0)
    {
    if(sockListHead == NULL)
        {
        if(setSize == 0) return(nilCell);
        else return(trueCell);
        }    

    list = getCell(CELL_EXPRESSION);
    if(setSize == 0) return(list);

    sockListPtr = sockListHead;
    while(sockListPtr != nilCell)
        {
        sockListPtr = getIntegerExt(sockListPtr, &socket, FALSE);
        /* printf("testing %lu\n", socket); */
        if(FD_ISSET(socket, &socketSet))
            {
            addList(list, stuffInteger(socket));
            /* printf("adding %lu\n", socket); */
            }
        }

    return(list);
    }

netError(ERR_INET_SELECT_FAILED);

return(nilCell);
}

extern char logFile[];

void writeLog(char * text, int newLine)
{
int handle;


#ifdef WINDOWS
handle = open(logFile, O_RDWR | O_APPEND | O_BINARY | O_CREAT, S_IREAD | S_IWRITE);
#else
handle = open(logFile, O_RDWR | O_APPEND | O_BINARY | O_CREAT,
          S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR | S_IWGRP | S_IWOTH); /* rw-rw-rw */
#endif

if(write(handle, text, strlen(text)) < 0) return;
if(newLine) 
    if(write(handle, &LINE_FEED, LINE_FEED_LEN) < 0) return;
#ifdef WINDOWS
_close(handle);
#else
close(handle);
#endif
}

FILE * win_fdopen(int handle, const char * mode);

FILE * serverFD(int port, char * domain, int reconnect)
{
static int sock, connection;
char name[STRADDR_LEN];
char text[80];
time_t t;

text[79] = 0;

if(!reconnect)
    {
#ifndef WINDOWS
    if(port != 0)
        sock = netListenOrDatagram(port, SOCK_STREAM, NULL, NULL, 0);
    else
        sock = netListenLocal(domain);
#else
    sock = netListenOrDatagram(port, SOCK_STREAM, NULL, NULL, 0);
#endif

    if(sock == SOCKET_ERROR) return(NULL);
    else {
        snprintf(text, 79, "newLISP v.%d listening on %s", version, domain);
        writeLog(text, TRUE);
        }
    }
else
    {
    deleteIOsession(connection);
    }

if((connection = netAccept(sock)) == SOCKET_ERROR)
    return NULL;

/* avoid registering socket twice */
if(!isSessionSocket(connection))
    createIOsession(connection, (port != 0) ? ADDR_FAMILY : AF_UNIX);

/* print log */
getIpPortFromSocket(connection, PEER_INFO, name);
t = time(NULL);
snprintf(text, 79, "Connected to %s on %s", name, ctime(&t));
/* printf(text); */
writeLog(text, 0);

setenv("REMOTE_ADDR", name, 1);

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
INT timeOut = 60000; /* milli secs */
int start, elapsed = 0;
CELL * result;
STREAM * netStream;
CELL * netEvalIdle = NULL;
char buffer[MAX_BUFF];
int rawMode = FALSE;
int singleSession = FALSE;
jmp_buf errorJumpSave;
int errNo;
UINT * resultStackIdxSave;

list  = evaluateExpression(params);
if(list->type == CELL_STRING)
    {
    host = (char *)list->contents;
    params = getIntegerExt(params->next, &port, TRUE);
    /* params = getStringSize(params, &prog, &size, TRUE); */
    /* convert to string if required (since 10.1.1) */
    prog = cellToString(evaluateExpression(params), &size, FALSE);
    params = params->next;
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
prog = cellToString(evaluateExpression(cell), &size, FALSE);
rawMode = getFlag(cell->next);

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

/* timeout for making connection is always 15 secs, 
   but variable timeOut is used during connection */
#ifndef WINDOWS
if(port != 0)
    sock = netConnect(host, (int)port, SOCK_STREAM, 0, 15000);
else
    sock = netConnectLocal(host);
#else
sock = netConnect(host, (int)port, SOCK_STREAM, 0, 15000);
#endif

if(sock == SOCKET_ERROR)
        {
        session->result = netEvalError(netErrorIdx);
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
createIOsession(sock, ADDR_FAMILY);
count++;
CONTINUE_CREATE_SESSION:
list = list->next;
if(list != nilCell) goto CREATE_SESSION;

/* get timeout ms and optional handler symbol
   this is the timeout after connections  are made
   waiting for the result to come back */
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
        cell = makeCell(CELL_EXPRESSION, (UINT)copyCell(netEvalIdle));

        pushResult(cell);
        if(!evaluateExpressionSafe(cell, &errNo))
            {
            freeSessions(base);
            longjmp(errorJump, errNo);
            }
        }

    bytes = -1; errNo = 0;
    ready = wait_ready(session->sock, 100, READY_READ);
    if(ready > 0)
        {
        memset(buffer, 0, MAX_BUFF);
        bytes = recv(session->sock, buffer, MAX_BUFF, NO_FLAGS_SET);

/*
        if(bytes >= 0) printf("bytes:%ld=>%s<=\n", bytes, buffer); 
*/
        if(bytes) 
            {
	    if(bytes == -1) bytes = 0;
            errNo = (memcmp(buffer, "\nERR: ", 6) == 0);
            writeStreamStr(netStream, buffer, bytes);
            }
        }
    if(ready < 0 || bytes == 0 || errNo || elapsed >= timeOut)
        {
/*
        printf("count=%ld ready=%d bytes=%ld elapsed=%d\n", count, ready, bytes, elapsed); 
*/
        if(elapsed >= timeOut) result = copyCell(nilCell); 
        else if(rawMode || errNo) /* get raw buffer without the quote */
            result = stuffStringN(netStream->buffer, netStream->position);
        else 
            {
            memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
            if((errNo = setjmp(errorJump)) != 0)
                {
                memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
                freeSessions(base);
                longjmp(errorJump, errNo);
                }
            /* Only one expression should be sent out by net-eval. When contained
               in a string more than one can be sent out and will be evaluated,
               burt net-eval will only return the result of the last(10.6.3) one */
            result = sysEvalString(netStream->buffer, currentContext, nilCell, READ_EXPR_NET);
            /* changed from READ_EXPR+SYNC to READ_EXPR in 10.6.3 */
            memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
            }

        if(netEvalIdle)
            {
            session->result = cell = makeCell(CELL_EXPRESSION, (UINT)stuffString(session->host));

            cell = (CELL *)cell->contents;
            cell->next = stuffInteger(session->port);
            cell = cell->next;
            cell->next = result;
            }
        else
            session->result = result;
            
        closeStrStream(netStream);
        deleteIOsession(session->sock);
        free(netStream);
        session->netStream = NULL;
            
        if(netEvalIdle)
            {
            list = makeCell(CELL_EXPRESSION, (UINT)copyCell(netEvalIdle));
            cell = makeCell(CELL_QUOTE, (UINT)session->result);

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
                result = makeCell(CELL_EXPRESSION, (UINT)base->result);
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
    netErrorIdx = ERR_INET_TIMEOUT;
else netErrorIdx = 0;
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
        deleteIOsession(base->sock);
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
    "DNS resolution failed",
    "Not a valid service",
    "Connection failed",
    "Accept failed",
    "Connection closed",
    "Connection broken",
    "Socket recv failed",
    "Socket send failed",
    "Cannot bind socket",
    "Too many sockets",
    "Listen failed",
    "Badly formed IP",
    "Select failed",
    "Peek failed",
    "Not a valid socket",
    "Cannot block or unblock socket",
    "Operation timed out",
/* for nl-web.c */
    "HTTP bad formed URL",
    "HTTP file operation failed",
    "HTTP transfer failed",
    "HTTP invalid response from server",
    "HTTP no response from server",
    "HTTP not content",
    "HTTP error in header",
    "HTTP error in chunked format"
    };


CELL * netError(int errorNo) 
{ 
netErrorIdx = errorNo; 
return(nilCell); 
} 

CELL * netEvalError(int errorNo)
{ 
netErrorIdx = errorNo; 
return(p_netLastError(nilCell));
}

CELL * p_netLastError(CELL * params)
{
CELL * result;
char str[40];
UINT numError = netErrorIdx;

if(params != nilCell)
    getInteger(params, &numError);
else
    if(numError == 0) return(nilCell);

result = makeCell(CELL_EXPRESSION, (UINT)stuffInteger(numError));

snprintf(str, 40, "%s", (numError > MAX_NET_ERROR) ? UNKNOWN_ERROR : netErrorMsg[numError]);
((CELL *)result->contents)->next = stuffString(str);

return(result);
}

#ifndef NO_NET_PING
/* net-ping 

   Undocumented boolean flag parameter (after count) puts error info
   into the result list when a packet could not be sent.
*/

CELL * p_netPing(CELL * params)
{
CELL * address;
UINT maxwait = 1000, listmode = 0;
UINT flag = 0;
UINT count = 0;

params = getEvalDefault(params, &address);
if(address->type == CELL_EXPRESSION)
    {
    address = (CELL *)address->contents;
    listmode = 1;
    }
else if(address->type != CELL_STRING)
  return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, address));

if(params != nilCell)  
    {
    params = getInteger(params, &maxwait);
    if(params != nilCell)
        params = getInteger(params, &count);
    flag = getFlag(params);
    }

return(ping(address, (int)maxwait, (int)listmode, (int)count, (int)flag));
}

#define PLEN 64

CELL * ping(CELL * address, int maxwait, int listmode, int maxCount, int flag)
{
char * host;
char * ptr;
char * hostaddr = NULL;
unsigned char packet[PLEN];
struct ip *ip;
char IPaddress[STRADDR_LEN];
struct icmp6_filter filter;
struct icmp6_hdr *icp6 = (struct icmp6_hdr *) packet;
struct icmp *icp = (struct icmp *) packet;
struct sockaddr * whereto;
struct sockaddr * from;
int s;
int sockopt = 1;
#ifdef TRU64
unsigned long sockaddr_len;
#else
#ifdef OS2
int sockaddr_len;
#else
socklen_t sockaddr_len;
#endif
#endif
int broadcast = 0;
int size, ipNo, startIp = 0, endIp = 0;
int timeout = 0, tdiff;
int sendCount = 0, receiveCount = 0;
ssize_t len;
struct timeval tv, tp;
CELL * result = NULL;
CELL * link = NULL;
char buff[64];

#ifdef MAC_OSX /* no superuser rights necessary */
if ((s = socket(ADDR_FAMILY, SOCK_DGRAM, ICMP_TYPE)) < 0) 
#else
if ((s = socket(ADDR_FAMILY, SOCK_RAW, ICMP_TYPE)) < 0)
#endif 
    return(netError(ERR_INET_OPEN_SOCKET));
   
sockaddr_len = (ADDR_FAMILY == AF_INET6) ? 
    sizeof(struct sockaddr_in6) : sizeof(struct sockaddr_in);

whereto = alloca(sockaddr_len);
from = alloca(sockaddr_len);

if(ADDR_FAMILY == AF_INET6)
    {
    ICMP6_FILTER_SETPASSALL (&filter);
    setsockopt (s, IPPROTO_ICMPV6, ICMP6_FILTER, &filter, sizeof (filter));
    }

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

    if(ADDR_FAMILY == AF_INET) /* IPv4 */
        {
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
                if(endIp > 254) endIp = 254;
                }
            }
        }

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

        memset((char *)whereto, 0, sockaddr_len);
#ifdef  MAC_OSX
        if(getHostAddr(whereto, SOCK_DGRAM, hostaddr) != 0)
#else
        if(getHostAddr(whereto, SOCK_RAW, hostaddr) != 0)
#endif
            {
            shutdown(s, SHUT_RDWR);
            return(netError(ERR_INET_HOST_UNKNOWN));
            }

        if(ADDR_FAMILY == AF_INET6)
            setSockaddrPort(whereto, htons(IPPROTO_ICMPV6));
        else if((broadcast = (strncmp(host + len - 4, ".255", 4) == 0)))
            setsockopt(s, SOL_SOCKET, SO_BROADCAST, (void *) &sockopt, sizeof(sockopt));
            
        /* ping setup ICMP packet */
        if(ADDR_FAMILY == AF_INET6)
            {
            memset(icp6, 0, PLEN);
            icp6->icmp6_type = ICMP6_ECHO_REQUEST;
            icp6->icmp6_id = getpid() & 0xFFFF; 
            gettimeofday((struct timeval *)&icp6->icmp6_data8[4], NULL);
            }
        else
            {
            memset(icp, 0, PLEN);
            icp->icmp_type = ICMP_ECHO;
            icp->icmp_id = getpid() & 0xFFFF; 
            gettimeofday((struct timeval *)&icp[1], NULL);
            icp->icmp_cksum = in_cksum((unsigned short *) icp, PLEN );
            }

        while(wait_ready(s, 10000, READY_WRITE) <= 0)
            {
            gettimeofday(&tp, NULL);
            if((timeout = (timediff_ms(tp, tv) > maxwait))) break;
            continue;
            }
        
        /* ping */  
        size = sendto(s, packet, PLEN, 0,(struct sockaddr *)whereto, sockaddr_len);
        if(size != PLEN)
            {
            if(flag)
                {
                snprintf(buff, 64, "%s", strerror(errno));
                ipstrFromSockAddr((struct sockaddr *)whereto, IPaddress, STRADDR_LEN);
                link = addResult(&result, link, 
                    makePair(stuffString(IPaddress), stuffString(buff)));
                }
            if( !(listmode || startIp) ) break;
            continue;
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

    if(wait_ready(s, 1000, READY_READ) <= 0)
        {
        gettimeofday(&tp, NULL);
        if((timeout = (timediff_ms(tp, tv) > maxwait))) break;
        continue;
        }    

    memset(packet, 0, PLEN);
    memset(from, 0, sockaddr_len);
    if ( (len = recvfrom(s, packet, PLEN, 0, (struct sockaddr *)from, 
                        (socklen_t *)&sockaddr_len)) < 0)
        continue;
    
    ip = (struct ip *) packet;
    gettimeofday(&tp, NULL);

    if(ADDR_FAMILY == AF_INET6)
        {
        icp6 = (struct icmp6_hdr *)packet;
        if(icp6->icmp6_type != ICMP6_ECHO_REPLY) continue;
        if(icp6->icmp6_id != (getpid() & 0xFFFF)) continue; 
        tdiff = timediff64_us(tp, *(struct timeval *)&icp6->icmp6_data8[4]);
        }
    else
        {
        icp = (struct icmp *)(packet + (ip->ip_hl << 2));
        if(icp->icmp_id != (getpid() & 0xFFFF)) continue; 
        tdiff = timediff64_us(tp, *(struct timeval *)&icp[1]);
        }

    ipstrFromSockAddr((struct sockaddr *)from, IPaddress, STRADDR_LEN);
    link = addResult(&result, link, 
            makePair(stuffString(IPaddress), stuffInteger(tdiff)));

    if(++receiveCount == maxCount) break;
    if( !(broadcast || listmode || startIp) ) break;
    } 

shutdown(s, SHUT_RDWR); 

if(timeout) netErrorIdx = ERR_INET_TIMEOUT;
else netErrorIdx = 0;
return(result == NULL ? getCell(CELL_EXPRESSION) : result);
}


CELL * addResult(CELL * * result, CELL * cell, CELL * new)
{
if(*result == NULL)
    *result = makeCell(CELL_EXPRESSION, (UINT)new);
else
    cell->next = new;   

return(new);
}

CELL * makePair(CELL * left, CELL * right)
{
left->next = right;
return(makeCell(CELL_EXPRESSION, (UINT)left));
}

unsigned short in_cksum(unsigned short * addr, int len)
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

#endif /* NO_NET_PING */


/* check socket for readability or error 
   return 0 if the time limit expires or -1
   on error
   tie limit in wait given in micro-secs
*/
int wait_ready(int sock, INT64 wait_us, int mode)
{
struct timeval timeOut;
fd_set socketSet;

FD_ZERO(&socketSet);
FD_SET(sock, &socketSet);

timeOut.tv_sec = wait_us/1000000;
timeOut.tv_usec = wait_us - timeOut.tv_sec * 1000000;

if(mode == READY_READ)
    return(select(sock + 1, &socketSet, NULL, NULL, &timeOut));
else
    return(select(sock + 1, NULL, &socketSet, NULL, &timeOut));
}


/* ------------------------- raw sockets ---------------------------------------

   (net-packet packet)

   Checksums are only gnerated if set to zero in the packet.

   Packets start with the ip header followed by either a TCP, UDP or ICMP
   header and data.
*/

/* for checksum calculation in TCP packets */
struct pseudohdr
    {
    struct in_addr source_addr;
    struct in_addr dest_addr;
    unsigned char dummy;
    unsigned char protocol;
    unsigned short len;
    };


#ifndef NO_NET_PACKET
unsigned short pseudo_chks( struct ip * iph, char * packet, char * header, int data_offset);
/* (net-packet str-packeA [int-config-flags]) */

CELL * p_netPacket(CELL * params)
{
char * packet;
int sock;
int one = 1;
struct sockaddr_in dest_sin;
size_t size;
ssize_t bytesSent;
struct ip * iph;
struct tcphdr * tcph;
struct udphdr * udph;
struct icmp * icmph;
int dport;
/* ip_len  and ip_off must be check-summed in network order (big-Endian),
   but one or both must be given to the sendto() in host byte-order on
   some OS. As of 10.2.6 the following configFLags work for Mac OS X
   on PPC and Intel, Linux on Intel and OpenBSD on Intel.

   Curently net-packet is IPv4 only.
*/
#if defined(MAC_OSX)
UINT configFlags = 3; /* ntohs() for both ip_len and ip_off */
#elif defined(LINUX)
UINT configFlags = 1; /* ntohs() for ip_len only */
#else /* works for OpenBSD */
UINT configFlags = 0; /* none */
#endif

params = getStringSize(params, &packet, &size, TRUE);
/* undocumented user-supplied extra configFlags parameter
   can have a value of 0,1,2,3
*/
if(params != nilCell)
    getInteger(params, &configFlags);

iph = (struct ip *)packet;

if(iph->ip_sum == 0)
    iph->ip_sum = in_cksum((unsigned short *)iph, sizeof(struct ip));

switch(iph->ip_p)
  {
  case IPPROTO_TCP:
    tcph = (struct tcphdr *)(packet + sizeof(struct ip));
#ifdef ANDROID
    dport = tcph->dest;
    if(tcph->check == 0) 
        tcph->check = pseudo_chks(iph, packet, (char *) tcph, tcph->doff * 4);
#else
    dport = tcph->th_dport;
    if(tcph->th_sum == 0) 
        tcph->th_sum = pseudo_chks(iph, packet, (char *) tcph, tcph->th_off * 4);
#endif
    break;
  case IPPROTO_UDP:
    udph = (struct udphdr *)(packet + sizeof(struct ip));
    dport = udph->uh_dport;
    if(udph->uh_sum == 0)
        udph->uh_sum = pseudo_chks(iph, packet, (char *) udph, sizeof(struct udphdr));
    break;
  case IPPROTO_ICMP:
    icmph = (struct icmp *)(packet + sizeof(struct ip));
    dport = 0;
    if(icmph->icmp_cksum == 0)
    icmph->icmp_cksum = in_cksum((unsigned short *)icmph, 
            ntohs(iph->ip_len) - iph->ip_hl * 4);
    break;
  default:
    return(nilCell);
  }

if(configFlags & 0x1) iph->ip_len = ntohs(iph->ip_len);
if(configFlags & 0x2) iph->ip_off = ntohs(iph->ip_off);

memset(&dest_sin, 0, sizeof(dest_sin));
dest_sin.sin_family = AF_INET;
dest_sin.sin_port = dport;
dest_sin.sin_addr = iph->ip_dst;

if((sock = socket(AF_INET, SOCK_RAW, IPPROTO_RAW)) < 0)
    return(netError(ERR_INET_OPEN_SOCKET));
  
if(setsockopt(sock, IPPROTO_IP, IP_HDRINCL, (char *)&one, sizeof(one)) < 0)
    return(netError(ERR_INET_OPEN_SOCKET));

if((bytesSent = sendto(sock, packet, size, NO_FLAGS_SET, 
        (struct sockaddr *)&dest_sin, sizeof(dest_sin))) != size)
    return(nilCell);

return(stuffInteger(bytesSent));
}

unsigned short pseudo_chks(
        struct ip * iph, char * packet, char * header, int data_offset)
{
/* packet was give with ip_len in net-work byte order */
unsigned short checksum;
int segment_len = ntohs(iph->ip_len) - iph->ip_hl * 4;
int data_len = segment_len - data_offset;
struct pseudohdr * pseudoh = alloca(sizeof(struct pseudohdr) + segment_len);

pseudoh->source_addr = iph->ip_src; 
pseudoh->dest_addr = iph->ip_dst;   
pseudoh->dummy = 0;
pseudoh->protocol = iph->ip_p;
pseudoh->len = htons(segment_len);

data_len = segment_len - data_offset;
memcpy((char *)pseudoh + sizeof(struct pseudohdr), header, data_offset);
memcpy((char *)pseudoh + sizeof(struct pseudohdr) + data_offset,
    packet + iph->ip_hl * 4 + data_offset, data_len);

checksum = in_cksum((unsigned short *)pseudoh, sizeof(struct pseudohdr) + segment_len);
#ifdef DEBUG
    printf("segment_len:%d\n", segment_len);
    printf("data_offset:%d\n", data_offset);
    printf("data_len:%d\n", data_len);
    printf("checksum:%x\n", ntohs(checksum));
#endif
return(checksum);
}
#endif /* NO_NET_PACKET */

/* ------------------ socket->filestream stuff for windows ------------------------*/

#ifdef WINDOWS
extern int IOchannelIsSocketStream;

/*
These functions use the FILE structure to store the raw file handle in '->_file' and
set ->_flag to 0xFFFF, to identify this as a faked FILE structure.
Sinc 10.0.1 the IOchannelIsSocketStream flag is used to identify IOchannel as
a fake file struct and extract the socket. Following win_fxxx routines
are used to define fopen(), fclose(), fprintf(), fgetc() and fgets() in some *.c
*/

FILE * win_fdopen(int handle, const char * mode)
{
FILE * fPtr;

if((fPtr = (FILE *)malloc(sizeof(FILE))) == NULL)
    return(NULL);

memset(fPtr, 0, sizeof(FILE));

fPtr->_file = handle;
fPtr->_flag = 0xFFFF;

return(fPtr);
}

int win_fclose(FILE * fPtr)
{
if(IOchannelIsSocketStream)
   return(close(getSocket(fPtr)));

return(fclose(fPtr));
}


int win_fprintf(FILE * fPtr, char * notused, char * buffer)
{
int pSize;

if(!IOchannelIsSocketStream)
    return(fprintf(fPtr, buffer));

pSize = strlen(buffer);

if((pSize = sendall(getSocket(fPtr), buffer, pSize)) == SOCKET_ERROR)
     {
     close(getSocket(fPtr));
     return(-1);
     }

return(pSize);
}

int win_fgetc(FILE * fPtr)
{
char chr;

if(!IOchannelIsSocketStream)
    return(fgetc(fPtr));

if(recv(getSocket(fPtr), &chr, 1, NO_FLAGS_SET) <= 0)
    {
    close(getSocket(fPtr));
    return(-1);
    }

return(chr);
}


char * win_fgets(char * buffer, int size, FILE * fPtr)
{
int bytesReceived = 0;
char chr;

if(!IOchannelIsSocketStream)
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

#endif /* WINDOWS */

#else /* for EMSCRIPTEN define dummy writeLog() */

void writeLog(char * text, int newLine) { return; }

#endif /* ifndef EMSCRIPTEN */
/* eof */
