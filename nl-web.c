/* nl-web.c --- HTTP network protocol routines for newLISPD

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
#include <errno.h>
#include "protos.h"
 
#ifdef WINDOWS
#include <winsock2.h>
#else
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#define BUFFSIZE 10240

#ifndef WINDOWS
#define SOCKET_ERROR -1
#else
#define fgets win_fgets
#define close closesocket  /* for file operations on Windows use _close */
#endif

/* from nl-sock.c */
extern UINT netErrorIdx;
extern char * netErrorMsg[];

/* from newlisp.c */
extern int httpSafe;

#define OK_FILE_DELETED "file deleted"

#define MAX_PROTOCOL 8
#define NO_FLAGS_SET 0

char * requestMethod[] = {"GET", "HEAD", "PUT", "PUT", "POST", "DELETE"};

/* with MinGW gcc 3.4.5 not needed
#ifdef WINDOWS
struct timezone {
       int     tz_minuteswest;
       int     tz_dsttime;
};

int gettimeofday( struct timeval *tp, struct timezone *tzp );
#endif
*/

#ifdef WINDOWS
extern int IOchannelIsSocketStream;
#endif

extern SYMBOL * transferEvent;

ssize_t readFile(char * fileName, char * * buffer);
int writeFile(char * fileName, char * buffer, size_t size, char * type);
int parseUrl(char *url, char * protocol, char * host, int * port, char * path, size_t bufflen);
void parsePath(char * url, char * path, size_t bufflen);
size_t parseValue(char * str);
void trimTrailing(char * ptr);
CELL * webError(int no, int sockno);
CELL * base64(CELL * params, int type);

#ifndef EMSCRIPTEN
jmp_buf socketTimeoutJump;
INT socketTimeout = 0;
struct timeval socketStart;

/* socket send and receive routines with timeout */
int recvc_tm(int sock)
{
struct timeval tm;
unsigned char chr;
ssize_t bytes;

while(wait_ready(sock, 1000, 0) <= 0)
  {
  if(socketTimeout)
    {
    gettimeofday(&tm, NULL);
    if(timediff_ms(tm, socketStart) > socketTimeout)
        longjmp(socketTimeoutJump, 1);
    }
  } 
   
bytes = recv(sock, (void *)&chr, 1, NO_FLAGS_SET);
if(bytes <= 0) return(-1);

return(chr);
}


char * recvs_tm(char * buffer, size_t size, int sock)
{
ssize_t bytesReceived = 0;
int chr;

while(bytesReceived < size)
    {
    if((chr = recvc_tm(sock)) < 0)
        {
        if(bytesReceived == 0)
            return(NULL);
        else break;
        }

    *(buffer + bytesReceived++) = chr;
    if(chr == '\n') break;
    }

*(buffer + bytesReceived) = 0;
return(buffer);
}

void wait_until_read_ready(int sock)
{
struct timeval tm;

while(wait_ready(sock, 1000, 0) <= 0)
    {
    if(socketTimeout)
        {
        gettimeofday(&tm, NULL);
        if(timediff_ms(tm, socketStart) > socketTimeout)
        longjmp(socketTimeoutJump, 1);
        }
    }
}

size_t recvsize_tm(char * buffer, size_t size, int sock, int flag)
{
ssize_t sizeRead = 0;
size_t resultSize = 0;

wait_until_read_ready(sock);
memset(buffer, 0, size);
while(size)
  { 
  sizeRead = recv(sock, buffer + resultSize, size, NO_FLAGS_SET);
  if(sizeRead <= 0)
      {
      sizeRead = 0;
      break;
      }
  resultSize += sizeRead;
  size -= sizeRead;
  if(flag && transferEvent != nilSymbol)
        executeSymbol(transferEvent, stuffInteger(sizeRead), NULL);
  wait_until_read_ready(sock);
  }

return(resultSize);
}

ssize_t sendf(int sock, int debug, char * format, ...)
{
char * buffer;
va_list argptr;
int result;
 
va_start(argptr,format);
/* new in 7201 , defined in nl-filesys.c if not in libc */
vasprintf(&buffer, format, argptr); 

result = send(sock, buffer, strlen(buffer), NO_FLAGS_SET);
if(debug) varPrintf(OUT_CONSOLE, "%s", buffer);

freeMemory(buffer);
va_end(argptr);

return(result);
}

CELL * p_getUrl(CELL * params)
{
return(getPutPostDeleteUrl(NULL, params, HTTP_GET, CONNECT_TIMEOUT));
}


CELL * p_putUrl(CELL * params)
{
return(getPutPostDeleteUrl(NULL, params, HTTP_PUT, CONNECT_TIMEOUT));
}


CELL * p_postUrl(CELL * params)
{
return(getPutPostDeleteUrl(NULL, params, HTTP_POST, CONNECT_TIMEOUT));
}

CELL * p_deleteUrl(CELL * params)
{
return(getPutPostDeleteUrl(NULL, params, HTTP_DELETE, CONNECT_TIMEOUT));
}

int transfer(int sock, char * buff, int len)
{
int bytesSend = 0, n;

while(bytesSend < len)
    {
    if((n = sendall(sock, buff + bytesSend, 
                (len - bytesSend) > BUFFSIZE ? BUFFSIZE : len - bytesSend) )
        == SOCKET_ERROR)
        return(SOCKET_ERROR);
    bytesSend += n;
    if(transferEvent != nilSymbol)
        executeSymbol(transferEvent, stuffInteger(bytesSend), NULL);
    }

return(bytesSend);
}

CELL * getPutPostDeleteUrl(char * url, CELL * params, int type, int timeout)
{
char * proxyUrl, * putPostStr = NULL, *contentType;
char * protocol;
char * host;
char * pHost;
char * path;
char * customHeader = NULL;
size_t bufflen;
int port, pPort, sock = 0;
char * option, * method = NULL;
char * buff;
char resultTxt[128];
char * buffPtr;
char * resultPtr = NULL;
int haveContentLength = FALSE, headRequest = FALSE, listFlag = FALSE, debugFlag = FALSE, rawFlag = FALSE;
int chunked = FALSE;
ssize_t sizeRead = 0;
size_t resultSize = 0, fSize = 0, size = 0;
CELL * result, * cell;
CELL * headerCell = NULL;
int ch, len;
int responseLoop;
int statusCode;

buff = alloca(BUFFSIZE);

/* reset net-error */
netErrorIdx = 0;

/* get parameters */

if(url == NULL)
    params = getString(params, &url);

if(type == HTTP_PUT || type == HTTP_PUT_APPEND || type == HTTP_POST)
    params = getStringSize(params, &putPostStr, &size, TRUE);


if(my_strnicmp(url, "file://", 7) == 0)
    {
    if(type == HTTP_GET)
        {
        if((size = readFile(url, &buffPtr)) == -1)
            return(webError(ERROR_FILE_OP, sock));
        return(makeStringCell(buffPtr, size));
        }
    if(type == HTTP_PUT)
        {
        if(writeFile(url, putPostStr, size, "w") == -1)
            return(webError(ERROR_FILE_OP, sock));
        snprintf(resultTxt, 64, "%u bytes written", (unsigned int)size);
        return(stuffString(resultTxt)); /* not an error */
        }
    if(type == HTTP_DELETE)
        {
        url = getLocalPath(url);
        return(unlink(url) == 0 ? stuffString(OK_FILE_DELETED) 
								: webError(ERROR_FILE_OP, sock));
        }

    return(webError(ERROR_BAD_URL, sock));
    }


if(type == HTTP_POST)
    {
    if(params->type != CELL_NIL)
        params = getString(params, &contentType);
    else 
        contentType = "application/x-www-form-urlencoded";
    }

result = evaluateExpression(params);
params = params->next;

if(isNumber(result->type))
    {
    getIntegerExt(result, (UINT*)&socketTimeout, FALSE);
    /* set connection timeout to total-timeout specified by user */
    timeout = socketTimeout;
    }

else if(result->type == CELL_STRING)
    {
    option = (char *)result->contents;    
    len = result->aux - 1;
    if(searchBuffer(option, len, "header", 6, 0) != -1)
      headRequest = TRUE;
    if(searchBuffer(option, len, "list", 4, 0) != -1)
      listFlag = TRUE;
    if(searchBuffer(option, len, "debug", 5, 0) != -1)
      debugFlag = TRUE;
    if(searchBuffer(option, len, "raw", 3, 0) != -1)
      rawFlag = TRUE;

    if(params != nilCell)
        params = getInteger(params, (UINT*)&socketTimeout);
    }
else if(result != nilCell)
    return(errorProcExt(ERR_NUMBER_OR_STRING_EXPECTED, result));

/* if total timeout is specified, custom-header can be specified too */
if(socketTimeout && params != nilCell)
        getString(params, &customHeader);

bufflen = strlen(url) + 1;
if(bufflen < MAX_URL_LEN + 1) bufflen = MAX_URL_LEN + 1;

protocol = alloca(8);
host = alloca(bufflen);
pHost = alloca(bufflen);
path = alloca(bufflen);

/* parse URL for parameters */
if(parseUrl(url, protocol, host, &port, path, bufflen) == FALSE)
    return(webError(ERROR_BAD_URL, sock));

/* printf("protocol: %s host:%s port %d path:%s\n", protocol, host, port, path); */

proxyUrl = getenv("HTTP_PROXY");

if(proxyUrl == NULL)
    {
    strncpy(pHost, host, bufflen);
    pPort = port;
    }
else
    {
    if(parseUrl(proxyUrl, protocol, pHost, &pPort, NULL, bufflen) == FALSE)
        return(webError(ERROR_BAD_URL, sock));
    }

/* start timer */
gettimeofday(&socketStart, NULL);
/* connect to host */
CONNECT_TO_HOST:
if(sock) 
    close(sock);


if((sock = netConnect(pHost, pPort, SOCK_STREAM, 0, timeout)) == SOCKET_ERROR)
    return(webError(netErrorIdx, sock));

if(type == HTTP_GET)
    if(headRequest == TRUE) type = HTTP_HEAD;
method = requestMethod[type];

/* send header */
if(proxyUrl != NULL)
    sendf(sock, debugFlag, "%s %s://%s:%d/%s HTTP/1.1\r\n", method, protocol, host, port, path);
else
    sendf(sock, debugFlag, "%s /%s HTTP/1.1\r\n", method, path);

/* obligatory host spec */
sendf(sock, debugFlag, "Host: %s\r\n", host);

/* send optional custom header entries */
if (customHeader != NULL)
    sendf(sock, debugFlag, "%s", customHeader);
else
    sendf(sock, debugFlag, "User-Agent: newLISP v%d\r\n", version);

sendf(sock, debugFlag, "%s", "Connection: close\r\n");

/* expanded header for PUT, POST and body */
if(type == HTTP_PUT || type == HTTP_PUT_APPEND)
    {
    if(type == HTTP_PUT_APPEND) sendf(sock, debugFlag, "%s", "Pragma: append\r\n");
    if(customHeader == NULL)
        sendf(sock, debugFlag, "Content-type: text/html\r\nContent-length: %d\r\n\r\n", size);
    else
        sendf(sock, debugFlag, "Content-length: %d\r\n\r\n", size);

    if(transfer(sock, putPostStr, size) == SOCKET_ERROR) 
        return(webError(ERROR_TRANSFER, sock));
    if(debugFlag) varPrintf(OUT_CONSOLE, "%s", putPostStr);
    }
else if(type == HTTP_POST)
    {
    sendf(sock, debugFlag, "Content-type: %s\r\nContent-length: %d\r\n\r\n", contentType, size);
    if(transfer(sock, putPostStr, size) == SOCKET_ERROR) 
        return(webError(ERROR_TRANSFER, sock));
    if(debugFlag) varPrintf(OUT_CONSOLE, "%s", putPostStr);
    }
else /* HTTP_GET, HTTP_DELETE */
    sendf(sock, debugFlag, "%s", "\r\n");

if(setjmp(socketTimeoutJump) != 0)
    {
    if(sock) close(sock);
    if(resultPtr != NULL) free(resultPtr);
    return(webError(ERR_INET_TIMEOUT, sock));
    }
  
/* Retrieve HTTP response and check for status code. */
responseLoop = 0;
READ_RESPONSE:
if(++responseLoop == 4)
        return(webError(ERROR_INVALID_RESPONSE, sock));


if (recvs_tm(buff, BUFFSIZE, sock) == NULL) 
   return(webError(ERROR_NO_RESPONSE, sock));

if(debugFlag) varPrintf(OUT_CONSOLE, "\n%s\n", buff);

/* go past first token */
for (buffPtr = buff; *buffPtr != '\0' && !isspace((int)*buffPtr); ++buffPtr) {;}

/* trim leading spaces */
while(isspace((int)*buffPtr)) ++buffPtr;

/* get status code */
statusCode = atoi(buffPtr);
if(statusCode >= 400)
    snprintf(resultTxt, 128, "ERR: %s", buff);
else
    snprintf(resultTxt, 128, "%s", buff);

switch (statusCode) 
    {
    case 0:
    case 100:
        /* drain and continue */
        while (ch = recvc_tm(sock), ch != EOF && ch != '\n') {;}
        goto READ_RESPONSE;
    case 200:
    case 201:
    case 202:
    case 203:
    case 204:
    case 205:
    case 206:
    case 300:
    case 301:
    case 302:
    case 303:
    case 307:
        break;
    default:
        break;
    }

/* Retrieve HTTP headers. */
memset(buff, 0, BUFFSIZE);
if(listFlag || headRequest)
    headerCell = stuffString("");

/* Retrieve header */
while(strcmp(buff, "\r\n") != 0 && strcmp(buff, "\n") != 0)
    {
    if(recvs_tm(buff, BUFFSIZE, sock) == NULL)
        return(webError(ERROR_HEADER, sock));

    if(listFlag || headRequest) appendCellString(headerCell, buff, strlen(buff));

    if(my_strnicmp(buff, "content-length:", 15) == 0)
        {
        fSize = parseValue(buff + 15);
        haveContentLength = TRUE;
        }
    /* 302 contradicts standard for redirection but is common practice */
    if(my_strnicmp(buff, "location:", 9) == 0 && !rawFlag &&
                (statusCode == 301 || statusCode == 302 || statusCode == 303))
        {
        buffPtr = buff + 9;
        while(isspace((int)*buffPtr)) ++buffPtr;
        if(*buffPtr == '/')
            strncpy(path, buffPtr + 1, bufflen);
        else /* its a url or path */
            {
            if(parseUrl(buffPtr, protocol, host, &port, path, bufflen) == FALSE)
                /* path only */
                parsePath(buffPtr, path, buffPtr - buff);

            if(proxyUrl == NULL)
                {
                strncpy(pHost, host, bufflen);
                pPort = port;
                }
            }

        if(headerCell) deleteList(headerCell);  
        goto CONNECT_TO_HOST;
        }

    if(my_strnicmp(buff, "Transfer-Encoding:", 18) == 0 
        && searchBuffer(buff, strlen(buff), "chunked", 7, 0) != -1)
        chunked = TRUE;
    }
    
if(headRequest)
    return(headerCell);

/* changed in 10.6.4 should allow 0-length contents and 204 handled earlier */
/* if((haveContentLength == TRUE && fSize == 0) || statusCode == 204) */
if(statusCode == 204)
    return(webError(ERROR_NO_CONTENT, sock));

/* Retrieve HTTP body. */
else if(chunked == TRUE)
    {
    resultPtr = NULL;
    if(recvs_tm(buff, BUFFSIZE, sock) == NULL)
        return(webError(ERROR_NO_CONTENT, sock));
    while((size = strtoul(buff, NULL, 16)) > 0)
        {
        if(resultSize == 0)
            resultPtr = allocMemory(size + 1);
        else 
            resultPtr = reallocMemory(resultPtr, resultSize + size + 1);
        if(recvsize_tm(resultPtr + resultSize, size, sock, TRUE) != size)
            {
            free(resultPtr);
            return(webError(ERROR_CHUNKED_FORMAT, sock));
            }
        resultSize += size;
        recvs_tm(buff, BUFFSIZE, sock); /* empty line */
        recvs_tm(buff, BUFFSIZE, sock); /*  chunck size  */
        }
    }

else if(haveContentLength == TRUE && fSize)
    {
    resultPtr = allocMemory(fSize + 1);
    if((resultSize = recvsize_tm(resultPtr, fSize, sock, TRUE)) == 0)
        {
        free(resultPtr);
        return(webError(ERROR_NO_CONTENT, sock));
        }
    }
    
else /* no content length given, relies on host closing the connection */
    {
    resultPtr = allocMemory(BUFFSIZE + 1);
    resultSize = 0;
    size = BUFFSIZE;
    while ((sizeRead = recvsize_tm(buff, BUFFSIZE, sock, FALSE)) > 0)
        {
        if((resultSize + sizeRead) > size)
            {
            size = resultSize + BUFFSIZE;
            resultPtr = reallocMemory(resultPtr, size + 1);
            }
        memcpy(resultPtr + resultSize, buff, sizeRead);
        resultSize += sizeRead;
        if(transferEvent != nilSymbol)
            executeSymbol(transferEvent, stuffInteger(sizeRead), NULL);
        }
    }


if(resultPtr == NULL)
    {
    if(statusCode < 400)
        result = stuffString("");
    else 
        result = stuffString(resultTxt);
    }
else
    {
    result = getCell(CELL_STRING);
    if(statusCode >= 400 && listFlag == FALSE)
        {
        bufflen = strlen(resultTxt);
        buffPtr = allocMemory(bufflen + resultSize + 1);
        memcpy(buffPtr, resultTxt, bufflen);
        memcpy(buffPtr + bufflen, resultPtr, resultSize);
        free(resultPtr);
        resultPtr = buffPtr;
        resultSize += bufflen;
        }       
    *(resultPtr + resultSize) = 0;
    result->contents = (UINT)resultPtr;
    result->aux = resultSize + 1;
    }

close(sock);

if(listFlag)
    {
    cell = getCell(CELL_EXPRESSION);
    addList(cell, headerCell); 
    addList(cell, result);
    addList(cell, stuffString(statusCode >= 400 ? resultTxt + 5 : resultTxt));
    addList(cell, stuffInteger(statusCode));
    return(cell);
    }

return(result);
}


int parseUrl(char * url, char * protocol, char * host, int * port, char * path, size_t bufflen)
{
char * bracketPtr = NULL;
char * colonPtr = NULL;
char * slashPtr;
int len;

/* trim trailing whitespace like '/r/n' from url */
len = strlen(url);
while(*(url + len) <= ' ' && len > 0)
    {
    *(url + len) = 0;
    len--;
    }

*port = 80;

if(my_strnicmp(url, "http://", 7) == 0)
    {
    strncpy(protocol,"http", MAX_PROTOCOL );
    if( (ADDR_FAMILY == AF_INET6) && (*(url + 7) == '[') )
        strncpy(host, url+8, bufflen);
    else
        strncpy(host, url+7, bufflen);
    }
else if( my_strnicmp(url, "https://", 8) == 0)
    {
    strncpy(protocol, "https", MAX_PROTOCOL);
    if( (ADDR_FAMILY == AF_INET6) && (*(url + 8) == '[') )
        strncpy(host, url+9, bufflen);
    else
        strncpy(host, url+8, bufflen);
    }
else 
    return(FALSE);

if(ADDR_FAMILY == AF_INET6)
    bracketPtr = strchr(host, ']');
else
    colonPtr = strchr(host, ':');

slashPtr = strchr(host, '/');

if(ADDR_FAMILY == AF_INET6)
    {
    if (bracketPtr != NULL && (slashPtr == NULL || bracketPtr < slashPtr))
        {
        *bracketPtr = '\0';
        if(*(bracketPtr + 1) == ':')
            *port = atoi(bracketPtr + 2);
        }
    else
        {
        colonPtr = strchr(host, ':');
        if (colonPtr != NULL && (slashPtr == NULL || colonPtr < slashPtr)) 
            {
            *colonPtr++ = '\0';
            *port = atoi(colonPtr);
            }
        }
    }
else
    {   
    if (colonPtr != NULL && (slashPtr == NULL || colonPtr < slashPtr)) 
        {
        *colonPtr++ = '\0';
        *port = atoi(colonPtr);
        }
    }

if(path == NULL) return(TRUE);

if (slashPtr != NULL) 
    {
    *slashPtr++ = '\0';
    strncpy(path, slashPtr, bufflen);
    } 
else
    strncpy(path, "", bufflen);

/* printf("protocol:%s host:%s port:%d path:%s\n", protocol, host, *port, path); */

return(TRUE);
}

void parsePath(char * url, char * path, size_t bufflen)
{
int len;

/* trim trailing whitespace like '/r/n' from url */
len = strlen(url);
while(*(url + len) <= ' ' && len > 0)
    {
    *(url + len) = 0;
    len--;
    }
    
/* trim leading whitespace */
while(*url <= ' ') url++;
strncpy(path, url, bufflen);
}

size_t parseValue(char * str)
{
while(!isDigit((unsigned char)*str) && *str != 0) ++str;
return atol(str);
}

CELL * webError(int errorNo, int sockno)
{
char msg[64];


if(sockno) close(sockno);
netErrorIdx = errorNo;
snprintf(msg, 64, "ERR: %s", netErrorMsg[errorNo]);

return(stuffString(msg));
}
#endif /* ifndef EMSCRIPTEN */

/***************************************************************************
 *                                  _   _ ____  _
 *  Project                     ___| | | |  _ \| |
 *                             / __| | | | |_) | |
 *                            | (__| |_| |  _ <| |___
 *                             \___|\___/|_| \_\_____|
 *
 * Copyright (C) 1998 - 2004, Daniel Stenberg, <daniel@haxx.se>, et al.
 *
 * This software is licensed as described in the file COPYING, which
 * you should have received as part of this distribution. The terms
 * are also available at http://curl.haxx.se/docs/copyright.html.
 *
 * You may opt to use, copy, modify, merge, publish, distribute and/or sell
 * copies of the Software, and permit persons to whom the Software is
 * furnished to do so, under the terms of the COPYING file.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied.
 *
 * $Id: base64.c,v 1.32 2004/12/15 01:38:25 danf Exp $
 ***************************************************************************/

/* Base64 encoding/decoding

   this file from the cURL project is included in nl-web.c for the
   newLISP functions 'base64-enc' and 'base64-dec' 

   all #include statements have and the test harness rootines have
   been stripped.  2005-1-6 Lutz Mueller
*/


static void decodeQuantum(unsigned char *dest, const char *src)
{
  unsigned int x = 0;
  int i;
  for(i = 0; i < 4; i++) {
    if(src[i] >= 'A' && src[i] <= 'Z')
      x = (x << 6) + (unsigned int)(src[i] - 'A' + 0);
    else if(src[i] >= 'a' && src[i] <= 'z')
      x = (x << 6) + (unsigned int)(src[i] - 'a' + 26);
    else if(src[i] >= '0' && src[i] <= '9')
      x = (x << 6) + (unsigned int)(src[i] - '0' + 52);
    else if(src[i] == '+')
      x = (x << 6) + 62;
    else if(src[i] == '/')
      x = (x << 6) + 63;
    else if(src[i] == '=')
      x = (x << 6);
  }

  dest[2] = (unsigned char)(x & 255);
  x >>= 8;
  dest[1] = (unsigned char)(x & 255);
  x >>= 8;
  dest[0] = (unsigned char)(x & 255);
}

/*
 * Curl_base64_decode()
 *
 * Given a base64 string at src, decode it into the memory pointed to by
 * dest. Returns the length of the decoded data.
 */
size_t Curl_base64_decode(const char *src, char *dest)
{
  int length = 0;
  int equalsTerm = 0;
  int i;
  int numQuantums;
  unsigned char lastQuantum[3];
  size_t rawlen=0;

  while((src[length] != '=') && src[length])
    length++;
  while(src[length+equalsTerm] == '=')
    equalsTerm++;

  if(equalsTerm > 3) equalsTerm = 3; /* LM added 2006-09-08 */

  numQuantums = (length + equalsTerm) / 4;

  if(numQuantums == 0) return(0);

  rawlen = (numQuantums * 3) - equalsTerm;

  for(i = 0; i < numQuantums - 1; i++) {
    decodeQuantum((unsigned char *)dest, src);
    dest += 3; src += 4;
  }

  decodeQuantum(lastQuantum, src);
  for(i = 0; i < 3 - equalsTerm; i++)
    dest[i] = lastQuantum[i];

  return rawlen;
}

#define BASE64_ENC 0
#define BASE64_DEC 1

CELL * p_base64Enc(CELL * params) { return(base64(params, BASE64_ENC)); }
CELL * p_base64Dec(CELL * params) { return(base64(params, BASE64_DEC)); }

CELL * base64(CELL * params, int type)
{
char * inPtr;
char * outPtr;
size_t sizein, sizeout;
int emptyFlag = 0;

params = getStringSize(params, &inPtr, &sizein, TRUE);
emptyFlag = getFlag(params);

if(type == BASE64_ENC)
    {
    if(sizein == 0)
        return(emptyFlag ? stuffString("") : stuffString("===="));
    if((sizeout = Curl_base64_encode(inPtr, sizein, &outPtr)) == 0)
        return(stuffString(""));
    }
else    /* BASE64_DEC */
    {
    outPtr = allocMemory((sizein * 3) / 4 + 9);
    sizeout = Curl_base64_decode(inPtr, outPtr);
    *(outPtr + sizeout) = 0;
    }
    
/*
strCell = getCell(CELL_STRING);
strCell->contents = (UINT)outPtr;
strCell->aux = sizeout + 1;
return(strCell);
*/

return(makeStringCell(outPtr, sizeout));
}

/* ---- Base64 Encoding --- */
static const char table64[]=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/*
 * Curl_base64_encode()
 *
 * Returns the length of the newly created base64 string. The third argument
 * is a pointer to an allocated area holding the base64 data. If something
 * went wrong, -1 is returned.
 *
 */
size_t Curl_base64_encode(const char *inp, size_t insize, char **outptr)
{
  unsigned char ibuf[3];
  unsigned char obuf[4];
  int i;
  int inputparts;
  char *output;
  char *base64data;

  char *indata = (char *)inp;

  *outptr = NULL; /* set to NULL in case of failure before we reach the end */

  if(0 == insize)
    insize = strlen(indata);

  base64data = output = (char*)malloc(insize*4/3+4);
  if(NULL == output)
    return 0;

  while(insize > 0) {
    for (i = inputparts = 0; i < 3; i++) {
      if(insize > 0) {
        inputparts++;
        ibuf[i] = *indata;
        indata++;
        insize--;
      }
      else
        ibuf[i] = 0;
    }

    obuf [0] = (ibuf [0] & 0xFC) >> 2;
    obuf [1] = ((ibuf [0] & 0x03) << 4) | ((ibuf [1] & 0xF0) >> 4);
    obuf [2] = ((ibuf [1] & 0x0F) << 2) | ((ibuf [2] & 0xC0) >> 6);
    obuf [3] = ibuf [2] & 0x3F;

    switch(inputparts) {
    case 1: /* only one byte read */
      snprintf(output, 5, "%c%c==",
               table64[obuf[0]],
               table64[obuf[1]]);
      break;
    case 2: /* two bytes read */
      snprintf(output, 5, "%c%c%c=",
               table64[obuf[0]],
               table64[obuf[1]],
               table64[obuf[2]]);
      break;
    default:
      snprintf(output, 5, "%c%c%c%c",
               table64[obuf[0]],
               table64[obuf[1]],
               table64[obuf[2]],
               table64[obuf[3]] );
      break;
    }
    output += 4;
  }
  *output=0;
  *outptr = base64data; /* make it return the actual data memory */

  return strlen(base64data); /* return the length of the new data */
}
/* ---- End of Base64 Encoding ---- */


#ifndef EMSCRIPTEN
/* --------------------------- HTTP server mode -----------------------------
   Handles GET, POST, PUT and DELETE requests
   handles queries in GET requests and sets environment variables 
   DOCUMENT_ROOT, REQUEST_METHOD, SERVER_SOFTWARE and QUERY_STRING,
   and when present in client request header HTTP_HOST, HTTP_USER_AGENT
   and HTTP_COOKIE. REMOTE_ADDR is set when the client connects.
   Subset HTTP/1.0 compliant.
*/
#ifndef LIBRARY
/* #define DEBUGHTTP  */
#define SERVER_SOFTWARE "newLISP/10.7.5"

int sendHTTPmessage(int status, char * description, char * request);
void handleHTTPcgi(char * command, char * query, ssize_t querySize);
size_t readHeader(char * buff, int * pragmaFlag);
ssize_t readPayLoad(ssize_t size, char * content, int outFile, char * request);
int endsWith(char * str, char * ext);
char * getMediaType(char * request);
void url_decode(char *dest, char *src);

void sendHTTPpage(char * content, size_t size, char * media)
{
int pos = 0;
char status[128];

memset(status, 0, 128);
if(strncmp(content, "Status:", 7) == 0)
    {
    /* get content after */
    while(*(content + pos) >= 32) pos++;
    memcpy(status, content + 7, pos - 7); 
    content = content + pos;
    if(size) size -= pos;
    while(*content == '\r' || *content == '\n') { content++; size--; }
    }
else
    strncpy(status, "200 OK", 6);

varPrintf(OUT_CONSOLE, "HTTP/1.0 %s\r\n", status);
varPrintf(OUT_CONSOLE, "Server: newLISP v.%d (%s)\r\n", version, OSTYPE);
#ifdef DEBUGHTTP
puts("# Header sent:");
printf("HTTP/1.0 %s\r\n", status);
printf("Server: newLISP v.%d (%s)\r\n", version, OSTYPE);
#endif

if(media != NULL)
    {
    varPrintf(OUT_CONSOLE, "Content-length: %d\r\nContent-type: %s\r\n\r\n", size, media);
#ifdef DEBUGHTTP
    printf("Content-length: %d\r\nContent-type: %s\r\n\r\n", (int)size, media);
#endif
    }
#ifndef WINDOWS
size = write(fileno(IOchannel), content, size); 
fflush(IOchannel); 
fclose(IOchannel); 
IOchannel = NULL;
#else /* it is WINDOWS */
if(IOchannel != NULL && IOchannelIsSocketStream)
    {
    sendall(getSocket(IOchannel), content, size);
    close(getSocket(IOchannel));
    }
else 
    varPrintf(OUT_CONSOLE, "%s", content);
return;
#endif
#ifdef DEBUGHTTP
printf("# content:%s:\r\n", content);
fflush(stdout);
#endif
}

#define MAX_BUFF 1024
#define ERROR_404 "File or Directory not found"
#define ERROR_411 "Length required for"
#define ERROR_500 "Server error"
#define DEFAULT_PAGE_1 "index.html"
#define DEFAULT_PAGE_2 "index.cgi"
#define CGI_EXTENSION ".cgi"
#define MEDIA_TEXT "text/plain"


int executeHTTPrequest(char * request, int type)
{
char * sptr;
char * query;
char * content = NULL;
char * decoded;
char buff[MAX_BUFF];
ssize_t transferred, size;
char * mediaType;
CELL * result = NULL;
int outFile;
int pragmaFlag;
int len;
char * fileMode = "w";

if(chdir(startupDir) < 0)
    fatalError(ERR_IO_ERROR, 0, 0);
query = sptr = request;

setenv("DOCUMENT_ROOT", startupDir, 1);
setenv("SERVER_SOFTWARE", SERVER_SOFTWARE, 1);
setenv("REQUEST_METHOD", requestMethod[type], 1);

#ifdef DEBUGHTTP
printf("# HTTP request:%s:%s:\r\n", request, requestMethod[type]);
#endif

/* stuff after request */
while(*sptr > ' ') ++sptr;
*sptr = 0;
while(*query != 0 && *query != '?') ++query;
if(*query == '?')
    {
    *query = 0;
    query++;
    }

setenv("QUERY_STRING", query, 1);

/* do url_decode */
decoded = alloca(strlen(request) + 1);
url_decode(decoded, request);
request = decoded;

setenv("REQUEST_URI", request, 1); /* 10.7.4 */

/* change to base dir of request file */
sptr = request + strlen(request);
while(*sptr != '/' && sptr != request) --sptr;
if(*sptr == '/') 
    {
    *sptr = 0;
    sptr++;
    if(chdir(request))
        {
        sendHTTPmessage(404, ERROR_404, request);
        return(TRUE);
        }
    request = sptr;
    }

if((len = strlen(request)) == 0)
    {
    if(isFile(DEFAULT_PAGE_2, 0) == 0) 
        request = DEFAULT_PAGE_2;
    else
        request = DEFAULT_PAGE_1;
    len = strlen(request);
    }

size = readHeader(buff, &pragmaFlag);
switch(type)
    {
    case HTTP_GET:
    case HTTP_HEAD:
        if(endsWith(request, CGI_EXTENSION))
            handleHTTPcgi(request, query, strlen(query));
        else
            {
            mediaType = getMediaType(request);

            if(type == HTTP_HEAD)
                {
                snprintf(buff, MAX_BUFF - 1, 
                    "Content-length: %"PRId64"\r\nContent-type: %s\r\n\r\n",
                    fileSize(request), mediaType);
                sendHTTPpage(buff, strlen(buff), NULL);
                }
            else
                {
                if((size = readFile(request, &content)) == -1)
                    sendHTTPmessage(404, ERROR_404, request);
                else
                    sendHTTPpage(content, size, mediaType);
                if(content) free(content);
                }
            }
        break;

    case HTTP_DELETE:
        if(httpSafe)
            {
            sendHTTPpage("Server in safe mode", 19, MEDIA_TEXT);
            break;
            }
            
        if(unlink(request) != 0)    
            sendHTTPmessage(500, "Could not delete", request);
        else
            sendHTTPpage("File deleted", 12, MEDIA_TEXT);
        break;

    case HTTP_POST:
        if(!size)
            {
            sendHTTPmessage(411, ERROR_411, request);
            break;
            }

        query = callocMemory(size + 1);

        if(readPayLoad(size, query, 0, request) == -1)
            {
            free(query);
            break;
            }

        handleHTTPcgi(request, query, size);
        free(query); 
        break;

    case HTTP_PUT:
        if(httpSafe)
            {
            sendHTTPpage("Server in safe mode", 19, request);
            break;
            }
        if(pragmaFlag) fileMode = "a";

        if(!size)
            {
            sendHTTPmessage(411, ERROR_411, request);
            break;
            }

        if( (outFile = openFile(request, fileMode, NULL)) == (int)-1)
            {
            sendHTTPmessage(500, "cannot create file", request);
            break;
            }

        transferred = readPayLoad(size, buff, outFile, request);
#ifdef WINDOWS
        _close(outFile);
#else
        close(outFile);
#endif
        if(transferred != -1)
            {
            snprintf(buff, 255, "%d bytes transferred for %s\r\n", (int)transferred, request);
            sendHTTPpage(buff, strlen(buff), MEDIA_TEXT);
            }
        break;

    default:
        break;
    }

if(chdir(startupDir) < 0) fatalError(ERR_IO_ERROR, 0, 0);
if(result != NULL) deleteList(result);
return(TRUE);
}


int sendHTTPmessage(int status, char * desc, char * req)
{
char msg[256];

snprintf(msg, 256, "Status:%d %s\r\nERR:%d %s: %s\r\n", status, desc, status, desc, req);
sendHTTPpage(msg, strlen(msg), MEDIA_TEXT);
return(0);
}


/* remove leading white space */ 
char * trim(char * buff) 
{
char * ptr = buff;
while(*ptr <= ' ') ptr++;
return(ptr);
}

/* retrieve rest of header */
size_t readHeader(char * buff, int * pragmaFlag)
{
size_t size = 0;
int offset;
char numStr[16];

*pragmaFlag = 0;

memset(buff, 0, MAX_LINE);

setenv("HTTP_HOST", "", 1);
setenv("HTTP_USER_AGENT", "", 1);
setenv("HTTP_COOKIE", "", 1);
setenv("HTTP_AUTHORIZATION", "", 1);

while(fgets(buff, MAX_LINE - 1, IOchannel) != NULL)
    {
    if(strcmp(buff, "\r\n") == 0 || strcmp(buff, "\n") == 0) break;

    /* trim trailing white space */
    offset = strlen(buff) - 1;
    while(offset > 0 && *(buff + offset) <= ' ') 
        *(buff + offset--) = 0; 

    if(my_strnicmp(buff, "content-length:", 15) == 0)
        {
        size = parseValue(buff + 15);
#if defined(WINDOWS) || defined(TRU64)
        snprintf(numStr, 16, "%lu", (long unsigned int)size);
#else
        snprintf(numStr, 16, "%llu", (long long unsigned int)size);
#endif
        setenv("CONTENT_LENGTH", numStr, 1);
        }
    if(my_strnicmp(buff, "pragma: append", 14) == 0)
        *pragmaFlag = TRUE;

    /* trim leading white space */
    if(my_strnicmp(buff, "content-type:", 13) == 0)
        setenv("CONTENT_TYPE", trim(buff + 13), 1);
    if(my_strnicmp(buff, "Host:", 5) == 0)
        setenv("HTTP_HOST", trim(buff + 5), 1);
    if(my_strnicmp(buff, "User-Agent:", 11) == 0)
        setenv("HTTP_USER_AGENT", trim(buff + 11), 1);
    if(my_strnicmp(buff, "Cookie:", 7) == 0)
        setenv("HTTP_COOKIE", trim(buff + 7), 1);
    if(my_strnicmp(buff, "Authorization:", 14) == 0)
        setenv("HTTP_AUTHORIZATION", trim(buff + 14), 1);
    }


return(size);
}


ssize_t readPayLoad(ssize_t size, char * buff, int outFile, char * request)
{
ssize_t bytes, readsize;
size_t offset = 0, transferred = 0;

#ifdef DEBUGHTTP
printf("# Payload size:%ld\r\n", (long)size);
#endif

while(size > 0)
    {
    readsize = (size > MAX_BUFF) ? MAX_BUFF : size;
#ifndef WINDOWS
    bytes = read(fileno(IOchannel), buff + offset, readsize); 
#else /* it is WINDOWS */
    if(IOchannel != NULL && IOchannelIsSocketStream)
        bytes = recv(getSocket(IOchannel), buff + offset, readsize, NO_FLAGS_SET);
    else
        bytes = read(fileno(IOchannel), buff + offset, readsize);
#endif

#ifdef DEBUGHTTP
    printf("Payload bytes:%ld:%s:\r\n", (long)bytes, buff + offset);
#endif

    if(bytes <= 0)
        {
        sendHTTPmessage(500, "Problem reading data", request);
        return(-1);
        }

    if(outFile)
        {
        if(write(outFile, buff + offset, bytes) != bytes)
            {
            sendHTTPmessage(500, "Cannot create file", request);
            return(-1);
            }
        }
    else
        offset += bytes;

    transferred += bytes;
    size -= bytes;
    }   
#ifndef WINDOWS
fflush(NULL); 
#endif
return(transferred);
}



void handleHTTPcgi(char * request, char * query, ssize_t querySize)
{
FILE * handle;
char * command;
char * content = NULL;
ssize_t size;
char tempfile[PATH_MAX];
#ifdef WINDOWS_BEFORE_SETTING_BINARYMODE
char * ptr;
char * pos;
int bytes = 0;
#endif

srandom(milliSecTime());

#ifdef DEBUGHTTP
printf("# CGI request:%s:%s:\r\n", request, query);
#endif

if(isFile(request, 0) != 0)
    {
    sendHTTPmessage(404, ERROR_404, request);
    return;
    }

if(isFile(tempDir, 0) != 0)
    {
    sendHTTPmessage(500, "cannot find tmp directory", request);
    return;
    }

size = strlen(request) + PATH_MAX;
command = alloca(size);
snprintf(tempfile, PATH_MAX, "%s/nl%04x-%08x-%08x", 
    tempDir, (unsigned int)size, (unsigned int)random(), (unsigned int)random());

#if defined (WINDOWS) || (OS2)
snprintf(command, size - 1, "newlisp \"%s\" > %s", request, tempfile);
#else
snprintf(command, size - 1, "./\"%s\" > %s", request, tempfile);
#endif

if((handle = popen(command, "w")) == NULL)
    {
    sendHTTPmessage(500, "failed creating pipe", request);
    return;
    }

if((size = fwrite(query, 1, querySize, handle)) < 0)
    fatalError(ERR_IO_ERROR, 0, 0);

fflush(handle);
pclose(handle);

size = readFile(tempfile, &content);
if(size == -1)  
    sendHTTPmessage(500, "cannot read output of", tempfile);
else
    sendHTTPpage(content, size, NULL);
    
#ifdef DEBUGHTTP
printf("# Temporary file: %s\n", tempfile);
#else
unlink(tempfile);
#endif

if(content) free(content);
}   


int endsWith(char * str, char * ext)
{
size_t size, len;

size = strlen(str);
len =  strlen(ext);

return(strncmp(str + size - len, ext, len) == 0);
}


typedef struct
    {
    char * extension;
    char * type;
    } T_MEDIA_TYPE;

/* T_ prefix added in 10.4.8 to compile on later MinGW */

T_MEDIA_TYPE mediaType[] = {
    {".avi", "video/x-msvideo"},
    {".css", "text/css"},
    {".gif", "image/gif"},
    {".htm", "text/html"},
    {".html","text/html"},
    {".jpg", "image/jpeg"},
    {".js", "application/javascript"},
    {".mov", "video/quicktime"},
    {".mp3", "audio/mpeg"},
    {".mpg", "video/mpeg"},
    {".pdf", "application/pdf"},
    {".png", "image/png"},
    {".wav", "audio/x-wav"},
    {".zip", "application/zip"},
    { NULL, NULL},
};

char * getMediaType(char * request)
{
int i;

for(i = 0; mediaType[i].extension != NULL; i++)
    {
    if(endsWith(request, mediaType[i].extension))
        return(mediaType[i].type);
    }
    
return(MEDIA_TEXT);
}

 
void url_decode(char *dest, char *src)
{
char code[3] = {0};
unsigned int ascii = 0;
char *end = NULL;

while(*src)
    {
    if(*src == '%')
        {
        memcpy(code, ++src, 2);
        ascii = strtoul(code, &end, 16);
        *dest++ = (char)ascii;
        src += 2;
        }
    else
        *dest++ = *src++;
    }
*dest = 0;
}

#endif /* ifndef EMSCRIPTEN */
#endif /* ifndef LIBRARY */
/* eof */

