/*


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

#define AF_UNSPEC 0 /* from socket.h or winsock2.h */


#if defined(SOLARIS) || defined(TRU64) || defined(AIX) 
#include <stropts.h>
#endif

#ifdef SOLARIS
#define FIONREAD I_NREAD
#endif

#ifndef WINDOWS
#include <sys/types.h>
#ifndef ANDROID
#ifndef EMSCRIPTEN
#include <sys/ipc.h>
#include <sys/sem.h>
#endif
#endif
#include <sys/mman.h>
#include <sys/ioctl.h>
#endif

int init_argv(char * ptr, char *argv[]);
char * getUUID(char * str, char * node);

#ifdef OS2
#include <conio.h>
int semctl(int semid, int semnum, int cmd, ...);
#endif

#if defined(LINUX) || defined(KFREEBSD)
union semun {
  int val;    /* Value for SETVAL */
  struct semid_ds *buf;    /* Buffer for IPC_STAT, IPC_SET */
  unsigned short *array;  /* Array for GETALL, SETALL */
#ifdef LINUX
  struct seminfo *__buf;  /* Buffer for IPC_INFO (Linux-specific) */
#endif
};
#endif /* LINUX || KFREEBSD */

#ifndef TRU64
extern char ** environ;
#endif

#ifdef WINDOWS
#define fgetc win_fgetc
#define realpath win_realpath
#include <conio.h>  
#include <io.h>
#include <direct.h>
#define pclose _pclose
#define pipe _pipe

/* 
Set binary as default file mode for Windows.
See also http://www.mingw.org/MinGWiki/index.php/binary
*/
unsigned int _CRT_fmode = _O_BINARY;

int setenv (const char *name, const char *value, int replace);
#endif /* Win32 */

#ifndef WINDOWS
#include <sys/socket.h>
#define SOCKET_ERROR -1
#define INVALID_SOCKET -1
#endif

#if defined(LINUX) || defined(KFREEBSD) || defined(CYGWIN)
char * strptime(const char * str, const char * fmt, struct tm * ttm);
#endif

time_t calcDateValue(int year, int month, int day, int hour, int min, int sec);
ssize_t currentDateValue(void);
extern STREAM readLineStream;
extern FILE * IOchannel;
extern int pagesize;

extern char * errorMessage[];
extern STREAM errorStream;
extern UINT netErrorIdx;
extern int newlispLibConsoleFlag;

/* semaphore() function type */
#ifndef NO_SEMAPHORE
#define SEM_CREATE 0
#define SEM_STATUS 1
#define SEM_SIGNAL 2
#endif

/* used in fork and spawn */
int parentPid = 0;
/* share, message */
CELL * readWriteShared(UINT * address, CELL * params, int flag);
CELL * readWriteSocket(int socket, CELL * params);
CELL * readWriteSharedExpression(UINT * adress, CELL * params);

void checkDeleteShareFile(UINT * address);

CELL * p_isFile(CELL * params) /* includes dev,socket,dir,file etc. */
{
char * fileName;
int flag;

params = getString(params, &fileName);
flag = getFlag(params);

return(isFile(fileName, flag) ? nilCell : flag ? stuffString(fileName) : trueCell);
}

int isFile(char * fileName, int flag)
{
struct stat fileInfo;
int result;

#ifdef WINDOWS
char slash;
size_t len;

len = strlen(fileName);
slash = *(fileName + len - 1);
if((slash == '\\' || slash == '/') && (!(len >= 2 && *(fileName + len - 2) == ':')))
    *(fileName + len - 1) = 0;

#ifdef USE_WIN_UTF16PATH
result = stat_utf16(fileName, &fileInfo);
#else
result = stat(fileName, &fileInfo);
#endif
if(slash == '\\' || slash == '/')
    *(fileName + len - 1) = slash;
#else /* not WINDOWS */
result = stat(fileName, &fileInfo);
#endif
if(result == 0)
    {
    if(flag)
        result = ! S_ISREG(fileInfo.st_mode);
    }

return(result);
}

CELL * p_isDirectory(CELL * params)
{
char * fileName;

getString(params, &fileName);
return(isDir(fileName) ? trueCell : nilCell);
}

int isDir(char * fileName)
{
struct stat fileInfo;

#ifdef WINDOWS
char slash;
size_t len;

len = strlen(fileName);
slash = *(fileName + len - 1);
if((slash == '\\' || slash == '/') && (!(len >= 2 && *(fileName + len - 2) == ':')))
    *(fileName + len - 1) = 0;
#endif

#ifdef USE_WIN_UTF16PATH
if(stat_utf16(fileName, &fileInfo) != 0)
#else
if(stat(fileName, &fileInfo) != 0)
#endif
    {
#ifdef WINDOWS
    *(fileName + len - 1) = slash;
#endif
    return(0);
    }

#ifdef WINDOWS
*(fileName + len - 1) = slash;
#endif

if(S_ISDIR(fileInfo.st_mode))
    return(1);
return(0);
}


CELL * p_open(CELL * params)
{
char * fileName;
char * accessMode;
char * option = NULL;
int handle;
IO_SESSION * session;

params = getString(params, &fileName);
params = getString(params, &accessMode);

if(params != nilCell)
    getString(params, &option);
    
if( (handle = openFile(fileName, accessMode, option)) == (int)-1)
    return(nilCell);

session = createIOsession(handle, AF_UNSPEC);
if(*accessMode == 'r')
    session->stream = fdopen(handle, "r");
else if(*accessMode == 'w')
    session->stream = fdopen(handle, "w");
else if(*accessMode == 'u')
    session->stream = fdopen(handle, "r+");
else if(*accessMode == 'a')
    session->stream = fdopen(handle, "a+");

return(stuffInteger((UINT)handle));
}

CELL * p_close(CELL * params)
{
UINT handle;

getInteger(params, &handle);
if(handle == 0) return(nilCell);
if(handle == printDevice) printDevice = 0;
if(deleteIOsession(handle)) return(trueCell);
return(nilCell);
}


CELL * p_readChar(CELL * params)
{
UINT handle;
unsigned char chr;

if(params != nilCell)
    getInteger(params, &handle);
else 
    handle = printDevice;

#ifdef WINDOWS
/* make it work as on Unix */
if(printDevice == 1 || printDevice == 2) handle = 0;
#endif

if(read((int)handle, &chr, 1) <= 0) return(nilCell);

return(stuffInteger((UINT)chr));
}


CELL * p_readBuffer(CELL * params)
{
UINT handle;
size_t size, length;
ssize_t bytesRead = 0;
char * waitFor;
STREAM stream = {NULL, NULL, 0, 0, 0};
CELL * strCell;
SYMBOL * readSptr;
int found = 0;
char chr;

params = getInteger(params, &handle);
params = getEvalDefault(params, &strCell);
if(!symbolCheck || symbolCheck->contents != (UINT)strCell)
    return(errorProc(ERR_IS_NOT_REFERENCED));
if(isProtected(symbolCheck->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));

readSptr = symbolCheck;
params = getInteger(params, (UINT *)&size);

if(params == nilCell)
    {
    openStrStream(&stream, size, 0);
    found = 1;
    if((bytesRead = read(handle, stream.buffer, size)) == -1)
        {
        closeStrStream(&stream); 
        return(nilCell);
        }
    }
else
    {
    getString(params, &waitFor);
    openStrStream(&stream, MAX_LINE, 0);
    length = strlen(waitFor);
    while(bytesRead < size)
        {
        if(read(handle, &chr, 1) <= 0)
            break;

        writeStreamChar(&stream, chr); 
        if(++bytesRead < length) continue;
        if(strcmp(waitFor,  stream.ptr - length) == 0)
            {
            found = 1;
            break;
            }       
        }           
    }

deleteList(strCell);

if(bytesRead == 0) 
    { 
    readSptr->contents = (UINT)copyCell(nilCell);
    closeStrStream(&stream); 
    return(nilCell);
    } 

/*
#ifndef WINDOWS
if((fstream = getIOstream(handle)) != NULL)
    {
    newPosition = lseek(handle, 0, SEEK_CUR);
    fseek(fstream, newPosition, 0);
    }
#endif
*/

if(stream.size > bytesRead)
    stream.buffer = reallocMemory(stream.buffer, bytesRead + 1);
readSptr->contents = (UINT)makeStringCell(stream.buffer, bytesRead);

if(found) return(stuffInteger(bytesRead));
return(nilCell);
}


CELL * p_readFile(CELL * params)
{
char * fileName;
char * buffer = NULL;
ssize_t size;
#ifndef EMSCRIPTEN
CELL * result;
#endif

params = getString(params, &fileName);
#ifndef EMSCRIPTEN
if(my_strnicmp(fileName, "http://", 7) == 0)
    {
    result = getPutPostDeleteUrl(fileName, params, HTTP_GET, CONNECT_TIMEOUT);
    return((my_strnicmp((char *)result->contents, (char *)"ERR:", 4) == 0) && netErrorIdx ? nilCell : result);
    }
#endif
if((size = readFile(fileName, &buffer)) == -1)
    return(nilCell);

return(makeStringCell(buffer, size));
}

/* allocates a buffer and reads a file into it */
ssize_t readFile(char * fileName, char * * buffer)
{
int handle; 
off_t size;
struct stat fileInfo;

fileName = getLocalPath(fileName);

#ifdef USE_WIN_UTF16PATH
if(stat_utf16(fileName, &fileInfo) != 0)
#else
if(stat(fileName, &fileInfo) != 0)
#endif
    return(-1);

size = fileInfo.st_size;

if( (handle = openFile(fileName, "r", NULL)) == (int)-1)
    return(-1);

*buffer = callocMemory(size+1);

if(read(handle, *buffer, size) == -1)
    {
    freeMemory(*buffer);
        close(handle);
    *buffer = NULL;
    return(-1);
    }

close(handle);

return(size);
}



CELL * p_writeChar(CELL * params)
{
UINT handle;
UINT data;
size_t count;
unsigned char chr;

params = getInteger(params, &handle);
count = 0;

while(params != nilCell)
    {
    params = getInteger(params, &data);
    chr = (unsigned char)data;
    if(write((int)handle, (void *)&chr, 1) == -1)
        return(nilCell);
    ++count;
    }
    
return(stuffInteger(count));
}


size_t appendCellString(CELL * cell, char * buffer, size_t size)
{
cell->contents = (UINT)reallocMemory((char *)cell->contents, cell->aux + size);
memcpy((char *)cell->contents + cell->aux - 1, buffer, size);
cell->aux += size;

*((char *)cell->contents + cell->aux - 1) = 0; 

return(size);
}


CELL * p_appendFile(CELL * params)
{
return(appendWriteFile(params, "a"));
}

CELL * p_writeFile(CELL * params)
{
return(appendWriteFile(params, "w"));
}

int writeFile(char * fileName, char * buffer, size_t size, char * type)
{
int handle;

if( (handle = openFile(fileName, type, NULL)) == (int)-1)
    return(-1);

if(write(handle, buffer, size) == (int)-1)
    return(-1);

close(handle);
return(0);
}

CELL * appendWriteFile(CELL * params, char * type)
{
char * fileName;
char * buffer;
size_t size;
#ifndef EMSCRIPTEN
CELL * result;
#endif

params = getString(params, &fileName);

#ifndef EMSCRIPTEN
if(my_strnicmp(fileName, "http://", 7) == 0)
    {
    result = getPutPostDeleteUrl(fileName, params, 
                (*type == 'w') ? HTTP_PUT : HTTP_PUT_APPEND, CONNECT_TIMEOUT);
    return((my_strnicmp((char *)result->contents, (char *)"ERR:", 4) == 0) && netErrorIdx ? nilCell : result);
    }
#endif

getStringSize(params, &buffer, &size, TRUE);

if(writeFile(fileName, buffer, size, type) == (int)-1)
    return(nilCell);

return(stuffInteger(size));
}

CELL * writeBuffer(CELL * params, int lineFeed);

CELL * p_writeBuffer(CELL * params)
{
return(writeBuffer(params, FALSE));
}

CELL * p_writeLine(CELL * params)
{
return(writeBuffer(params, TRUE));
}


CELL * writeBuffer(CELL * params, int lineFeed)
{
CELL * device;
UINT handle;
SYMBOL * symbolRef;
char * buffer;
size_t size, userSize;

if(params == nilCell)
    {
    varPrintf(OUT_DEVICE, "%s", readLineStream.buffer);
    if(lineFeed) varPrintf(OUT_DEVICE, LINE_FEED);
    size = readLineStream.ptr - readLineStream.buffer;
    goto RETURN_WRITE_BUFFER;
    }

params = getEvalDefault(params, &device);
symbolRef = symbolCheck;

if(params == nilCell)
    {
    buffer = readLineStream.buffer;
    size = readLineStream.ptr - readLineStream.buffer;
    }
else
    params = getStringSize(params, &buffer, &size, TRUE);

if(!lineFeed)
    {
    if(params != nilCell)
        {
        getInteger(params, (UINT *)&userSize);
        size = (userSize > size) ? size : userSize;
        }
    }

if(isNumber(device->type))
    {
    getIntegerExt(device, &handle, FALSE);
    if(write((int)handle, buffer, size) == -1) return(nilCell);
    if(lineFeed)
        if(write((int)handle, LINE_FEED, LINE_FEED_LEN) == -1) return(nilCell);
    }

else if(device->type == CELL_STRING)
    {
    if(symbolRef && isProtected(symbolRef->flags))
        return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolRef)));

    appendCellString(device, buffer, size);
    if(lineFeed)
        appendCellString(device, LINE_FEED, LINE_FEED_LEN);
    }
else
    return(errorProcExt(ERR_INVALID_PARAMETER, device));


RETURN_WRITE_BUFFER:
return(stuffInteger(size + (lineFeed ? LINE_FEED_LEN : 0)));
}


CELL * p_seek(CELL * params)
{
UINT handle;
FILE * fstream;
#ifdef LFS
INT64 paramPosition;
off_t newPosition;
#else
off_t paramPosition;
off_t newPosition;
#endif

params = getInteger(params, &handle);

if(params == nilCell)
    {
    if(handle == 0)
        newPosition = ftell(stdout);
    else if((fstream = getIOstream(handle)) != NULL)
        newPosition = ftell(fstream);
    else if( (newPosition = lseek(handle, 0, SEEK_CUR)) == -1)
        return(nilCell);
    }
else
    {
#ifdef LFS
    getInteger64Ext(params, &paramPosition, TRUE);
#else
    getInteger(params, (UINT *)&paramPosition);
#endif

    newPosition = paramPosition;

    if(newPosition == -1)
        {
        if( (newPosition = lseek((int)handle, 0, SEEK_END)) == -1)
            return(nilCell);
        }
    else
        {
        if( lseek((int)handle, newPosition, SEEK_SET) == -1)
            return(nilCell);
        }
    }

paramPosition = newPosition;
#ifdef LFS
return(stuffInteger64(paramPosition));
#else
return(stuffInteger(paramPosition));
#endif
}

char * readStreamLine(STREAM * stream, FILE * inStream)
{
#ifdef OLD_READ_STREAM /* pre 10.5.8 */
int chr;
#else
char buff[MAX_STRING];
size_t l;
#endif

openStrStream(stream, MAX_STRING, 1);

#ifdef TRU64
do {
errno = 0;
#endif
#ifdef TRUE64 /* pre 10.5.8 also all other OS */
while((chr = fgetc(inStream)) != EOF)
    {
    if(chr == '\n') break;
    if(chr == '\r')
        {
        chr = fgetc(inStream);
        if(chr == '\n' || chr == EOF) break;
        }
    writeStreamChar(stream, chr);
    }
#else
while(fgets(buff, MAX_STRING, inStream) != NULL)
    {
    l=strlen(buff);
    if(buff[l-1] == 0x0A)
        {
        buff[--l] = 0;
        if(buff[l-1] == 0x0D)
            buff[--l] = 0;
        writeStreamStr(stream, buff, l);
        break;
        }
    writeStreamStr(stream, buff, l);
    }
#endif /* pre 10.5.8 also all other OS */
#ifdef TRU64
} while (errno == EINTR);
#endif

#ifdef TRU64 /* and pre 10.5.8 on all other OS */
if(chr == EOF && stream->position == 0) return(NULL);
#else
if(feof(inStream)) 
    {
    clearerr(inStream);
    if(stream->position == 0) return(NULL);
    }
#endif
return(stream->buffer);
}


CELL * p_readLine(CELL * params)
{
UINT handle;
unsigned char chr;
char * line;
int bytesRead;
FILE * fstream;

if(params != nilCell)
    getInteger(params, &handle);
else 
    handle = printDevice;

#ifdef WINDOWS
/* make it work as on Unix */
if(printDevice == 1 || printDevice == 2) handle = 0;
#endif

/* check if stream input can be done */
fstream = (handle == 0) ? IOchannel : getIOstream(handle);
#ifdef LIBRARY
if(!newlispLibConsoleFlag && fstream == stdin)
    return(nilCell);
#endif
if(fstream != NULL)
    {
    if((line = readStreamLine(&readLineStream, fstream)) == NULL)
        return(nilCell);
    return(stuffString(line));
    }

/* do raw handle input, only happens when using read-line on 
   sockets on UNIX and pipes on Windows  */
openStrStream(&readLineStream, MAX_STRING, 1);
while(TRUE)
    {
    if((bytesRead = read((int)handle, &chr, 1)) <= 0) break;
    if(chr == '\n') break;
    if(chr == '\r')
        {
        if(read((int)handle, &chr, 1) < 0) break;
        if(chr == '\n') break;
        }
    writeStreamChar(&readLineStream, chr);
    }

if(bytesRead <= 0 && readLineStream.position == 0)
    return(nilCell);

return(stuffStringN(readLineStream.buffer, readLineStream.position));;
}


CELL * p_currentLine(CELL * params)
{
return(stuffString(readLineStream.buffer));
}


char * getLocalPath(char * fileName)
{
if(my_strnicmp(fileName, "file://", 7) == 0)
    fileName = fileName + 7;

#ifdef WINDOWS
if(*fileName == '/' && *(fileName + 2) == ':')
    fileName = fileName + 1;
#endif

return(fileName);
}


int openFile(char * fileName, char * accessMode, char * option)
{
int blocking = 0;
#ifndef WINDOWS
int handle;
#endif

fileName = getLocalPath(fileName);

#ifndef WINDOWS
if(option != NULL && *option == 'n')
    blocking = O_NONBLOCK;
#endif


if(*accessMode == 'r')
#ifdef USE_WIN_UTF16PATH
    return(open_utf16(fileName, O_RDONLY | O_BINARY | blocking, 0));
#else
    return(open(fileName, O_RDONLY | O_BINARY | blocking, 0));
#endif

else if(*accessMode == 'w')
#ifdef WINDOWS
#ifdef USE_WIN_UTF16PATH
    return(open_utf16(fileName, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, S_IREAD | S_IWRITE));
#else
    return(open (fileName, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, S_IREAD | S_IWRITE));
#endif /* UTF16 */
#else
    return(open(fileName,O_WRONLY | O_CREAT | O_TRUNC | O_BINARY | blocking, 
        S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR | S_IWGRP | S_IWOTH)); /* rw-rw-rw */
#endif

else if(*accessMode == 'u')
    return(open(fileName, O_RDWR | O_BINARY, 0));

else if(*accessMode == 'a')
   {
#ifdef WINDOWS
#ifdef USE_WIN_UTF16PATH
   return(open_utf16(fileName, O_RDWR | O_APPEND | O_BINARY | O_CREAT, S_IREAD | S_IWRITE));
#else
   return(open(fileName, O_RDWR | O_APPEND | O_BINARY | O_CREAT, S_IREAD | S_IWRITE));
#endif /* UTF 16 */
#else
   handle = open(fileName, O_RDWR | O_APPEND | O_BINARY | O_CREAT,
          S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR | S_IWGRP | S_IWOTH); /* rw-rw-rw */
#ifdef EMSCRIPTEN
       /* oppen append is broken on Emscripten, open for update but filepointer
          stays at the beginning and old contents is overwritten */
   if(lseek(handle, 0, SEEK_END) != -1)
      return(handle);
#else
    return(handle);
#endif

#endif
       }

return(-1);
}

/* ------------------------- directory management ------------------------- */

CELL * p_copyFile(CELL * params)
{
char * fromName;
char * toName;
int fromHandle, toHandle;
unsigned char * copyBuffer;
UINT bytesRead;

params = getString(params, &fromName);
getString(params, &toName);

if((fromHandle = openFile(fromName, "read", NULL)) < 0)
    return(nilCell);

if((toHandle = openFile(toName,"write", NULL)) < 0)
    return(nilCell);

copyBuffer = allocMemory(MAX_FILE_BUFFER);
do
    {
    bytesRead = read(fromHandle, copyBuffer, MAX_FILE_BUFFER);
    if(write(toHandle, copyBuffer, (int)bytesRead) < 0) 
        fatalError(ERR_IO_ERROR, 0, 0);
    } while (bytesRead == MAX_FILE_BUFFER);

free(copyBuffer);

close(fromHandle);
close(toHandle);

return(trueCell);
}


CELL * p_renameFile(CELL * params)
{
char *oldName;
char *newName;

params = getString(params, &oldName);
getString(params, &newName);

#ifdef USE_WIN_UTF16PATH
return(rename_utf16(oldName, newName) == 0 ? trueCell : nilCell);
#else
return(rename(oldName, newName) == 0 ? trueCell : nilCell);
#endif
}


CELL * p_deleteFile(CELL * params)
{
char * fileName;
#ifndef EMSCRIPTEN
CELL * result;
#endif

params = getString(params, &fileName);
#ifndef EMSCRIPTEN
if(my_strnicmp(fileName, "http://", 7) == 0)
    {
    result = getPutPostDeleteUrl(fileName, params, HTTP_DELETE, CONNECT_TIMEOUT);
    return((my_strnicmp((char *)result->contents, (char *)"ERR:", 4) == 0) && netErrorIdx ? nilCell : result);
    }
#endif

fileName = getLocalPath(fileName);
#ifdef USE_WIN_UTF16PATH
return(unlink_utf16(fileName) == 0 ? trueCell : nilCell);
#else
return(unlink(fileName) == 0 ? trueCell : nilCell);
#endif
}


CELL * p_makeDir(CELL * params)
{
char * dirString;
UINT mode = 0777; /* drwxrwxrwx  gets user masked to drwxr-xr-x on most UNIX */

/* consume param regardless of OS */
params = getString(params, &dirString);

if(params != nilCell)
    {
    getInteger(params, &mode);
    mode = mode > 0xfff ? 0xfff : mode;
    }

#ifdef WINDOWS
#ifdef USE_WIN_UTF16PATH
return(mkdir_utf16(dirString) == 0 ? trueCell : nilCell);
#else
return(mkdir(dirString) == 0 ? trueCell : nilCell);
#endif /* UTF16 */
#else
return(mkdir(dirString, (mode_t)mode) == 0 ? trueCell : nilCell);
#endif
}


CELL * p_removeDir(CELL * params)
{
char * dirString;

getString(params, &dirString);
#ifdef USE_WIN_UTF16PATH
return(rmdir_utf16(dirString) == 0 ? trueCell : nilCell);
#else
return(rmdir(dirString) == 0 ? trueCell : nilCell);
#endif
}


CELL * p_changeDir(CELL * params)
{
char * newDir;

getString(params, &newDir);
#ifdef USE_WIN_UTF16PATH
return(chdir_utf16(newDir) == 0 ? trueCell : nilCell);
#else
return(chdir(newDir) == 0 ? trueCell : nilCell);
#endif
}

CELL * p_directory(CELL * params)
{
CELL * dirList;
char * dirPath;
char * fileName;
char * pattern = NULL;
INT options = 0;
DIR * dir;
struct dirent * dEnt;

if(params != nilCell)
    {
    params = getString(params, &dirPath);
    if(params != nilCell)
        {
        params = getString(params, &pattern);
        if(params != nilCell)
            /* 10.6.1 also accept string for options */
            parseRegexOptions(params, (UINT *)&options, TRUE);
        }
    }
else dirPath = ".";

dirList = getCell(CELL_EXPRESSION);

dir = opendir(dirPath);
if(dir == NULL) return(nilCell);

while((dEnt = readdir(dir)) != NULL)
    {
#ifdef USE_WIN_UTF16PATH
    fileName = utf16_to_utf8(dEnt->d_name);
#else
    fileName = dEnt->d_name;
#endif
    if(!pattern || searchBufferRegex(fileName, 0, pattern, strlen(fileName), options, NULL) != -1)
        addList(dirList, stuffString(fileName));
#ifdef USE_WIN_UTF16PATH
    free(fileName);
#endif
    }

closedir(dir);
return(dirList);
}


#define DOT_PATH ".\000"


CELL * p_realpath(CELL * params)
{
char  path[PATH_MAX];
char * name;

if(params != nilCell)
    {
    params = getString(params, &name);
    if(getFlag(params))
        {
        if((name = which(name, alloca(PATH_MAX))) == NULL)
            return(nilCell);
        return(stuffString(name));
        }
    }
else name = DOT_PATH;

if(realpath(name, path) == NULL)
    return(nilCell);

#ifdef _BSD /* behaves like Windows */
if(isFile(path, 0)) return(nilCell);
#endif

return(stuffString(path));
}

CELL * p_fileInfo(CELL * params)
{
char * pathName;
struct stat fileInfo;
CELL * list;
int result = 0;

params = getString(params, &pathName);

#ifdef WINDOWS /* has no link-flag */
#ifdef USE_WIN_UTF16PATH
result = stat_utf16(pathName, &fileInfo);
#else
result = stat(pathName, &fileInfo);
#endif

#else /* Unix */
if(getFlag(params->next))
    result = stat(pathName, &fileInfo);
else
    result = lstat(pathName, &fileInfo);
#endif

if(result != 0)
    return(nilCell);

list = stuffIntegerList(
    8,
    (UINT)fileInfo.st_size,
    (UINT)fileInfo.st_mode,
    (UINT)fileInfo.st_rdev,
    (UINT)fileInfo.st_uid,
    (UINT)fileInfo.st_gid,
    (UINT)fileInfo.st_atime,
    (UINT)fileInfo.st_mtime,
    (UINT)fileInfo.st_ctime
    );

#ifndef NEWLISP64
#ifdef LFS
((CELL *)list->contents)->type = CELL_INT64;
*(INT64 *)&((CELL *)list->contents)->aux = (INT64)fileInfo.st_size;
#endif /* LFS */
#endif /* NEWLISP64 */

if(params != nilCell)
    {
    pushResult(list);
    return(copyCell(implicitIndexList(list, params)));
    }
    
return(list);
}


#ifdef LFS
INT64 fileSize(char * pathName)
#else
size_t fileSize(char * pathName)
#endif
{
struct stat fileInfo;
int result;

#ifdef WINDOWS /* has no link-flag */
#ifdef USE_WIN_UTF16PATH
result = stat_utf16(pathName, &fileInfo);
#else
result = stat(pathName, &fileInfo);
#endif
#else /* Unix */
result = stat(pathName, &fileInfo);
#endif

if(result != 0) return 0;

return(fileInfo.st_size);
}


/* ------------------------- processes and pipes ------------------------- */

#ifndef WINDOWS
CELL * p_system(CELL *params)
{
char * command;
getString(params, &command);
return(stuffInteger((UINT)system(command)));
}
#else
CELL * p_system(CELL *params)
{
UINT creation_flags = 0;
char * command;
STARTUPINFO si;
PROCESS_INFORMATION pi;
UINT result;

memset(&si, 0, sizeof(STARTUPINFO));
memset(&pi, 0, sizeof(PROCESS_INFORMATION));

si.cb = sizeof(STARTUPINFO);

params = getString(params, &command);
if(params != nilCell)
    getInteger(params, &creation_flags);
else
    return(stuffInteger((UINT)system(command)));

result = CreateProcessA(NULL, command, NULL, NULL, 0, (DWORD)creation_flags, NULL, NULL, 
        (LPSTARTUPINFO)&si, (LPPROCESS_INFORMATION)&pi); 


if(!result) return(nilCell);

WaitForSingleObject(pi.hProcess, -1);
CloseHandle(pi.hProcess);
CloseHandle(pi.hThread); 

return(stuffInteger(result));
}
#endif


CELL * p_exec(CELL * params)
{
CELL * lineList;
char * line;
char * command, * data;
FILE * handle;
size_t size;

params = getString(params, &command);
if(params == nilCell)
    {
    if((handle = popen(command , "r")) == NULL)
        return(nilCell);

    lineList = getCell(CELL_EXPRESSION);
    while((line = readStreamLine(&readLineStream, handle)) != NULL)
        addList(lineList, stuffString(line));

    pclose(handle);
    return(lineList);
    }
    
getStringSize(params, &data, &size, TRUE);

if((handle = popen(command, "w")) == NULL)
    return(nilCell);

if(fwrite(data, 1, (size_t)size, handle) < size)
    return(nilCell);

pclose(handle);
return(trueCell);
}


/* parses/splits a string intor substrings separated
   by spaces, strings containing spaces can be enclosed
   in either a pair of single or double quotes
*/
int init_argv(char * ptr, char *argv[])
{
int argc = 0;
char brkChr;

while(*ptr != 0)
    {
    while(*ptr == ' ') ++ptr;
    if(*ptr == 0) break;
    if(*ptr == '\'' || *ptr == '"')
        {
        brkChr = *ptr;
        argv[argc++] = ++ptr;
        while(*ptr != brkChr && *ptr != 0) ++ptr;
        if(*ptr == 0) break;
        *ptr++ = 0; 
        continue;
        }
    else
        {
        argv[argc++] = ptr++;
        while(*ptr != ' ' && *ptr != 0) ptr++;
        if(*ptr == 0) break;
        *ptr++ = 0;
        }
    }

argv[argc] = 0;
return(argc);
}


#ifndef EMSCRIPTEN
#ifdef WINDOWS
int kill(pid_t pid, int sig);
int winPipe(UINT * inpipe, UINT * outpipe);
UINT winPipedProcess(char * command, int inpipe, int outpipe, int option);
UINT plainProcess(char * command, size_t size);

CELL * p_pipe(CELL * params)
{
UINT hin, hout;
IO_SESSION * session;

if(!winPipe(&hin, &hout))    /* see file win-util.c */
    return(nilCell);

session = createIOsession(hin, AF_UNSPEC);
session->stream = fdopen(hin, "r");
session = createIOsession(hout, AF_UNSPEC);
session->stream = fdopen(hout, "w");

return(stuffIntegerList(2, hin, hout));
}


CELL * p_process(CELL * params)
{
char * command;
int result;
size_t size;

UINT inpipe = 0, outpipe = 0, option = 1;

params = getStringSize(params, &command, &size, TRUE);
if(params != nilCell)
    {
    params = getInteger(params, (UINT *)&inpipe);
    params = getInteger(params, (UINT *)&outpipe);
    if(params != nilCell)
        getInteger(params, (UINT *)&option);
	result = winPipedProcess(command, (int)inpipe, (int)outpipe, (int)option);
    }
else result = plainProcess(command, size);

if(!result) return(nilCell);

return(stuffInteger(result));
}


#else /* not WINDOWS */

CELL * p_pipe(CELL * params)
{
int handles[2];
#ifndef SUNOS
IO_SESSION * session;
#endif

if(pipe(handles) != 0)
    return(nilCell);

#ifndef SUNOS
session = createIOsession(handles[0], AF_UNSPEC);
session->stream = fdopen(handles[0], "r");
session = createIOsession(handles[1], AF_UNSPEC);
session->stream = fdopen(handles[0], "w");
#endif

return(stuffIntegerList(2, (UINT)handles[0], (UINT)handles[1]));
}


CELL * p_process(CELL * params)
{
char * command;
char * cmd;
int forkResult;
UINT inpipe = 0, outpipe = 0, errpipe = 0;
char * argv[16];
size_t  size;

params = getStringSize(params, &command, &size, TRUE);
cmd = callocMemory(size + 1);
memcpy(cmd, command, size + 1);

#ifdef DEBUG_INIT_ARGV
    int i;
    init_argv(cmd, argv);
    for(i = 0; i < 15; i++)
        {
        if(argv[i] == NULL) break;
        printf("->%s<-\n", argv[i]);
        }
    return(trueCell);
#endif

if(params != nilCell)
    {
    params = getInteger(params, (UINT *)&inpipe);
    params = getInteger(params, (UINT *)&outpipe);
    if(params != nilCell)
        getInteger(params, (UINT *)&errpipe);
    }

if((forkResult = fork()) == -1)
    return(nilCell);
if(forkResult == 0)
    {
    /* redirect stdin and stdout, stderr to pipe handles */
    if(inpipe)
        {
        close(STDIN_FILENO); 
        if(dup2((int)inpipe, STDIN_FILENO) == -1) exit(0);
        close((int)inpipe);
        }
    if(outpipe)
        {
        close(STDOUT_FILENO); 
        if(dup2((int)outpipe, STDOUT_FILENO) == -1) exit(0);
        if(!errpipe)
            if(dup2((int)outpipe, STDERR_FILENO) == -1) exit(0);
        close((int)outpipe);
        }
    if(errpipe)
        {
        close(STDERR_FILENO);
        if(dup2((int)errpipe, STDERR_FILENO) == -1) exit(0);
        close((int)errpipe);
        }
    
    init_argv(cmd, argv);

    execve(argv[0], argv, environ);
    exit(0);
    }

freeMemory(cmd);

return(stuffInteger(forkResult));
}

#ifndef NO_FORK
CELL * p_fork(CELL * params)
{
int forkResult;
int ppid = getpid();

if((forkResult = fork()) == -1)
    return(nilCell);
if(forkResult == 0)
    {
    parentPid = ppid;
    evaluateExpression(params);
    exit(0);
    }

return(stuffInteger(forkResult));
}
#endif

/* ------------------------------------------------------------------------- */


/* Cilk like interface for spawning and syncronizing child processes
   spawn - start child
   sync  - syncronize results
   abort - abort child

   message - share data with chold and parent
*/

/* run with or without semaphores */

void * parentPad = NULL;    /* written by parent for this process */
void * thisPad = NULL;      /* written by this process for the parent */
int thisSocket = 0;
fd_set myFdSet;             /* set of all child sockets */


#ifndef NO_SPAWN

typedef struct 
    {
    void * result_addr; /* written by child */
    SYMBOL * symbolPtr; /* smbol for result */
    int pid;            /* childs pid */
    int socket; 
    void * next;    
    } SPAWN_LIST;

SPAWN_LIST * mySpawnList = NULL;

void addSpawnedChild(void * addr, SYMBOL * sPtr, int pid, int socket)
{
SPAWN_LIST * spawnList;

spawnList = (SPAWN_LIST  *)allocMemory(sizeof(SPAWN_LIST));

spawnList->result_addr = addr;
spawnList->symbolPtr = sPtr;
spawnList->pid = pid;
spawnList->socket = socket;
spawnList->next = NULL;

if(mySpawnList == NULL)
    mySpawnList = spawnList;
else/* insert in front */
    {
    spawnList->next = mySpawnList;
    mySpawnList = spawnList;
    }
}


SPAWN_LIST * getSpawnedChild(int pid)
{
SPAWN_LIST * spawnList = mySpawnList;

while(spawnList != NULL)
    {
    if(spawnList->pid == pid) break;
    spawnList = spawnList->next;
    }

return(spawnList);
}

void purgeSpawnList(int sockFlag)
{
SPAWN_LIST * spawnList;

/* pop and delete entries */

while(mySpawnList != NULL)
    {
    if(sockFlag)
        close(mySpawnList->socket);
    spawnList = mySpawnList->next;
    free(mySpawnList);
    mySpawnList = spawnList;
    }
}

/* lookup pid get result from shared memory and delete entry */

#define PROCESS_SPAWN_RESULT 0
#define PROCESS_SPAWN_ABORT 1
#define PROCESS_SPAWN_ABNORMAL_END 2
#define ABEND "ERR: abnormal process end"

void processSpawnList(int pid, int mode, int result)
{
SPAWN_LIST * pidSpawn;
SPAWN_LIST * previousSpawn;
CELL * cell;
SYMBOL * sPtr;
char str[32];

pidSpawn = previousSpawn = mySpawnList;

while(pidSpawn)
    {
    if(pidSpawn->pid == pid)
        {
        if(pidSpawn == mySpawnList)
            mySpawnList = pidSpawn->next;
        else
            previousSpawn->next = pidSpawn->next;

        if(mode == PROCESS_SPAWN_RESULT)
            {
            cell = readWriteShared(pidSpawn->result_addr, nilCell, 0);
            sPtr = pidSpawn->symbolPtr;
            deleteList((CELL *)sPtr->contents);
            sPtr->contents = (UINT)cell;
            }
        else if(mode == PROCESS_SPAWN_ABORT)
            {
            FD_CLR(pidSpawn->socket, &myFdSet);
            kill(pidSpawn->pid, 9);
            waitpid(pidSpawn->pid, (int *)0, 0);
            }
        else /* PROCESS_SPAWN_ABNORMAL_END */
            {
            sPtr = pidSpawn->symbolPtr;
            deleteList((CELL *)sPtr->contents);
            snprintf(str, 32, "%s %d", ABEND, result);
            sPtr->contents = (UINT)stuffString(str);
            }
           
	/* close parent socket */
        if(pidSpawn->socket) close(pidSpawn->socket); 
        checkDeleteShareFile(pidSpawn->result_addr);
        /* unmap shared result memory */
        munmap(pidSpawn->result_addr, pagesize);
        free((char *)pidSpawn);
        break;
        }
    previousSpawn = pidSpawn;
    pidSpawn = pidSpawn->next;
    }
}

/* spawn (fork) a process and assign result to the symbol given
     (spawn <quoted-symbol> <epxression>) => pid
   creates a memory share and passes it to the spawned process
   when the spawned child finishes, it copies the result
   to the memory share. If the result does not fit in the pagesize
   store the result in a file with a unique filename which is
   stored in the memory share. The first int32 word is -1 for
   memshare store or 0 for file store.
   For house keeping purpose SPAWN_LIST is maintained to find
   the memshare adddress from the child pid.
*/
CELL * p_spawn(CELL * params)
{
int forkPid;
int pid;
void * address; /* share memory area for result */
SYMBOL * symPtr;
int sockets[2] = {0, 0};

if((address = mmap( 0, pagesize, 
    PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0)) == (void*)-1)
        return(nilCell);

memset(address, 0, sizeof(INT));

params = getSymbol(params, &symPtr);
if(isProtected(symPtr->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symPtr)));
deleteList((CELL *)symPtr->contents);
symPtr->contents = (UINT)nilCell;

pid = getpid();

/* socketpair for send/receive API is optional */
if(getFlag(params->next))
    {
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockets) == -1) 
        {
        munmap(address, pagesize);
        return(errorProc(ERR_CANNOT_OPEN_SOCKETPAIR)); 
        }

    /* add the parent socket to myFdSet */ 
    if(mySpawnList == NULL)
        FD_ZERO(&myFdSet);
    FD_SET(sockets[0], &myFdSet);
    }

/* make signals processable by waitpid() in p_sync() */
signal(SIGCHLD, SIG_DFL);

if((forkPid = fork()) == -1)
    {
    if(sockets[0]) close(sockets[0]);
    if(sockets[1]) close(sockets[1]);
    munmap(address, pagesize);
    return(nilCell);
    }

if(forkPid == 0) /* the child process */
    {
    /* seed random generator for message fail delay */
    srandom(getpid()); 
    /* get parent pid */
    parentPid = pid;
    if(sockets[0]) close(sockets[0]);
    thisSocket = sockets[1];
    /* purge inherited spawnlist */
    purgeSpawnList(FALSE);
    /* evaluate and write result to shared memory */
    readWriteShared(address, params, TRUE);
    /* close child socket */
    if(thisSocket) close(thisSocket);
    exit(0);
    }

if(sockets[1]) close(sockets[1]);
addSpawnedChild(address, symPtr, forkPid, sockets[0]);

return(stuffInteger(forkPid));
}

/* wait for spawned processes to finish for the timeout specified:
     (sync <timeout-milli-seconds>) => true
   if no timeout is not specified only return a list of pending
   child pids:
     (sync) => list of pids
   For each finished child get the result and assign it to the 
   symbol looked up in SPAWN_LIST.
*/
CELL * p_sync(CELL * params)
{
int result;
int pid;
UINT timeout = 0;
struct timeval tv, tp;
SPAWN_LIST * spawnList;
CELL * resultList = getCell(CELL_EXPRESSION);
int inletFlag = 0;
UINT * resultIdxSave;
CELL * cell;

if(mySpawnList == NULL)
    return(resultList); /* nothing pending */

if(params == nilCell)
    {
    spawnList = mySpawnList;
    while(spawnList != NULL)
        {
        addList(resultList, stuffInteger(spawnList->pid));
        spawnList = spawnList->next;
        }
    return(resultList);
    }

deleteList(resultList);

params = getInteger(params, &timeout);
if(params == nilCell || isNil((CELL *)((SYMBOL *)params->contents)->contents))
    signal(SIGCHLD, SIG_DFL);
else
    inletFlag = TRUE;
    
gettimeofday(&tv, NULL);

while(mySpawnList != NULL)
    {
    gettimeofday(&tp, NULL);
    if(timediff_ms(tp, tv) > timeout) return(nilCell);
    /* wait for any child process to finish */
    pid = waitpid(-1, &result, WNOHANG); 
    if(pid) 
        {
        if(!WIFEXITED(result))
            processSpawnList(pid, PROCESS_SPAWN_ABNORMAL_END, result);
        else
            processSpawnList(pid, PROCESS_SPAWN_RESULT, 0);
        if(inletFlag)
            {
            resultIdxSave = resultStackIdx;
            pushResult(cell = makeCell(CELL_EXPRESSION, (UINT)copyCell(params)));
            ((CELL *)cell->contents)->next = stuffInteger((UINT)pid);
            evaluateExpression(cell);
            cleanupResults(resultIdxSave);
            }
        }
    }

/* put initial behaviour back */
#if defined(SOLARIS) || defined(TRU64) || defined(AIX) 
setupSignalHandler(SIGCHLD, sigchld_handler);
#else
setupSignalHandler(SIGCHLD, signal_handler);
#endif

return(trueCell);
}

/* if abort a specific pid if specified:
     (abort <pid>)
   or abort all:
     (abort)
*/

CELL * p_abort(CELL * params)
{
UINT pid;
 
if(params != nilCell)
    {
    getInteger(params, &pid);
    processSpawnList(pid, PROCESS_SPAWN_ABORT, 0);
    }
else /* abort all */
    {
    while(mySpawnList != NULL) 
        processSpawnList(mySpawnList->pid, PROCESS_SPAWN_ABORT, 0);
    /* put initial behaviour back */
#if defined(SOLARIS) || defined(TRU64) || defined(AIX) 
   setupSignalHandler(SIGCHLD, sigchld_handler);
#else
    setupSignalHandler(SIGCHLD, signal_handler);
#endif
}

return(trueCell);
}

#define SELECT_READ_READY 0
#define SELECT_WRITE_READY 1

CELL * getSelectReadyList(int mode)
{
CELL * pidList = getCell(CELL_EXPRESSION);
SPAWN_LIST * child;
int ready = 0;
struct timeval tv;
fd_set thisFdSet;

tv.tv_sec = 0;
tv.tv_usec = 892 + random() / 10000000;

#if defined(SUNOS) || defined(LINUX) || defined(CYGWIN) || defined(AIX) || defined(KFREEBSD)
memcpy(&thisFdSet, &myFdSet, sizeof(fd_set));
#else
FD_COPY(&myFdSet, &thisFdSet);
#endif

if(mode == SELECT_READ_READY)
    ready = select(FD_SETSIZE, &thisFdSet, NULL, NULL, &tv);
else /* SELECT_WRITE_READY */
    ready = select(FD_SETSIZE, NULL, &thisFdSet, NULL, &tv);

if(ready == 0) return(pidList);

if(ready < 0) 
    return(pidList);

child = mySpawnList;
while (child != NULL)
    {
    if(FD_ISSET(child->socket, &thisFdSet))
         addList(pidList, stuffInteger(child->pid)); 
    child = child->next;
    }

return(pidList);
}
     

CELL * p_send(CELL * params)
{
UINT pid;
CELL * result = nilCell;
SPAWN_LIST * child = NULL;
int socket;

/* return list of writable child pids */
if(params == nilCell) 
    return(getSelectReadyList(SELECT_WRITE_READY));

params = getInteger(params, &pid);

if(pid == parentPid) /* write to parent */
    {
    socket = thisSocket;
    }
else  /* write to child */
    {
    if((child = getSpawnedChild(pid)) == NULL)
        errorProcExt2(ERR_INVALID_PID, stuffInteger(pid));
    socket = child->socket;
    }

if(!socket)
    errorProc(ERR_NO_SOCKET);

if(params == nilCell)
    errorProc(ERR_MISSING_ARGUMENT);

result = readWriteSocket(socket, params);

return(result);
}


CELL * p_receive(CELL * params)
{
UINT pid;
CELL * cell;
SPAWN_LIST * child = NULL;
SYMBOL * sPtr = NULL;
int socket;

/* return list of readable child pids */
if(params == nilCell) 
    return(getSelectReadyList(SELECT_READ_READY));

params = getInteger(params, &pid);

if(params != nilCell)
    {
    getEvalDefault(params, &cell);
    if(!symbolCheck)
        return(errorProc(ERR_IS_NOT_REFERENCED));
    if(isProtected(symbolCheck->flags))
        return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));
    if(symbolCheck->contents != (UINT)cell)
        return(errorProc(ERR_IS_NOT_REFERENCED));
    sPtr = symbolCheck;
    }

/* read from parent */
if(pid == parentPid)
    {
    socket = thisSocket;
    }
else  /* read from child */
    {
    if((child = getSpawnedChild(pid)) == NULL)
        errorProcExt2(ERR_INVALID_PID, stuffInteger(pid));
        socket = child->socket;
    }

if(!socket)
    errorProc(ERR_NO_SOCKET);

cell = readWriteSocket(socket, nilCell);
if(cell == nilCell)
    return(nilCell);

/* if no msg variable is given make message the return value */
if(sPtr == NULL)
    return(cell);

deleteList((CELL *)sPtr->contents);
sPtr->contents = (UINT)cell;
pushResultFlag = FALSE;

return(trueCell);
}


/* evaluate expression in params and write to socket,
   part of a socket pair. Similar to readWriteShare()
   but uses sockets instead od shared memory */
   
CELL * readWriteSocket(int socket, CELL * params)
{
char * buffer;
CELL * cell;
STREAM strStream = {NULL, NULL, 0, 0, 0};
UINT length;
ssize_t size, bytesReceived;
struct timeval tv;
fd_set fdset;
int ready;

tv.tv_sec = 0;
tv.tv_usec = 892 + random()/10000000;

FD_ZERO(&fdset);
FD_SET(socket, &fdset);

if(params != nilCell) /* send message, write */
    {
    /* ready = select(socket, NULL, &fdset, NULL, &tv); */
    ready = select(FD_SETSIZE, NULL, &fdset, NULL, &tv); 
    /* ready = FD_ISSET(socket, &fdset) */
    if(ready == 1)
        {
        cell = evaluateExpression(params);
        if(cell->type == CELL_EXPRESSION)
            size = 128;
        else
            size = 32;
        openStrStream(&strStream, size, 0);
        prettyPrintFlags |= PRETTYPRINT_STRING;
        printCell(cell , TRUE, (UINT)&strStream);
        prettyPrintFlags &= ~PRETTYPRINT_STRING;
        length = strStream.position;
        if(send(socket, &length, sizeof(UINT), 0) ==  sizeof(UINT))
            {
            size = send(socket, strStream.buffer, strStream.position, 0);
            if(size == strStream.position)
                {
                closeStrStream(&strStream);
                return(trueCell);
                }
            /* caller should check errno using (sys-error) */
            closeStrStream(&strStream);
            return(nilCell);
            }
        }
    else
        {
        /* timeout, socket not ready */
        return(nilCell);
        }
    }
/* receive message, read */
/* ready = select(socket, &fdset, NULL, NULL, &tv); */
ready = select(FD_SETSIZE, &fdset, NULL, NULL, &tv);
/* ready = FD_ISSET(socket, &fdset) */
if(ready == 1)
    {
    if((size = recv(socket, &length, sizeof(UINT), 0)) == sizeof(UINT))
        {
        buffer = callocMemory(length + 1);
        bytesReceived = 0;
        while(bytesReceived < length)
            {
            size = recv(socket, buffer + bytesReceived, length - bytesReceived, 0);
            if(size == -1)
                {
                free(buffer);
                return(nilCell);
                }
            bytesReceived += size; 
            }
        cell = sysEvalString(buffer, currentContext, nilCell, READ_EXPR_SYNC);
        free(buffer);
        return(cell);
        }
    }

return(nilCell);
}

#endif /* NO_SPAWN */

/* --------------------------- end Cilk ------------------------------------- */


extern SYMBOL * symHandler[];

CELL * p_waitpid(CELL * params)
{
UINT pid, options;
int result, retval;

symHandler[SIGCHLD - 1] = nilSymbol;
signal(SIGCHLD, SIG_DFL);

params = getInteger(params, (UINT *)&pid);
if(params != nilCell)
    {
    params = evaluateExpression(params);
    if(isNil(params))
        options = WNOHANG;
    else
        getIntegerExt(params, (UINT *)&options, FALSE);
    }
else
    options = 0;

retval = waitpid((int)pid, &result , (int)options);

return(stuffIntegerList(2, (UINT)retval, (UINT)result));
}

#endif

CELL * p_destroyProcess(CELL * params)
{
UINT pid;
UINT sig;

params = getInteger(params, &pid);
if(params != nilCell)   
    getInteger(params, &sig);
else
    sig = 9;

if(kill(pid, sig) != 0)
    return(nilCell);

return(trueCell);
}

/* ------------------------------ semaphores --------------------------------- */
#ifndef NO_SEMAPHORE
#ifdef WINDOWS

UINT winCreateSemaphore(void);
UINT winWaitSemaphore(UINT hSemaphore);
UINT winSignalSemaphore(UINT hSemaphore, int count);
UINT winDeleteSemaphore(UINT hSemaphore);
int getSemaphoreCount(UINT hSemaphore);

CELL * p_semaphore(CELL * params)
{
UINT sem_id;
INT value;

if(params != nilCell)
    {
    params = getInteger(params, &sem_id);
    if(params != nilCell)
        {
        getInteger(params,(UINT *)&value);
        if(value == 0)
            {
            if(!winDeleteSemaphore(sem_id)) 
                return(nilCell);
            return(trueCell);
            }

        /* wait or signal */
        if(value < 0)
            {
            if(winWaitSemaphore(sem_id)) return(trueCell);
            return(nilCell);
            }
        if(value > 0)
            {
            if(winSignalSemaphore(sem_id, value)) return(trueCell);
            return(nilCell);    
            }
        }

    else
        {
        /* return semaphore value, not on Win32 ? */
        return(nilCell);
        }
    }

/* create semaphore */
if((sem_id = winCreateSemaphore()) == 0) return(nilCell);
return(stuffInteger(sem_id));
}
#else /* Mac OS X, Linux/UNIX */

CELL * p_semaphore(CELL * params)
{
INT sem, value, result;

if(params == nilCell)
    {
    result = semaphore(0, 0, SEM_CREATE);
    goto SEMAPHORE_END;
    }

params = getInteger(params, (UINT *)&sem);
if(params == nilCell)
    {
    result = semaphore(sem, 0, SEM_STATUS);
    goto SEMAPHORE_END;
    }

getInteger(params, (UINT *)&value);
    {
    result = semaphore(sem, value, SEM_SIGNAL);
    if(result != -1) return(trueCell);
    }

SEMAPHORE_END:
if(result == -1) return(nilCell);
return(stuffInteger((UINT)result));
}


int semaphore(UINT sem_id, int value, int type)
{
struct sembuf sem_b;
#ifdef SPARC
#ifndef NEWLISP64
int semun_val = 0;
#endif
#endif

#if defined(MAC_OSX) || defined(LINUX) || defined(KFREEBSD)
union semun semu;

semu.val = 0;
#endif

if(type != SEM_CREATE)
    {
    if(type == SEM_SIGNAL)
        {
        if(value == 0)
            {
            /* remove semaphore */
#ifdef SPARC
    #ifndef NEWLISP64
            if(semctl(sem_id, 0, IPC_RMID, &semun_val) == -1) /* SPARC 32 */
    #else 
            if(semctl(sem_id, 0, IPC_RMID, 0) == -1) /* SPARC 64 */
    #endif

#else /* not SPARC */
    #if defined(MAC_OSX) || defined(LINUX) || defined(KFREEBSD)
            if(semctl(sem_id, 0, IPC_RMID, semu) == -1) /* MAC_OSX, GNU/Linux, GNU/kFreeBSD */
    #else
            if(semctl(sem_id, 0, IPC_RMID, 0) == -1) /* BSD, TRU64 */
    #endif /* not MAC_OSX */
#endif /* not SPARC */
                return(-1);
            return(0);
            }

        /* wait or signal */
        sem_b.sem_num = 0;
        sem_b.sem_op = value;
        sem_b.sem_flg = 0; 
        if(semop(sem_id, &sem_b, 1) == -1)
            return(-1);
        return(0);
        }

    else
        /* return semaphore value */
#if defined(MAC_OSX) || defined(LINUX) || defined(KFREEBSD)
        return(semctl(sem_id, 0, GETVAL, semu));
#else
        return(semctl(sem_id, 0, GETVAL, 0));
#endif
    }

/* create semaphore */
sem_id = semget(IPC_PRIVATE, 1, 0666 );

#ifdef SPARC
  #ifndef NEWLISP64
if(semctl(sem_id, 0, SETVAL, &semun_val) == -1) /* SPARC 32 */
  #else
if(semctl(sem_id, 0, SETVAL, 0) == -1) /* SPARC 64 */
  #endif
#else /* not SPARC */
 #if defined(MAC_OSX) || defined(LINUX) || defined(KFREEBSD)
if(semctl(sem_id, 0, SETVAL, semu) == -1) /* MAC_OSX, GNU/Linux, GNU/kFreeBSD */
 #else
if(semctl(sem_id, 0, SETVAL, 0) == -1) /* BSD, TRU64 */
 #endif /* not MAC_OSX */
#endif /* not SPARC */
    return(-1);

return(sem_id);
}

#endif /* MAC OSX, Unix, Linux */
#endif /* NO_SEMAPHORE */


#ifndef NO_SHARE

#ifdef WINDOWS
UINT winSharedMemory(int size);
UINT * winMapView(UINT handle, int size);
#endif

/* since 10.1.0 also can share object > pagesize
   objects are stored in the tmp directory of OS
   as a file starting with nls-
*/

CELL * p_share(CELL * params)
{
void * address;
CELL * cell;
#ifdef WINDOWS
UINT handle;
#endif

/* read write  or release (UNIX) shared memory */
if(params != nilCell) 
    {
    cell = evaluateExpression(params);
#ifndef WINDOWS
    if(isNil(cell)) /* release shared address */
        {
        getInteger(params->next, (UINT *)&address);
        checkDeleteShareFile(address);
        if(munmap(address, pagesize) == -1)
            return(nilCell);
        else
            return(trueCell);
        }
#endif
    getIntegerExt(cell, (UINT *)&address, FALSE);
    params = params->next;
#ifdef WINDOWS
    if((address = winMapView((UINT)address, pagesize)) == NULL)
        return(nilCell);
#endif
    return(readWriteShared(address, params, 0));
    }

/* get shared memory UNIX */
#ifndef WINDOWS
if((address = (UINT*)mmap(
    0, pagesize, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0)) == (void*)-1)
        return(nilCell);

memset((char *)address, 0, pagesize);
return(stuffInteger((UINT)address));

/* get shared memory WINDOWS */
#else 

if((handle = winSharedMemory(pagesize)) == 0)
    return(nilCell);

if((address = winMapView(handle, pagesize)) == NULL)
    return(nilCell);

memset((char *)address, 0, pagesize);
return(stuffInteger(handle));
#endif
}
#endif /* NO_SHARE  */

/* evaluate the expression in params and the write the result
   to shared memory. If size > pagesize use files tmp files
   for transfer. For atomic datatypes are xlation into message
   optimized for speed (but doesn't bring much in overall
   performance, should perhaps be taken out and only use
   readWriteSharedExpression() */
CELL * readWriteShared(UINT * address, CELL * params, int flag)
{
CELL * cell;
size_t size;
char * str;
int errNo;

/* write to shared memory */
if(params != nilCell) 
    {
    if(flag) /* in spawned process */
        {
        if((cell = evaluateExpressionSafe(params, &errNo)) == NULL)
            cell = stuffString(errorStream.buffer);
        }
    else 
        cell = evaluateExpression(params);

    /* if a previous share mem file is still present, delete it
       when *address == 0 when called from Cilk then file is
       deleted p_message(). Here only used from p_share() */
    if(*address == (CELL_STRING | SHARED_MEM_EVAL_MASK))
        checkDeleteShareFile(address);

    /* write anything not bool, number or string */
    if((cell->type & COMPARE_TYPE_MASK) > (CELL_STRING & COMPARE_TYPE_MASK))
        return(copyCell(readWriteSharedExpression(address, cell)));

    switch(cell->type)
        {
        case CELL_NIL:
            *address = cell->type;
#ifdef WINDOWS
            UnmapViewOfFile(address);
#endif
            return(nilCell);
        case CELL_TRUE:
            *address = cell->type;
#ifdef WINDOWS
            UnmapViewOfFile(address);
#endif
            return(trueCell);
        case CELL_LONG:
            *(address + 1) = sizeof(INT);
            *(address + 2) = cell->contents;
            break;
#ifndef NEWLISP64
        case CELL_INT64:
            *(address + 1) = sizeof(INT64);
            memcpy(address + 2, (void *)&cell->aux, sizeof(INT64));
            break;
        case CELL_FLOAT:
            *(address + 1) = sizeof(double);
            *(address + 2) = cell->aux;
            *(address + 3) = cell->contents;
            break;
#else /* NEWLISP64 */
        case CELL_FLOAT:
            *(address + 1) = sizeof(double);
            *(address + 2) = cell->contents;
            break;
#endif /* NEWLISP64 */
        case CELL_STRING:
            getStringSize(cell, &str, &size, FALSE);
            if(size > (pagesize - 3 * sizeof(INT)))
                return(copyCell(readWriteSharedExpression(address, cell)));

            *(address + 1) = size;
            memcpy((char *)(address + 2), str, size);
            *((char *)address + 2 * sizeof(INT) + size) = 0;
            break;
        default:
            return(errorProcExt(ERR_ILLEGAL_TYPE, cell));
        }

    *address = cell->type;
#ifdef WINDOWS
    UnmapViewOfFile(address);
#endif
    return(copyCell(cell));
    }

/* read shared memory */
switch(*address & RAW_TYPE_MASK)
    {
    case CELL_NIL:
#ifdef WINDOWS 
        UnmapViewOfFile(address);
#endif
        return(nilCell);
    case CELL_TRUE:
#ifdef WINDOWS 
        UnmapViewOfFile(address);
#endif
        return(trueCell);   
    case CELL_LONG:
        cell = stuffInteger(*(address + 2));
        break;
#ifndef NEWLISP64
    case CELL_INT64:
        cell = stuffInteger64(*(INT64 *)(address + 2));
        break;
#endif
    case CELL_FLOAT:
#ifndef NEWLISP64
        cell = getCell(CELL_FLOAT);
        cell->aux = *(address + 2);
        cell->contents = *(address + 3);
#else
        cell = getCell(CELL_FLOAT);
        cell->contents = *(address + 2);
#endif
        break;
    case CELL_STRING:
        if(*address & SHARED_MEM_EVAL_MASK)
            return(readWriteSharedExpression(address, nilCell));
        size = *(address + 1);
        cell = makeStringCell(allocMemory(size + 1), size);
        memcpy((char *)cell->contents, (char*)(address + 2), cell->aux);
        break;
    default:
        return(nilCell);
    }

#ifdef WINDOWS
        UnmapViewOfFile(address);
#endif
return(cell);
}


/* Takes anything and passes as string or file which has to
   be compiled back into expression when reading.
   Returns a new cell object on read, old on write
*/

CELL * readWriteSharedExpression(UINT * address, CELL * params)
{
ssize_t size;
STREAM strStream = {NULL, NULL, 0, 0, 0};
CELL * cell;
char * buffer = NULL;
/* int errNo; */

/* read */
if(params == nilCell)
    {
    size = *(address + 1);
    if(size < (pagesize - 3 * sizeof(INT) ))
        {   
        cell = sysEvalString((char *)(address + 2), 
                currentContext, nilCell, READ_EXPR_SYNC);
        }
    else 
        {
        if((size = readFile((char *)(address + 2), &buffer)) != -1)
            cell = sysEvalString(buffer, currentContext, nilCell, READ_EXPR_SYNC);
        else cell = nilCell;
        }

    if(buffer != NULL) free(buffer); 
    return(cell);
    }

/* write */
cell = params;
openStrStream(&strStream, MAX_STRING, 0);
prettyPrintFlags |= PRETTYPRINT_STRING;
printCell(cell , TRUE, (UINT)&strStream);
prettyPrintFlags &= ~PRETTYPRINT_STRING;

*(address + 1) = strStream.position;

if(strStream.position < pagesize - 3 * sizeof(INT))
    {
    memcpy((char *)(address + 2), strStream.buffer, strStream.position);
    *((char *)address + 2 * sizeof(INT) + strStream.position) = 0;
    }
else
    {
    checkDeleteShareFile(address);
    memset((char *)(address + 2), 0, pagesize - 2 * sizeof(INT));
    strncpy((char *)(address + 2), tempDir, PATH_MAX - 2 * sizeof(INT));
    strncat((char *)(address + 2), "/nls-", 6);
    size = strlen((char *)(address + 2));
    getUUID((char *)(address + 2) + size, 0);
    writeFile((char *)(address + 2), strStream.buffer, strStream.position, "w");
    }
closeStrStream(&strStream);

*address = (CELL_STRING | SHARED_MEM_EVAL_MASK);
return(cell);
}

void checkDeleteShareFile(UINT * address)
{
if(     (*address == (CELL_STRING | SHARED_MEM_EVAL_MASK)) &&
#ifndef WINDOWS
#ifdef ANDROID
        (strncmp((char *)(address + 2), "/data/tmp/nls-", 9) == 0) &&
#else
        (strncmp((char *)(address + 2), "/tmp/nls-", 9) == 0) &&
#endif
        (strlen((char *)(address + 2)) == 45) )
#else
        (strncmp((char *)(address + 2), "/temp/nls-", 10) == 0) &&
        (strlen((char *)(address + 2)) == 46) )
#endif
    unlink((char *)(address + 2)); 
}
#endif /* ifndef EMSCRIPTEN */

extern int ADDR_FAMILY;
CELL * p_systemInfo(CELL * params)
{
CELL * cell;

cell = stuffIntegerList(
    10,
    cellCount,
    MAX_CELL_COUNT,
    symbolCount,
    (UINT)recursionCount,
    (UINT)(envStackIdx - envStack)/sizeof(UINT),
    (UINT)MAX_CPU_STACK,
    (UINT)parentPid,
    (UINT)getpid(),
    (UINT)version,
    (UINT)opsys
    );

if(params != nilCell)   
    {
    pushResult(cell);
    return(copyCell(implicitIndexList(cell, params)));
    }

return(cell);
}


CELL * p_systemError(CELL * params)
{
CELL * cell;
UINT errnum = errno;

if(params != nilCell)
    {
    getInteger(params, &errnum);
    if(errnum == 0) errno = 0;
    }
else 
    if(!errnum) return(nilCell);

cell = makeCell(CELL_EXPRESSION, (UINT)stuffInteger(errnum));
((CELL *)cell->contents)->next = stuffString(strerror(errnum));

/* on some platforms strerror(0) causes errno set to 22 */
if(errnum == 0) errno = 0;

return(cell);
}

/* ------------------------------ time and date functions -------------------- */
CELL * p_date(CELL * params)
{
time_t t;
struct tm * ltm;
char * ct;
char * fmt;
ssize_t offset;
/* time_t tme; 10.6.1. */
UINT tme;
size_t size;

#ifdef SUPPORT_UTF8
#ifdef WCSFTIME
int * ufmt;
int * timeString;
#endif
char * utf8str;
#else
char * timeString;
#endif

if(params == nilCell)
    t = (time_t)currentDateValue();
else
    {
    /* 10.6.1 */
    params = getInteger(params, &tme);
    t = (time_t)tme;
    
    if(params != nilCell)
        {
        params = getInteger(params, (UINT *)&offset);
            t += (int)offset * 60;
        }
        
    if(params != nilCell)
        {
        params = getStringSize(params, &fmt, &size, TRUE);
        ltm = localtime(&t);
#ifdef SUPPORT_UTF8
    /* some Linux do UTF-8 but don't have wcsftime() or it is buggy */
#ifdef WCSFTIME
        size = utf8_wlen(fmt, fmt + size + 1);
        ufmt = alloca(UTF8_MAX_BYTES * (size + 1));
        utf8_wstr(ufmt, fmt, size);

        timeString = alloca(UTF8_MAX_BYTES * 128);
        size = wcsftime((wchar_t *)timeString, 127, (wchar_t *)ufmt, ltm);
        utf8str = alloca(size * UTF8_MAX_BYTES + 1);
        size =  wstr_utf8(utf8str, timeString, size * UTF8_MAX_BYTES);
        return(stuffString(utf8str));
#else
        utf8str = alloca(128);
        strftime(utf8str, 127, fmt, ltm);
        return(stuffString(utf8str));
#endif /* WCSFTIME */

#else
        timeString = alloca(128);
        strftime(timeString, 127, fmt, ltm);
        return(stuffString(timeString));
#endif
        }
    }

ct = ctime(&t);
if(ct == NULL) return(nilCell);

ct[strlen(ct) - 1] = 0;  /* supress linefeed */
return(stuffString(ct));
}

INT64 microSecTime(void)
{
struct timeval tv;
struct tm * ttm;
time_t sec;

gettimeofday(&tv, NULL);
sec = tv.tv_sec;
ttm = localtime(&sec);

return (ttm->tm_hour * 3600000000LL + 
       ttm->tm_min * 60000000LL + ttm->tm_sec * 1000000 + 
       tv.tv_usec);
}


int milliSecTime(void)
{
return(microSecTime()/1000);
}


/* returns a differerence of 2 timeval structs in milliseconds
*/
int timediff_ms(struct timeval out, struct timeval in )
{
    if( (out.tv_usec -= in.tv_usec) < 0 )   {
        out.tv_sec--;
        out.tv_usec += 1000000;
    }
    out.tv_sec -= in.tv_sec;

return(out.tv_sec*1000 + (out.tv_usec/1000));
}


/* returns a differerence of 2 timeval structs in microseconds
*/
UINT64 timediff64_us(struct timeval out, struct timeval in )
{
UINT64 usec;

    if( (out.tv_usec -= in.tv_usec) < 0 )   {
        out.tv_sec--;
        out.tv_usec += 1000000;
    }
    out.tv_sec -= in.tv_sec;

usec = (UINT64)1000000 * out.tv_sec + out.tv_usec;
return(usec);
}

#ifndef WINDOWS
CELL * p_dateParse(CELL * params)
{
struct tm ttm;
char * dateStr;
char * formatStr;
time_t dateValue;

params = getString(params, &dateStr);
getString(params, &formatStr);

memset (&ttm, 0, sizeof (ttm));
ttm.tm_mday = 1;

if(strptime(dateStr, formatStr, &ttm) == NULL)
    return(nilCell);

dateValue = calcDateValue(
        ttm.tm_year + 1900,
        ttm.tm_mon + 1,
        ttm.tm_mday,
        ttm.tm_hour,
        ttm.tm_min,
        ttm.tm_sec);

return(stuffInteger(dateValue));
}
#endif

CELL * p_time(CELL * params)
{
struct timeval start, end;
INT64 N = 1;
UINT * resultIdxSave;
double diff;

gettimeofday(&start, NULL);
if(params->next != nilCell)
    getInteger64Ext(params->next, &N, TRUE);

resultIdxSave = resultStackIdx;
while(N--)  
    {
    evaluateExpression(params);
    cleanupResults(resultIdxSave);
    }

gettimeofday(&end, NULL);

diff = (1.0 * timediff64_us(end, start)) / 1000;
return(stuffFloat(diff));
}


CELL * p_timeOfDay(CELL * params)
{
double microSecs = microSecTime()/1000.0;
return(stuffFloat(microSecs));
}


CELL * p_now(CELL * params)
{
struct timeval tv;
struct tm *ttm;
#ifndef WINDOWS
struct tm *ltm;
#ifndef SUNOS
#ifndef OS2
#ifndef AIX
INT gmtoff;
UINT isdst;
#endif
#endif
#endif
#else /* WINDOWS */
TIME_ZONE_INFORMATION timeZone;
int retval;
#endif
ssize_t offset = 0;
time_t sec;
CELL * cell;

gettimeofday(&tv, NULL);

if(params != nilCell)
    {
    params = getInteger(params, (UINT*)&offset);
    offset *= 60;
        tv.tv_sec += offset;
    }

#ifndef WINDOWS
ltm = localtime((time_t *)&tv.tv_sec);
#ifndef SUNOS
#ifndef OS2
#ifndef AIX
isdst = ltm->tm_isdst;

#ifdef CYGWIN
gmtoff = _timezone/60;
#else
gmtoff = ltm->tm_gmtoff/60;
#endif

#endif
#endif
#endif
#else /* WINDOWS */
memset((void *)&timeZone, 0, sizeof(timeZone));
retval = GetTimeZoneInformation(&timeZone);
#endif

sec = tv.tv_sec;
ttm = gmtime(&sec);

cell = stuffIntegerList(
    11,
    (UINT)ttm->tm_year + 1900,
    (UINT)ttm->tm_mon + 1,
    (UINT)ttm->tm_mday,
    (UINT)ttm->tm_hour,
    (UINT)ttm->tm_min,
    (UINT)ttm->tm_sec,
    (UINT)tv.tv_usec,
    (UINT)ttm->tm_yday + 1,
    ((UINT)ttm->tm_wday == 0 ? 7 : (UINT)ttm->tm_wday),

#if defined(MAC_OSX) || defined(LINUX) || defined(_BSD) || defined(KFREEBSD) || defined(CYGWIN)
    gmtoff, isdst
#endif

#if defined(SUNOS)
    timezone/60, daylight
#endif

#if defined(OS2) || defined(TRU64) || defined(AIX)
#ifdef NEWLISP64
    (UINT)0L, (UINT)0L
#else
    (UINT)0, (UINT)0
#endif
#endif

#if defined(WINDOWS)
    (retval == 2) ?  ((UINT)-timeZone.Bias - (UINT)timeZone.DaylightBias) : (UINT)-timeZone.Bias,
    (UINT)retval
#endif
    );

if(params != nilCell)   
    {
    pushResult(cell);
    return(copyCell(implicitIndexList(cell, params)));
    }

return(cell);
}

CELL * p_dateList(CELL * params)
{
struct tm *ttm;
ssize_t timeValue;
time_t timer;
CELL * cell;

if(params == nilCell)
    timeValue = currentDateValue();
else
    params = getInteger(params, (UINT*)&timeValue);

timer = (time_t)timeValue;
if((ttm = gmtime(&timer)) == NULL)
    return(errorProcExt2(ERR_INVALID_PARAMETER, stuffInteger((UINT)timeValue)));

cell = stuffIntegerList(
    8,
    (UINT)ttm->tm_year + 1900,
    (UINT)ttm->tm_mon + 1,
    (UINT)ttm->tm_mday,
    (UINT)ttm->tm_hour,
    (UINT)ttm->tm_min,
    (UINT)ttm->tm_sec,
    (UINT)ttm->tm_yday + 1,
    ((UINT)ttm->tm_wday == 0 ? 7 : (UINT)ttm->tm_wday)
);

if(params != nilCell)   
    {
    pushResult(cell);
    return(copyCell(implicitIndexList(cell, params)));
    }

return(cell);
}

ssize_t currentDateValue(void)
{
struct timeval tv;

gettimeofday(&tv, NULL);
return(tv.tv_sec);
}

CELL * p_dateValue(CELL * params)
{
ssize_t year, month, day, hour, min, sec;
time_t dateValue;
int evalFlag = TRUE;
CELL * next;

if(params->type == CELL_NIL)
    return(stuffInteger(currentDateValue()));

next = params->next;
params = evaluateExpression(params);
if(params->type == CELL_EXPRESSION)
    {
    params = (CELL *)params->contents;
    next = params->next;
    evalFlag = FALSE;
    }

params = getIntegerExt(params, (UINT *)&year, FALSE);
params = getIntegerExt(next, (UINT *)&month, evalFlag);
params = getIntegerExt(params, (UINT *)&day, evalFlag);

hour = min = sec = 0;
if(params != nilCell)
        {
        params = getIntegerExt(params, (UINT *)&hour, evalFlag);
        params = getIntegerExt(params, (UINT *)&min, evalFlag);
        getIntegerExt(params, (UINT *)&sec, evalFlag);
        }

dateValue = calcDateValue(year, month, day, hour, min, sec);

#ifndef NEWLISP64
return(stuffInteger64((INT64)dateValue));
#else
return(stuffInteger((UINT)dateValue));
#endif
}



/* changed for 10.6.1 where time_t can be 64-bit on 32-bit Windows */
time_t calcDateValue(int year, int month, int day, int hour, int min, int sec)
{
time_t dateValue;
INT64 value;

value = 367 * year - (7 * (year + ((month + 9) / 12)))/4 
            + (275 * month)/9 + day + 1721013;

value = value * 24 * 3600 + hour * 3600 + min * 60 + sec 
            - 413319296; /* correction for 1970-1-1 */

if(sizeof(time_t) == 8)
    {
    if(value & 0x80000000)
        dateValue = value | 0xFFFFFFFF00000000LL;
    else
        dateValue = value & 0x00000000FFFFFFFF; 
    }
else
    dateValue = value;

return(dateValue);
}


#ifdef MAC_OSX
extern int nanosleep();
#endif

void mySleep(int ms)
{
#ifdef NANOSLEEP
struct timespec tm;

tm.tv_sec = ms / 1000;
tm.tv_nsec = (ms - tm.tv_sec * 1000) * 1000000;
nanosleep(&tm, 0);

#else

#ifdef WINDOWS
Sleep(ms);
#else
sleep((ms + 500)/1000);
#endif

#endif
}

#ifdef NANOSLEEP
void myNanoSleep(int nanosec)
{
struct timespec tm;

tm.tv_sec =  nanosec / 1000000000;
tm.tv_nsec = (nanosec - tm.tv_sec * 1000000000);
nanosleep(&tm, 0);
}
#endif


CELL * p_sleep(CELL * params)
{
double milliSecsFloat;
#ifdef NANOSLEEP
int nanoSecsInt;
#endif

getFloat(params, &milliSecsFloat);

mySleep((UINT)milliSecsFloat);
#ifdef NANOSLEEP
nanoSecsInt = (milliSecsFloat - (int)milliSecsFloat) * 1000000;
if(nanoSecsInt) myNanoSleep(nanoSecsInt);
#endif

return(stuffFloat(milliSecsFloat));
}

/* -------------------------------- environment functions ------------------- */


CELL * p_env(CELL * params)
{
char * varName;
char * varValue;

/* no parameters returns whole environment */
if(params == nilCell) 
    return(environment());

/* one parameter get environment for one variable */
params = getString(params, &varName);
if(params == nilCell) 
    {
    if( (varValue = getenv(varName)) == NULL)
        return(nilCell);
    return(stuffString(varValue));
    }

/* two parameters sets environment for one variable */
getString(params, &varValue);
#ifndef MY_SETENV
if(*varValue == 0)
    unsetenv(varName);
else
#endif
    if(setenv(varName, varValue, 1) != 0)
        return(nilCell);

return(trueCell);
}


#ifdef MY_SETENV
int my_setenv(const char * varName, const char * varValue, int flag)
{
char * envstr;
envstr = alloca(strlen(varName) + strlen(varValue) + 2);
strcpy(envstr, varName);
strcat(envstr, "=");
strcat(envstr, varValue);
return(putenv(envstr));
}
#endif


CELL * environment(void)
{
char ** env;
CELL * envList;
CELL * lastEntry;
CELL * pair;
char * ptr;

lastEntry = NULL;
envList = getCell(CELL_EXPRESSION);

env = environ;

while(*env)
    {
    if((ptr = strstr(*env, "=")) != NULL)
        {
        pair = getCell(CELL_EXPRESSION);
        addList(pair, stuffStringN(*env, ptr - *env));
        addList(pair, stuffString(ptr + 1));
        }
    else 
        {
        env++;
        continue;
        }
    
    if(lastEntry == NULL)
        {
        lastEntry = pair;
        envList->contents = (UINT)lastEntry;
        }
    else
        {
        lastEntry->next = pair;
        lastEntry = lastEntry->next;
        }
    env++;
    }

return(envList);
}

/* --------------------- read the keyboard -----------------------------------*/

/* thanks to Peter van Eerten for contributing this function */
/* included non-blocking ability 10.7.3, LM */

CELL * p_readKey(CELL * params)
{

#if defined(WINDOWS) || defined(OS2)
if(!isNil(evaluateExpression(params)) )
	{
	if(kbhit()) 
		return(stuffInteger(getch()));
	else 
		return(stuffInteger(0));
	}
else
	return(stuffInteger(getch()));
#else

struct termios term, oterm;
char ch = 0;
int noblock = 0;
int oldf;

noblock = !isNil(evaluateExpression(params));

tcgetattr(0, &oterm);
term = oterm;
term.c_lflag &= ~(ICANON | ECHO);

if(!noblock)
    {
    term.c_cc[VMIN] = 0;
    term.c_cc[VTIME] = 1;
    }

tcsetattr(STDIN_FILENO, TCSANOW, &term);

if(noblock)
    {
    oldf = fcntl(STDIN_FILENO, F_GETFL, 0);
    fcntl(STDIN_FILENO, F_SETFL, oldf | O_NONBLOCK);
    }

while(read(STDIN_FILENO, &ch, 1) == 0); 

if(noblock)
    fcntl(STDIN_FILENO, F_SETFL, oldf);

tcsetattr(STDIN_FILENO, TCSANOW, &oterm);

if(ch != EOF)
    return(stuffInteger((UINT)ch));

return(stuffInteger(0));
#endif /* not Windows or OS2 */
} 

/* --------------------- peek a file descriptor ------------------------------*/

#ifndef WINDOWS
CELL * p_peek(CELL * params)
{
UINT handle;
int result;

getInteger(params, &handle);

if(ioctl((int)handle, FIONREAD, &result) < 0)
    return(nilCell);

return(stuffInteger((UINT)result));
} 
#endif

/* --------------------- library functions not found on some OSs -------------*/

#ifdef MY_VASPRINTF
int my_vasprintf(char * * buffer, const char * format, va_list argptr)
{
int size;

/* get size */
size = vsnprintf(NULL, 0, format, argptr);
if (size < 0) return -1;

*buffer = calloc(size + 1, 1);
if (!*buffer) return(-1);

vsnprintf(*buffer, size + 1, format, argptr);
(*buffer)[size] = '\0';

return(size);
}
#endif


/* ---------------------- Universal Unique ID version 1 and 3 ----------- */

#define UINT16 unsigned short
#define UINT32 unsigned int

typedef struct 
    {
    UINT32          time_low;
    UINT16          time_mid;
    UINT16          time_hi_and_version;
    unsigned char   clock_seq_hi_and_reserved;
    unsigned char   clock_seq_low;
    unsigned char   node[6];
    } UUID;

UINT16 clock_seq = 0;
INT64 last_time = 0;
char last_node[6];

#define OCT151582 0x01B21DD213814000LL

char * getUUID(char * str, char * node) 
{
UUID uuid;
struct timeval tp;
INT64 timestamp;
UINT16 nodeID[3];
int uuid_version;

gettimeofday(&tp, (struct timezone *)0);

/* add UUID UTC offset Oct 15, 1582 */
timestamp = tp.tv_sec * (INT64)10000000 + tp.tv_usec * 10 + OCT151582; 

#ifdef WINDOWS
if(timestamp <= last_time) timestamp = last_time + 1;
#else
if(timestamp < last_time) clock_seq++;
if(timestamp == last_time) timestamp++;
#endif

if(last_time == 0)
    srandom((timestamp & 0xFFFFFFFF) + getpid());

last_time = timestamp;


if(clock_seq == 0) clock_seq = random();
if(node != NULL && (memcmp(last_node, node, 6) != 0))
    {
    clock_seq = random();
    memcpy(last_node, node, 6);
    }

if(node == NULL)
    {
    nodeID[0] = random(); 
    nodeID[1] = random();
    nodeID[2] = random();
    uuid_version = 4;
    memcpy(uuid.node, (void *)nodeID, 6);
    }
else
    {
    uuid_version = 1;
    /* least sign bit of first byte must be 0 on MACs
       and 1 on artifical generated node IDs */
    memcpy(uuid.node, node, 6);
    }

if(uuid_version == 4) 
    {
    clock_seq = random();
    uuid.time_low = random();
#ifdef WINDOWS
    uuid.time_low |= (random() << 16);
#endif
    uuid.time_mid = random();
    uuid.time_hi_and_version = random();
    }
else
    {
    uuid.time_low = (unsigned int)(timestamp & 0xFFFFFFFF);
    uuid.time_mid = (unsigned short)((timestamp >> 32) & 0xFFFF);
    uuid.time_hi_and_version = (unsigned short)(timestamp >> 48) ;
    }

uuid.time_hi_and_version &= 0x0FFF;
uuid.time_hi_and_version |= (uuid_version << 12);
uuid.clock_seq_low = clock_seq & 0xFF;
uuid.clock_seq_hi_and_reserved = (clock_seq & 0x3F00) >> 8;
uuid.clock_seq_hi_and_reserved |= 0x80;

snprintf(str, 37, "%08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X", 
    uuid.time_low, uuid.time_mid, uuid.time_hi_and_version,
    uuid.clock_seq_hi_and_reserved, uuid.clock_seq_low,
    uuid.node[0], uuid.node[1], uuid.node[2], 
    uuid.node[3], uuid.node[4], uuid.node[5]);

return(str);
}

CELL * p_uuid(CELL * params)
{
char * nodeMAC = NULL;
size_t size;
char str[38];

if(params != nilCell)
    {
    getStringSize(params, &nodeMAC, &size, TRUE);
    if(size < 6) nodeMAC = NULL;
    }
    
return(stuffString(getUUID(str, nodeMAC)));
}


SYMBOL * getSymbolCheckProtected(CELL * params)
{
SYMBOL * sPtr = NULL;
CELL * cell;

if(params->type == CELL_SYMBOL)
    {
    sPtr = (SYMBOL *)params->contents;
    cell = (CELL *)sPtr->contents;
    if(cell->type == CELL_CONTEXT)
        sPtr = translateCreateSymbol( ((SYMBOL*)cell->contents)->name, CELL_NIL,
            (SYMBOL*)cell->contents, TRUE);
    }
else if(params->type == CELL_DYN_SYMBOL)
    sPtr = getDynamicSymbol(params);
else errorProcExt(ERR_SYMBOL_EXPECTED, params);

if(isProtected(sPtr->flags))
    errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(sPtr));

symbolCheck = sPtr;

return sPtr;
}


/* eof */



