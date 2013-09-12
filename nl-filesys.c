/*


    Copyright (C) 2011 Lutz Mueller

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

#if defined(SOLARIS) || defined(TRU64) || defined(AIX) 
#include <stropts.h>
#endif

#ifdef SOLARIS
#define FIONREAD I_NREAD
#endif

#ifndef WIN_32
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#endif

int init_argv(char * ptr, char *argv[]);
char * getUUID(char * str, char * node);

#ifdef OS2
#include <conio.h>
int semctl(int semid, int semnum, int cmd, ...);
#endif

#ifdef MAC_OSX
#ifdef LIBRARY
char ** environ = NULL;
#else
extern char ** environ;
#endif
#else
#ifndef TRU64
extern char ** environ;
#endif
#endif

#ifdef WIN_32
#define fgetc win32_fgetc
#define random rand
#define srandom srand
#include <process.h>
#include <conio.h>  
#include <dir.h>
#define popen  _popen
#define pclose _pclose
#define pipe _pipe

/* 
Set binary as default file mode for Windows.
See also http://www.mingw.org/MinGWiki/index.php/binary
*/
unsigned int _CRT_fmode = _O_BINARY;

int setenv (const char *name, const char *value, int replace);
#ifdef USE_WIN_UTF16PATH
INT64 fileSizeW(WCHAR * pathName);
#endif

#endif /* Win32 */

#ifndef WIN_32
#include <sys/socket.h>
#define SOCKET_ERROR -1
#define INVALID_SOCKET -1
#endif

#if defined(LINUX) || defined(CYGWIN)
char * strptime(const char * str, const char * fmt, struct tm * ttm);
#endif

time_t calcDateValue(int year, int month, int day, int hour, int min, int sec);
extern STREAM readLineStream;
extern FILE * IOchannel;
extern int pagesize;

extern char * errorMessage[];
extern STREAM errorStream;

/* semaphore() function type */
#define SEM_CREATE 0
#define SEM_STATUS 1
#define SEM_SIGNAL 2

/* used in fork and spawn */
int parentPid = 0;
/* share, message */
CELL * readWriteShared(UINT * address, CELL * params);
void checkDeleteShareFile(UINT * address);

CELL * p_isFile(CELL * params) /* includes dev,socket,dir,file etc. */
{
char * fileName;

getString(params, &fileName);
return(isFile(fileName) ? nilCell : trueCell);
}

int isFile(char * fileName)
{
struct stat fileInfo;

#ifdef WIN32
char slash;
size_t len;
int result;

len = strlen(fileName);
slash = *(fileName + len - 1);
if(slash == '\\' || slash == '/')
	*(fileName + len - 1) = 0;

#ifdef USE_WIN_UTF16PATH
result = stat_utf16(fileName, &fileInfo);
#else
result = stat(fileName, &fileInfo);
#endif
if(slash == '\\' || slash == '/')
	*(fileName + len - 1) = slash;
return(result);
#else /* not WIN32 */
return(stat(fileName, &fileInfo));
#endif
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

#ifdef WIN32
char slash;
size_t len;

len = strlen(fileName);
slash = *(fileName + len - 1);
if(slash == '\\' || slash == '/')
	*(fileName + len - 1) = 0;
#endif

#ifdef USE_WIN_UTF16PATH
if(stat_utf16(fileName, &fileInfo) != 0)
#else
if(stat(fileName, &fileInfo) != 0)
#endif
	{
#ifdef WIN32
	*(fileName + len - 1) = slash;
#endif
	return(0);
	}

#ifdef WIN32
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

params = getString(params, &fileName);
params = getString(params, &accessMode);

if(params != nilCell)
	getString(params, &option);
	
if( (handle = openFile(fileName, accessMode, option)) == (int)-1)
	return(nilCell);
return(stuffInteger((UINT)handle));
}

CELL * p_close(CELL * params)
{
UINT handle;

getInteger(params, &handle);
if(handle == 0) return(nilCell);
if(handle == printDevice) printDevice = 0;
if(close((int)handle) == -1) return(nilCell);
return(trueCell);
}

CELL * p_readChar(CELL * params)
{
UINT handle;
unsigned char chr;

getInteger(params, &handle);
if(read((int)handle, &chr, 1) <= 0) return(nilCell);

return(stuffInteger((UINT)chr));
}


CELL * p_readBuffer(CELL * params)
{
UINT handle;
size_t size, length;
ssize_t bytesRead = 0;
int found = 0;
char * waitFor;
char chr;
STREAM stream = {0, NULL, NULL, 0, 0};
CELL * strCell;
SYMBOL * readSptr;

params = getInteger(params, &handle);
params = getEvalDefault(params, &strCell);
if(!symbolCheck)
	return(errorProc(ERR_IS_NOT_REFERENCED));
if(isProtected(symbolCheck->flags))
	return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));
if(symbolCheck->contents != (UINT)strCell)
	return(errorProc(ERR_IS_NOT_REFERENCED));

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
CELL * result;

params = getString(params, &fileName);
if(my_strnicmp(fileName, "http://", 7) == 0)
	{
	result = getPutPostDeleteUrl(fileName, params, HTTP_GET, CONNECT_TIMEOUT);
	if(memcmp((char *)result->contents, "ERR:", 4) == 0)
		return(errorProcExt2(ERR_ACCESSING_FILE, stuffString((char *)result->contents)));
	return(result);
	}

if((size = readFile(fileName, &buffer)) == -1)
	return(nilCell);

return(makeStringCell(buffer, size));
}

/* allocates a buffer and reads a file into it */
ssize_t readFile(char * fileName, char * * buffer)
{
int handle; 
size_t size;
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
CELL * result;

params = getString(params, &fileName);

if(my_strnicmp(fileName, "http://", 7) == 0)
	{
	result = getPutPostDeleteUrl(fileName, params, 
				(*type == 'w') ? HTTP_PUT : HTTP_PUT_APPEND, CONNECT_TIMEOUT);
	if(memcmp((char *)result->contents, "ERR:", 4) == 0)
		return(errorProcExt2(ERR_ACCESSING_FILE, stuffString((char *)result->contents)));
	return(result);
	}

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
		if(write((int)handle, LINE_FEED, strlen(LINE_FEED)) == -1) return(nilCell);
	}

else if(device->type == CELL_STRING)
	{
	if(symbolRef && isProtected(symbolRef->flags))
   		return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolRef)));

	appendCellString(device, buffer, size);
	if(lineFeed)
		appendCellString(device, LINE_FEED, strlen(LINE_FEED));
	}
else
	return(errorProcExt(ERR_INVALID_PARAMETER, device));


RETURN_WRITE_BUFFER:
return(stuffInteger(size + (lineFeed ? strlen(LINE_FEED) : 0)));
}


CELL * p_seek(CELL * params)
{
UINT handle;
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
	else if( (newPosition = lseek(handle, 0, SEEK_CUR)) == -1)
		return(nilCell);
	}
else
	{
#ifdef LFS
	getInteger64(params, &paramPosition);
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
int chr;

openStrStream(stream, MAX_STRING, 1);

#ifdef TRU64
do {
errno = 0;
#endif
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
#ifdef TRU64
} while (errno == EINTR);
#endif

if(chr == EOF && stream->position == 0) return(NULL);
return(stream->buffer);
}


CELL * p_readLine(CELL * params)
{
UINT handle;
unsigned char chr;
char * line;
int bytesRead;


if(params != nilCell)
	getInteger(params, &handle);
else 
	handle = printDevice;

if(handle == 0) 
	{
	if((line = readStreamLine(&readLineStream, IOchannel)) == NULL)
		return(nilCell);

	return(stuffString(line));
	}

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

return(stuffString(readLineStream.buffer));;
}


CELL * p_currentLine(CELL * params)
{
return(stuffString(readLineStream.buffer));
}


char * getLocalPath(char * fileName)
{
if(my_strnicmp(fileName, "file://", 7) == 0)
	fileName = fileName + 7;

#ifdef WIN_32
if(*fileName == '/' && *(fileName + 2) == ':')
	fileName = fileName + 1;
#endif

return(fileName);
}


int openFile(char * fileName, char * accessMode, char * option)
{
int blocking = 0;

fileName = getLocalPath(fileName);

#ifndef WIN_32
if(option != NULL && *option == 'n')
	blocking = O_NONBLOCK;
#endif

if(*accessMode == 'r')
	return(open(fileName, O_RDONLY | O_BINARY | blocking, 0));

else if(*accessMode == 'w')
#ifdef WIN_32
    return(open( fileName, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, S_IREAD | S_IWRITE) );
#else
    return(open(fileName,O_WRONLY | O_CREAT | O_TRUNC | O_BINARY | blocking, 
        S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR | S_IWGRP | S_IWOTH)); /* rw-rw-rw */
#endif

else if(*accessMode == 'u')
	return(open(fileName, O_RDWR | O_BINARY, 0));

else if(*accessMode == 'a')
       {
#ifdef WIN_32
       return(open(fileName, O_RDWR | O_APPEND | O_BINARY | O_CREAT, S_IREAD | S_IWRITE));
#else
       return(open(fileName, O_RDWR | O_APPEND | O_BINARY | O_CREAT,
          S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR | S_IWGRP | S_IWOTH)); /* rw-rw-rw */
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
return(rename(oldName, newName) == 0 ? trueCell : nilCell);
}


CELL * p_deleteFile(CELL * params)
{
char * fileName;
CELL * result;

params = getString(params, &fileName);
if(my_strnicmp(fileName, "http://", 7) == 0)
	{
	result = getPutPostDeleteUrl(fileName, params, HTTP_DELETE, CONNECT_TIMEOUT);
	return(my_strnicmp((char *)result->contents, (char *)"ERR:", 4) == 0 ? nilCell : trueCell);
	}

fileName = getLocalPath(fileName);
return(unlink(fileName) == 0 ? trueCell : nilCell);
}


CELL * p_makeDir(CELL * params)
{
char * dirString;
mode_t mode;
UINT inMode;

params = getString(params, &dirString);
if(params != nilCell)
	{
	getInteger(params, &inMode);
	mode = inMode;
	}
else
	mode = 0777; /* drwxrwxrwx  gets user masked to drwxr-xr-x on most UNIX */

#ifdef WIN_32 
return(mkdir(dirString) == 0 ? trueCell : nilCell);
#else
return(mkdir(dirString, (mode_t)mode) == 0 ? trueCell : nilCell);
#endif
}


CELL * p_removeDir(CELL * params)
{
char * dirString;

getString(params, &dirString);
return(rmdir(dirString) == 0 ? trueCell : nilCell);
}


CELL * p_changeDir(CELL * params)
{
char * newDir;

getString(params, &newDir);
return(chdir(newDir) == 0 ? trueCell : nilCell);
}

CELL * p_directory(CELL * params)
{
CELL * dirList;
char * dirPath;
char * fileName;
char * pattern = NULL;
UINT options = 0;
DIR * dir;
struct dirent * dEnt;

if(params != nilCell)
	{
	params = getString(params, &dirPath);
	if(params != nilCell)
		{
		params = getString(params, &pattern);
		if(params != nilCell)
			getInteger(params, &options);
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
char * dir;

if(params != nilCell)
	getString(params, &dir);
else dir = DOT_PATH;

if(realpath(dir, path) == NULL)
	return(nilCell);

return(stuffString(path));
}

CELL * p_fileInfo(CELL * params)
{
char * pathName;
struct stat fileInfo;
CELL * list;
int result = 0;

params = getString(params, &pathName);

#ifdef WIN_32 /* has no link-flag */
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
#ifdef USE_WIN_UTF16PATH
*(INT64 *)&((CELL *)list->contents)->aux = fileSize_utf16(pathName);
#else
*(INT64 *)&((CELL *)list->contents)->aux = fileSize(pathName);
#endif /* UTF16PATH */
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
{
int handle;
INT64 size;

#ifndef WIN_32
handle = open(pathName,O_RDONLY | O_BINARY | O_NONBLOCK , 0);
#else
handle = open(pathName,O_RDONLY | O_BINARY, 0);
#endif
size = lseek(handle, 0, SEEK_END);
close(handle);
if(size == -1) size = 0;
return(size);
}
#endif


/* ------------------------- processes and pipes ------------------------- */

#ifndef WIN_32
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

if(fwrite(data, 1, (size_t)size, handle) < strlen(data))
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



#ifdef WIN_32
int winPipe(UINT * inpipe, UINT * outpipe);
UINT winPipedProcess(char * command, int inpipe, int outpipe, int option);
CELL * plainProcess(char * command, size_t size);

CELL * p_pipe(CELL * params)
{
UINT hin, hout;

if(!winPipe(&hin, &hout))    /* see file win32-util.c */
	return(nilCell);

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
	}
else return(plainProcess(command, size));

result = winPipedProcess(command, (int)inpipe, (int)outpipe, (int)option);

if(!result) return(nilCell);

return(stuffInteger(result));
}

CELL * plainProcess(char * command, size_t len)
{
char * cPtr;
char * argv[16];
int idx;

cPtr = callocMemory(len + 1);
memcpy(cPtr, command, len + 1);

init_argv(cPtr, argv);

idx = spawnvp(P_NOWAIT, argv[0], (const char * const *)argv); 

free(cPtr);
if(idx == -1) return(nilCell);

return(stuffInteger(idx));
}


CELL * p_destroyProcess(CELL * params)
{
UINT pid;

getInteger(params, &pid);

/* GetProcessId(handle) */

if(TerminateProcess((HANDLE)pid, 0) == 0)
	return(nilCell);

return(trueCell);
}


#else /* not WIN_32 */

CELL * p_pipe(CELL * params)
{
int handles[2];
if(pipe(handles) != 0)
	return(nilCell);

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


/* ------------------------------------------------------------------------- */


/* Cilk like interface for spawning and syncronizing child processes
   spawn - start child
   sync  - syncronize results
   abort - abort child

   message - share data with chold and parent
*/



void * parentPad = NULL; /* written by parent for this process */
void * thisPad = NULL;   /* written by this process for the parent */

#ifdef HAVE_FORK
#ifndef OS2

typedef struct 
	{
	void * address;
	void * parent;
	void * child;
	int pid;
	SYMBOL * symbolPtr;
	void * next;	
	} SPAWN_LIST;

SPAWN_LIST * mySpawnList = NULL;

void addSpawnedChild(void * addr, void * parent, void * child, int pid, SYMBOL * sPtr)
{
SPAWN_LIST * spawnList;

spawnList = (SPAWN_LIST  *)allocMemory(sizeof(SPAWN_LIST));

spawnList->address = addr;
spawnList->parent = parent;
spawnList->child = child;
spawnList->pid = pid;
spawnList->symbolPtr = sPtr;
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

void purgeSpawnList(void)
{
SPAWN_LIST * spawnList;

/* pop and delete entries */
while(mySpawnList != NULL)
	{
	spawnList = mySpawnList->next;
	free(mySpawnList);
	mySpawnList = spawnList;
	}
}

/* lookup pid get result from shared memory and delete entry */

#define PROCESS_SPAWN_RESULT 0
#define PROCESS_SPAWN_ABORT 1

void processSpawnList(int pid, int mode)
{
SPAWN_LIST * pidSpawn;
SPAWN_LIST * previousSpawn;
CELL * cell;
SYMBOL * sPtr;

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
			cell = readWriteShared(pidSpawn->address, nilCell);
			checkDeleteShareFile(pidSpawn->address);
			sPtr = pidSpawn->symbolPtr;
			deleteList((CELL *)sPtr->contents);
			sPtr->contents = (UINT)cell;
			}
		else /* PROCESS_SPAWN_ABORT */
			{
			kill(pidSpawn->pid, 9);
			waitpid(pidSpawn->pid, (int *)0, 0);
			}

		munmap(pidSpawn->address, pagesize);
		munmap(pidSpawn->parent, pagesize);
		munmap(pidSpawn->child, pagesize);
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
int forkResult;
int pid;
void * address; /* share memory area for result */
void * parent;  /* receive messages from parent here */
void * child;   /* write message for parent here */
SYMBOL * symPtr;

/* make signals processable by waitpid() in p_sync() */
signal(SIGCHLD, SIG_DFL);

if((address = mmap( 0, pagesize, 
	PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0)) == (void*)-1)
		return(nilCell);
if((parent = mmap( 0, pagesize, 
	PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0)) == (void*)-1)
		return(nilCell);
if((child = mmap( 0, pagesize, 
	PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0)) == (void*)-1)
		return(nilCell);

memset(address, 0, sizeof(long));
memset(parent, 0, sizeof(long));
memset(child, 0, sizeof(long));
pid = getpid();

params = getSymbol(params, &symPtr);
if(isProtected(symPtr->flags))
	return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symPtr)));

if((forkResult = fork()) == -1)
	{
	munmap(address, pagesize);
	munmap(parent, pagesize);
	munmap(child, pagesize);
    return(nilCell);
	}

if(forkResult == 0)
    {
	/* seed random generator for message fail delay */
	srandom(getpid()); 
	/* get parentPid, parentPad and thisPad */
	parentPid = pid;
	parentPad = parent; /* to be read by this child */
	thisPad = child;    /* to be written for parent by this child */
	/* purge inherited spawnlist */
	purgeSpawnList();
	/* evaluate and write result to shared memory */
	readWriteShared(address, params);
    exit(0);
    }

addSpawnedChild(address, parent, child, forkResult, symPtr);

return(stuffInteger(forkResult));
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
	pid = waitpid(-1, &result, WNOHANG);
	if(pid) 
		{
		processSpawnList(pid, PROCESS_SPAWN_RESULT);
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
	processSpawnList(pid, PROCESS_SPAWN_ABORT);
	}
else /* abort all */
	{
	while(mySpawnList != NULL) 
		processSpawnList(mySpawnList->pid, PROCESS_SPAWN_ABORT);
	/* put initial behaviour back */
#if defined(SOLARIS) || defined(TRU64) || defined(AIX) 
	setupSignalHandler(SIGCHLD, sigchld_handler);
#else
	setupSignalHandler(SIGCHLD, signal_handler);
#endif
	}

return(trueCell);
}


CELL * p_send(CELL * params)
{
UINT pid;
UINT * address;
SPAWN_LIST * child = NULL;

params = getInteger(params, &pid);

/* write from either the parent or a child */
if(pid == parentPid)
	address = thisPad;
else 
	{
	if((child = getSpawnedChild(pid)) == NULL)
		errorProcExt2(ERR_INVALID_PID, stuffInteger(pid));
	address = child->parent;
	}

/* don't write if not read yet */
if(*address ) 
	{
	/* 50 to 250  micro seconds delay */
	myNanoSleep(50000 + (random() / 10000)); 
	return(nilCell); 
	}

deleteList(readWriteShared(address, params));

return(trueCell);
}



CELL * p_receive(CELL * params)
{
UINT pid;
UINT * address;
SPAWN_LIST * child = NULL;
CELL * cell;
SYMBOL * sPtr;

params = getInteger(params, &pid);
params = getEvalDefault(params, &cell);
if(!symbolCheck)
	return(errorProc(ERR_IS_NOT_REFERENCED));
if(isProtected(symbolCheck->flags))
	return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));
if(symbolCheck->contents != (UINT)cell)
	return(errorProc(ERR_IS_NOT_REFERENCED));

sPtr = symbolCheck;

/* read from either the parent or a child */
if(params == nilCell) 
	{
	if(pid == parentPid)
		address = parentPad;
	else 
		{
		if((child = getSpawnedChild(pid)) == NULL)
			errorProcExt2(ERR_INVALID_PID, stuffInteger(pid));
		address = child->child;
		}
	if(*address == 0) 
		{
		/* 50 to 250  micro seconds delay */
		myNanoSleep(50000 + (random() / 10000)); 
		return(nilCell);
		}
	cell = readWriteShared(address, params);
	/* delete potential nls-file! */
	if(*address == (CELL_STRING | SHARED_MEMORY_EVAL))
		checkDeleteShareFile(address);
	/* don't let read twice the same */ 
	*address = 0; 
	deleteList((CELL *)sPtr->contents);
	sPtr->contents = (UINT)cell;
	pushResultFlag = FALSE;
	}

return(trueCell);
}
#endif /* OS 2 */

#endif /* HAVE_FORK */

/* --------------------------- end Cilk ------------------------------------- */


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

/* ------------------------------ semaphores --------------------------------- */

#ifdef WIN_32

UINT winCreateSemaphore(void);
UINT winWaitSemaphore(UINT hSemaphore);
UINT winSignalSemaphore(UINT hSemaphore, int count);
UINT winDeleteSemaphore(UINT hSemaphore);
int getSemaphoreCount(UINT hSemaphore);

CELL * p_semaphore(CELL * params)
{
UINT sem_id;
long value;

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
#endif

#ifndef WIN_32
/* Mac OS X, Linux/UNIX */

CELL * p_semaphore(CELL * params)
{
long sem, value, result;

if(params == nilCell)
	{
	result = semaphore(0, 0, SEM_CREATE);
	goto SEMAPHORE_END;
	}

params = getInteger(params, (UINT *)&sem);
if(params == nilCell)
	return(stuffInteger((UINT)semaphore(sem, 0, SEM_STATUS)));

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

#ifdef MAC_OSX
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

#ifdef TRU64
                        if(semctl(sem_id, 0, IPC_RMID, 0) == -1)
#else
#ifndef NEWLISP64
                        if(semctl(sem_id, 0, IPC_RMID, &semun_val) == -1)
#else
                        if(semctl(sem_id, 0, IPC_RMID, 0) == -1)
#endif
#endif /* SPARC */

#else

#ifdef MAC_OSX
                        if(semctl(sem_id, 0, IPC_RMID, semu) == -1)
#else
                        if(semctl(sem_id, 0, IPC_RMID, 0) == -1)
#endif
#endif
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
#ifdef MAC_OSX
                return(semctl(sem_id, 0, GETVAL, semu));
#else
                return(semctl(sem_id, 0, GETVAL, 0));
#endif
	}

/* create semaphore */
sem_id = semget(IPC_PRIVATE, 1, 0666 );
#ifdef SPARC

#ifdef TRU64
if(semctl(sem_id, 0, SETVAL, 0) == -1)
#else
#ifndef NEWLISP64
if(semctl(sem_id, 0, SETVAL, &semun_val) == -1)
#else
if(semctl(sem_id, 0, SETVAL, 0) == -1)
#endif
#endif

#else

#ifdef MAC_OSX
if(semctl(sem_id, 0, SETVAL, semu) == -1)
#else
if(semctl(sem_id, 0, SETVAL, 0) == -1)
#endif

#endif
	return(-1);
return(sem_id);
}

#endif  /* Mac OS X, Linux, UNIX */

#ifdef WIN_32
UINT winSharedMemory(int size);
UINT * winMapView(UINT handle, int size);
#endif

#ifndef OS2

/* since 10.1.0 also can share object > pagesize
   in this case transfer is done using /tmp/nls-*
   ot /temp/nls-* files (Win32)
*/

CELL * p_share(CELL * params)
{
void * address;
CELL * cell;
#ifdef WIN_32
UINT handle;
#endif

/* read write  or release (UNIX) shared memory */
if(params != nilCell) 
	{
	cell = evaluateExpression(params);
#ifndef WIN_32
	if(isNil(cell))
		{
		getInteger(params->next, (UINT *)&address);
		if(munmap(address, pagesize) == -1)
			return(nilCell);
		else return(trueCell);
		}
#endif
	getIntegerExt(cell, (UINT *)&address, FALSE);
	params = params->next;
#ifdef WIN_32
	if((address = winMapView((UINT)address, pagesize)) == NULL)
		return(nilCell);
#endif
	return(readWriteShared(address, params));
	}

/* get shared memory UNIX */
#ifndef WIN_32
if((address = (UINT*)mmap(
	0, pagesize, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0)) == (void*)-1)
		return(nilCell);

memset((char *)address, 0, pagesize);
return(stuffInteger((UINT)address));

/* get shared memory WIN_32 */
#else 

if((handle = winSharedMemory(pagesize)) == 0)
	return(nilCell);

if((address = winMapView(handle, pagesize)) == NULL)
	return(nilCell);

memset((char *)address, 0, pagesize);
return(stuffInteger(handle));
#endif
}
#endif /* no OS2 */

CELL * readWriteSharedExpression(UINT * adress, CELL * params);

CELL * readWriteShared(UINT * address, CELL * params)
{
CELL * cell;
size_t size;
char * str;

/* write to shared memory */
if(params != nilCell) 
	{
	cell = evaluateExpression(params);

	/* if a previous share mem file is still present, delete it
       when *address == 0 when called from Cilk then file is
       deleted p_message(). Here only used from p_share() */
	if(*address == (CELL_STRING | SHARED_MEMORY_EVAL))
		checkDeleteShareFile(address);

	/* write list types */
	if((cell->type & COMPARE_TYPE_MASK) > (CELL_STRING & COMPARE_TYPE_MASK))
		return(copyCell(readWriteSharedExpression(address, cell)));

	switch(cell->type)
		{
		case CELL_NIL:
			*address = cell->type;
#ifdef WIN_32
			UnmapViewOfFile(address);
#endif
	        return(nilCell);
		case CELL_TRUE:
			*address = cell->type;
#ifdef WIN_32
			UnmapViewOfFile(address);
#endif
			return(trueCell);
		case CELL_LONG:
			*(address + 1) = sizeof(long);
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
			if(size > (pagesize - 3 * sizeof(long)))
				return(copyCell(readWriteSharedExpression(address, cell)));

			*(address + 1) = size;
			memcpy((char *)(address + 2), str, size);
			*((char *)address + 2 * sizeof(long) + size) = 0;
			break;
		default:
			return(errorProcExt(ERR_ILLEGAL_TYPE, cell));
		}

	*address = cell->type;
#ifdef WIN_32
	UnmapViewOfFile(address);
#endif
	return(copyCell(cell));
	}

/* read shared memory */
switch(*address & RAW_TYPE_MASK)
	{
	case CELL_NIL:
#ifdef WIN_32 
		UnmapViewOfFile(address);
#endif
		return(nilCell);
	case CELL_TRUE:
#ifdef WIN_32 
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
		if(*address & SHARED_MEMORY_EVAL)
			return(readWriteSharedExpression(address, nilCell));
		size = *(address + 1);
		cell = makeStringCell(allocMemory(size + 1), size);
		memcpy((char *)cell->contents, (char*)(address + 2), cell->aux);
		break;
	default:
		return(nilCell);
	}

#ifdef WIN_32
		UnmapViewOfFile(address);
#endif
return(cell);
}

/* Takes anyhting and passes as string or file which has to
   be compiled back into expression when reading

   returns a new cell object on read, old on write
*/

CELL * readWriteSharedExpression(UINT * address, CELL * params)
{
size_t size;
CELL * cell;
char * buffer = NULL;

/* read */
if(params == nilCell)
	{
	size = *(address + 1);
	if(size < (pagesize - 3 * sizeof(long) ))
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
buffer = cellToString(params, &size, TRUE);
*(address + 1) = size;

if(size < pagesize - 3 * sizeof(long))
	{
	memcpy((char *)(address + 2), buffer, size);
	*((char *)address + 2 * sizeof(long) + size) = 0;
	}
else
	{
	checkDeleteShareFile(address);
	memset((char *)(address + 2), 0, pagesize - 2 * sizeof(long));
#ifndef WIN_32
	strncpy((char *)(address + 2), "/tmp/nls-", 10);
	getUUID((char *)(address + 2) + 9, 0);
#else
	strncpy((char *)(address + 2), "/temp/nls-", 11);
	getUUID((char *)(address + 2) + 10, 0);
#endif
	writeFile((char *)(address + 2), buffer, size, "w");
	}

*address = (CELL_STRING | SHARED_MEMORY_EVAL);
return(params);
}

void checkDeleteShareFile(UINT * address)
{
if( 	(*address == (CELL_STRING | SHARED_MEMORY_EVAL)) &&
#ifndef WIN_32
	 	(strncmp((char *)(address + 2), "/tmp/nls-", 9) == 0) &&
		(strlen((char *)(address + 2)) == 45) )
#else
	 	(strncmp((char *)(address + 2), "/temp/nls-", 10) == 0) &&
		(strlen((char *)(address + 2)) == 46) )
#endif
	unlink((char *)(address + 2)); 
}

/* ------------------------------ time and date functions -------------------- */
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


CELL * p_date(CELL * params)
{
time_t t;
struct timeval tv;
struct tm * ltm;
char * ct;
char * fmt;
ssize_t offset;
time_t tme;

#ifdef SUPPORT_UTF8
#ifdef WCSFTIME
int * ufmt;
int * timeString;
int size;
#endif
char * utf8str;
#else
char * timeString;
#endif

if(params == nilCell)
	{
	gettimeofday(&tv, NULL);
	t = tv.tv_sec;
	}
else
	{
	params = getInteger(params, (UINT *)&tme);
	t = tme;
	
	if(params != nilCell)
		{
		params = getInteger(params, (UINT *)&offset);
	        t += (int)offset * 60;
		}
		
	if(params != nilCell)
		{
		params = getString(params, &fmt);
		ltm = localtime(&t);
#ifdef SUPPORT_UTF8
	/* some Linux do UTF-8 but don't have wcsftime() or it is buggy */
#ifdef WCSFTIME
		size = utf8_wlen(fmt);
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

gettimeofday(&tv, NULL);

ttm = localtime((time_t *)&tv.tv_sec);

return (ttm->tm_hour * 3600000000LL + 
       ttm->tm_min * 60000000 + ttm->tm_sec * 1000000 + 
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

#ifndef WIN_32
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
    getInteger64(params->next, &N);

resultIdxSave = resultStackIdx;
while(N--)  
    {
    evaluateExpression(params);
    cleanupResults(resultIdxSave);
    }

gettimeofday(&end, NULL);

diff = (1.0 * timediff64_us(end, start)) / 1000;
return(stuffFloat(&diff));
}


CELL * p_timeOfDay(CELL * params)
{
double microSecs = microSecTime()/1000.0;
return(stuffFloat(&microSecs));
}


CELL * p_now(CELL * params)
{
struct timeval tv;
struct tm *ttm;
#ifndef WIN_32
struct tm *ltm;
#ifndef SUNOS
#ifndef OS2
long gmtoff;
UINT isdst;
#endif
#endif
#else
TIME_ZONE_INFORMATION timeZone;
#endif
ssize_t offset = 0;
CELL * cell;

gettimeofday(&tv, NULL); /* get secs and microsecs */

if(params != nilCell)
	{
	params = getInteger(params, (UINT*)&offset);
	offset *= 60;
        tv.tv_sec += offset;
	}

#ifndef WIN_32
ltm = localtime((time_t *)&tv.tv_sec);
#ifndef SUNOS
#ifndef OS2
isdst = ltm->tm_isdst;
#ifdef CYGWIN
gmtoff = _timezone/60;
#else
gmtoff = - ltm->tm_gmtoff/60 + isdst * 60;
#endif
#endif
#endif
#else
GetTimeZoneInformation(&timeZone);
#endif

ttm = gmtime((time_t *)&tv.tv_sec);

cell = stuffIntegerList(
    11,
    (UINT)ttm->tm_year + 1900,
    (UINT)ttm->tm_mon + 1,
    (UINT)ttm->tm_mday,
    (UINT)ttm->tm_hour,
    (UINT)ttm->tm_min,
    (UINT)ttm->tm_sec,
    (UINT)tv.tv_usec,
    (UINT)ttm->tm_yday,
    ((UINT)ttm->tm_wday == 0 ? 7 : (UINT)ttm->tm_wday),

#if defined(MAC_OSX) || defined(LINUX) || defined(_BSD) || defined(CYGWIN)
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

#if defined(WIN_32)
     timeZone.Bias,
     timeZone.DaylightBias	
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
CELL * cell;

params = getInteger(params, (UINT*)&timeValue);
ttm = gmtime((time_t *)&timeValue);

cell = stuffIntegerList(
    8,
    (UINT)ttm->tm_year + 1900,
    (UINT)ttm->tm_mon + 1,
    (UINT)ttm->tm_mday,
    (UINT)ttm->tm_hour,
    (UINT)ttm->tm_min,
    (UINT)ttm->tm_sec,
    (UINT)ttm->tm_yday,
    ((UINT)ttm->tm_wday == 0 ? 7 : (UINT)ttm->tm_wday)
);

if(params != nilCell)	
	{
	pushResult(cell);
	return(copyCell(implicitIndexList(cell, params)));
	}

return(cell);
}


CELL * p_dateValue(CELL * params)
{
struct timeval tv;
ssize_t year, month, day, hour, min, sec;
time_t dateValue;

if(params->type == CELL_NIL)
	{
	gettimeofday(&tv, NULL);
	return(stuffInteger(tv.tv_sec));
	}

params = getInteger(params, (UINT *)&year);
params = getInteger(params, (UINT *)&month);
params = getInteger(params, (UINT *)&day);

if(year < 1970) return(stuffInteger(0));

hour = min = sec = 0;
if(params != nilCell)
        {
        params = getInteger(params, (UINT *)&hour);
        params = getInteger(params, (UINT *)&min);
		getInteger(params, (UINT *)&sec);
        }

dateValue = calcDateValue(year, month, day, hour, min, sec);

#ifndef NEWLISP64
return(stuffInteger64((INT64)dateValue));
#else
return(stuffInteger((UINT)dateValue));
#endif
}


time_t calcDateValue(int year, int month, int day, int hour, int min, int sec)
{
time_t dateValue;

dateValue = 367 * year - (7 * (year + ((month + 9) / 12)))/4 
            + (275 * month)/9 + day + 1721013;

dateValue = dateValue * 24 * 3600 + hour * 3600 + min * 60 + sec 
            - 413319296; /* correction for 1970-1-1 */

#ifdef NEWLISP64
dateValue = dateValue & 0xffffffff;
#endif

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

#ifdef WIN_32
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

return(stuffFloat(&milliSecsFloat));
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

#ifdef MAC_OSX
#ifdef LIBRARY
return(envList);
#endif
#endif

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
CELL * p_readKey(CELL * params)
{
#if defined(WIN_32) || defined(OS2)
return(stuffInteger(getch()));
#else

struct termios term, oterm;
char c = 0;

tcgetattr(0, &oterm);

memcpy(&term, &oterm, sizeof(term));

/* put the terminal in non-canonical mode, any
reads timeout after 0.1 seconds or when a
single character is read */
term.c_lflag &= ~(ICANON | ECHO);
term.c_cc[VMIN] = 0;
term.c_cc[VTIME] = 1;
tcsetattr(0, TCSANOW, &term);

#if defined(_BSD) || defined(MAC_OSX)
while(read(0, &c, 1) == 0);
#else
while((c = (char)getchar()) == (char)-1);
#endif

/* reset the terminal to original state */
tcsetattr(0, TCSANOW, &oterm);

return(stuffInteger(c));
#endif
} 

/* --------------------- peek a file descriptor ------------------------------*/

#ifndef WIN_32
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
ssize_t size = MAX_STRING;
ssize_t pSize;

while(TRUE)
	{
	*buffer = allocMemory(size + 2);
	pSize = vsnprintf(*buffer, size + 1, format, argptr);

#if defined(TRU64)
	if(pSize < 0)
		{
		freeMemory(*buffer);
		size = size + size / 2;
		continue;
		}
#else
	if(pSize > size)
		{
		freeMemory(*buffer);
		size = pSize;    
		continue;
		}
#endif
	break;
	}

return((int)pSize);
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
	unsigned char 	node[6];
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

#ifdef WIN_32
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
#ifdef WIN_32
	uuid.time_low |= (random() << 16);
#endif
	uuid.time_mid = random();
	uuid.time_hi_and_version = random();
	}
else
	{
	uuid.time_low = (unsigned long)(timestamp & 0xFFFFFFFF);
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

