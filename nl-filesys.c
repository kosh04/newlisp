/* nl-filesys.c --- I/O process control, date/time - functions for newLISP


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

#include "newlisp.h"
#include <errno.h>
#include "protos.h"

#ifdef SOLARIS
#include <stropts.h>
#ifndef TRU64
#define FIONREAD I_NREAD
#endif
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

#endif

#ifndef WIN_32
#include <sys/socket.h>
#define SOCKET_ERROR -1
#define INVALID_SOCKET -1
#endif

#ifdef LINUX
char * strptime(const char * str, const char * fmt, struct tm * ttm);
#endif

size_t calcDateValue(int year, int month, int day, int hour, int min, int sec);
extern STREAM readLineStream;
extern FILE * IOchannel;
extern int pagesize;

extern char * errorMessage[];
extern STREAM errorStream;

/* used for shared memory in p_share and p_spaw, p_sync */
union num_ptr {
	UINT num;
	UINT * ptr;
};

/* semaphore() function type */
#define SEM_CREATE 0
#define SEM_STATUS 1
#define SEM_SIGNAL 2


CELL * p_isFile(CELL * params) /* includes dev,socket,dir,file etc. */
{
char * fileName;

getString(params, &fileName);
if(isFile(fileName) == 0)
	return(trueCell);

return(nilCell);
}

int isFile(char * fileName)
{
struct stat fileInfo;


#ifdef USE_WIN_UTF16PATH
return(stat_utf16(fileName, &fileInfo));
#else
return(stat(fileName, &fileInfo));
#endif
}

CELL * p_isDirectory(CELL * params)
{
char * fileName;

getString(params, &fileName);
if(isDir(fileName)) return(trueCell);
return(nilCell); 
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
params = getSymbol(params, &readSptr);
params = getInteger(params, (UINT *)&size);

if(isProtected(readSptr->flags))
	return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(readSptr)));

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
	
if(bytesRead == 0) 
	{ 
	deleteList((CELL *)readSptr->contents);
	readSptr->contents = (UINT)nilCell;
	closeStrStream(&stream); 
	return(nilCell);
	} 

/*
strCell = getCell(CELL_STRING);
stream.buffer = reallocMemory(stream.buffer, bytesRead +1);
strCell->aux = bytesRead + 1;
strCell->contents = (UINT)stream.buffer;
*/

stream.buffer = reallocMemory(stream.buffer, bytesRead +1);
strCell = makeStringCell(stream.buffer, bytesRead);

deleteList((CELL *)readSptr->contents);
readSptr->contents = (UINT)strCell;

if(found)
	return(stuffInteger(bytesRead));
return(nilCell);
}

CELL * p_readFile(CELL * params)
{
char * fileName;
char * buffer = NULL;
ssize_t size;

params = getString(params, &fileName);
if(my_strnicmp(fileName, "http://", 7) == 0)
	return(getPutPostDeleteUrl(fileName, params, HTTP_GET_URL, 0));

if(my_strnicmp(fileName, "file://", 7) == 0)
	fileName = fileName + 7;

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


CELL * p_writeBuffer(CELL * params)
{
UINT handle;
ssize_t bytesWritten;
size_t size;
char * buffer;
CELL * strCell;
SYMBOL * writeSptr;
CELL * cell;
CELL * flagPtr = NULL;

cell = evalCheckProtected(params, &flagPtr);

params = params->next;

if(isNumber(cell->type))
	{
	getIntegerExt(cell, &handle, FALSE); 
	cell = NULL;
	}
else if(cell->type != CELL_STRING) 
	return(errorProc(ERR_NUMBER_OR_STRING_EXPECTED));

strCell = evaluateExpression(params);
if(strCell->type == CELL_SYMBOL)
    {
    writeSptr = (SYMBOL *)strCell->contents;
    strCell = (CELL *)writeSptr->contents;
    }
else if(strCell->type == CELL_DYN_SYMBOL)
    {
    writeSptr = getDynamicSymbol(strCell);
    strCell = (CELL *)writeSptr->contents;
    }

if(strCell->type != CELL_STRING)
	return(errorProcExt(ERR_STRING_EXPECTED, params));

if(params->next == nilCell)
	size = strCell->aux - 1;
else       
	getInteger(params->next, (UINT *)&size);

buffer = (char *)strCell->contents;
if(size > (strCell->aux - 1)) size = strCell->aux - 1;

if(cell != NULL)
	{
	if(flagPtr) return(errorProcExt(ERR_SYMBOL_PROTECTED, flagPtr));
	return(stuffInteger(appendCellString(cell, buffer, size)));
	}

if((bytesWritten = write((int)handle, buffer, size)) == (UINT)-1)
	return(nilCell);

return(stuffInteger(bytesWritten));
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

params = getString(params, &fileName);

if(my_strnicmp(fileName, "http://", 7) == 0)
	return(getPutPostDeleteUrl(fileName, params, 
				(*type == 'w') ? HTTP_PUT_URL : HTTP_PUT_APPEND_URL, 0));

if(my_strnicmp(fileName, "file://", 7) == 0)
	fileName = fileName + 7;

getStringSize(params, &buffer, &size, TRUE);

if(writeFile(fileName, buffer, size, type) == (int)-1)
	return(nilCell);

return(stuffInteger(size));
}


CELL * p_writeLine(CELL * params)
{
char * buffer;
UINT handle;
CELL * flagPtr = NULL;
size_t size;

if(params->type == CELL_NIL)
	buffer = readLineStream.buffer;
else
	params = getStringSize(params, &buffer, &size, TRUE);

if(params != nilCell)
	{
	params = evalCheckProtected(params, &flagPtr);
	if(isNumber(params->type))
		{
		getIntegerExt(params, &handle, FALSE);
		if(write((int)handle, buffer, strlen(buffer)) == -1) return(nilCell);
		if(write((int)handle, LINE_FEED, strlen(LINE_FEED)) == -1) return(nilCell);
		}
	if(params->type == CELL_STRING)
		{
    	if(flagPtr) return(errorProcExt(ERR_SYMBOL_PROTECTED, flagPtr));
		appendCellString(params, buffer, size);
		appendCellString(params, LINE_FEED, strlen(LINE_FEED));
		}
	}
else
	{
	varPrintf(OUT_DEVICE, "%s", buffer);
	varPrintf(OUT_DEVICE, LINE_FEED);
	}

return(stuffString(buffer));
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


int openFile(char * fileName, char * accessMode, char * option)
{
int blocking = 0;

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
	write(toHandle, copyBuffer, (int)bytesRead);
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

params = getString(params, &fileName);
if(my_strnicmp(fileName, "http://", 7) == 0)
	return(getPutPostDeleteUrl(fileName, params, HTTP_DELETE_URL, 0));

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
	if(pattern)
		{
		if(searchBufferRegex(fileName, 0, pattern, strlen(fileName), options, NULL) == -1)
			continue;
		}
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
#ifdef USE_WIN_UTF16PATH
WCHAR * pathName;
struct _stat fileInfo;
#else
char * pathName;
struct stat fileInfo;
#endif

CELL * list;

#ifdef USE_WIN_UTF16PATH
char * utf8pathName;
params = getString(params, &utf8pathName);
pathName = utf8_to_utf16(utf8pathName);
#else
params = getString(params, &pathName);
#endif

if(lstat(pathName, &fileInfo) != 0)
	{
#ifdef USE_WIN_UTF16PATH
	free(pathName);
#endif
	return(nilCell);
	}

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
*(INT64 *)&((CELL *)list->contents)->aux = fileSizeW(pathName);
#else
*(INT64 *)&((CELL *)list->contents)->aux = fileSize(pathName);
#endif /* UTF16PATH */
#endif /* LFS */
#endif /* NEWLISP64 */

#ifdef USE_WIN_UTF16PATH
free(pathName);
#endif

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


CELL * p_system(CELL *params)
{
char * command;
getString(params, &command);
return(stuffInteger((UINT)system(command)));
}


CELL * p_exec(CELL * params)
{
char * command, * data;

params = getString(params, &command);
if(params == nilCell)
    return(readProcess(command));    
    
getString(params, &data);
return(writeProcess(command, data));
}


CELL * readProcess(char * command)
{
CELL * lineList;
FILE * handle;
char * line;

if((handle = popen(command , "r")) == NULL)
	return(nilCell);

lineList = getCell(CELL_EXPRESSION);
while((line = readStreamLine(&readLineStream, handle)) != NULL)
	addList(lineList, stuffString(line));

pclose(handle);

return(lineList);
}


CELL * writeProcess(char * command, char * data)
{
FILE * handle;

if((handle = popen(command, "w")) == NULL)
	return(nilCell);

if(fwrite(data, sizeof(char), strlen(data), handle) < strlen(data))
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

#ifdef OLD_PP
argv[0] = cPtr;
for(idx = 1; idx < 5; idx++)
    {
    cPtr = strchr(cPtr, ' ');
    if(cPtr == NULL) break;
    while(*cPtr == ' ') *cPtr++ = 0;
    argv[idx] = cPtr;
    }
argv[idx] = NULL;
#endif

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

return(stuffInteger(forkResult));
}

CELL * p_fork(CELL * params)
{
int forkResult;

if((forkResult = fork()) == -1)
    return(nilCell);
if(forkResult == 0)
    {
    evaluateExpression(params);
    exit(0);
    }

return(stuffInteger(forkResult));
}

/* Cilk like interface for spawning and syncronizing child processes */
#ifdef HAVE_FORK
typedef struct 
	{
	void * address;
	int pid;
	SYMBOL * symbolPtr;
	void * next;	
	} SPAWN_LIST;

SPAWN_LIST * mySpawnList = NULL;

void addSpawnedChild(void * addr, int pid, SYMBOL * sPtr)
{
SPAWN_LIST * spawnList;

spawnList = (SPAWN_LIST  *)allocMemory(sizeof(SPAWN_LIST));

spawnList->address = addr;
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
char * buffer = NULL;
ssize_t size;

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
			if(*(char *)pidSpawn->address)
				{
				/* xlate but don't evaluate shared memory and no callback */
				cell = sysEvalString((char *)pidSpawn->address + 4, nilCell, 
							currentContext, READ_XLATE_NO_CALLBACK);
				}
			else /* shared address contains filename with result */
				{
				if((size = readFile((char *)pidSpawn->address + 4, &buffer)) != -1)
					cell = sysEvalString(buffer, nilCell, 
							currentContext, READ_XLATE_NO_CALLBACK);
				else cell = nilCell;
				unlink((char *)pidSpawn->address + 4);
				}

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
        free((char *)pidSpawn);
        break;
        }
    previousSpawn = pidSpawn;
    pidSpawn = pidSpawn->next;
    }
}


/* spawn (fork) a process and assign result to the symbol given
     (spawn <quoted-symbol> <epxression>) => pid
   creates a memory share and passes it to the spaned process
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
void * address;
SYMBOL * symPtr;
CELL * cell;
STREAM strStream;
int errNo;

/* make signals processable by waitpid() in p_sync() */
signal(SIGCHLD, SIG_DFL);

if((address = mmap( 0, pagesize, 
	PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0)) == (void*)-1)
		return(nilCell);

params = getSymbol(params, &symPtr);
if(isProtected(symPtr->flags))
	return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symPtr)));

if((forkResult = fork()) == -1)
	{
	munmap(address, pagesize);
    return(nilCell);
	}

if(forkResult == 0)
    {
	purgeSpawnList(); /* purge inherited spawnlist */
	cell = evaluateExpressionSafe(params, &errNo);
	if(cell == NULL)
		cell = stuffString(errorStream.buffer);
	openStrStream(&strStream, MAX_STRING, 0);
	prettyPrintFlags |= PRETTYPRINT_STRING;
 	printCell(cell , TRUE, (UINT)&strStream);
	if(strStream.position < pagesize + 4)
		{
		memset(address, 0xf, 4);
		memcpy((char *)address + 4, strStream.buffer, strStream.position);
		}
	else
		{
		memset(address, 0, 64);
		strncpy((char *)address + 4, "/tmp/nls-", 10);
		getUUID((char *)address + 9, 0);
		writeFile((char *)address + 4, strStream.buffer, strStream.position, "w");
		}
    exit(0);
    }

addSpawnedChild(address, forkResult, symPtr);

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
int resultIdxSave;
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
	if(timediff(tp, tv) > timeout) return(nilCell);
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
#ifdef SOLARIS
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
#ifdef SOLARIS
	setupSignalHandler(SIGCHLD, sigchld_handler);
#else
	setupSignalHandler(SIGCHLD, signal_handler);
#endif
	}

return(trueCell);
}

#endif /* HAVE_FORK */


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
                return(semctl(sem_id, 0, GETVAL));
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
UINT handle;
#endif

#ifndef OS2
CELL * p_share(CELL * params)
{
union num_ptr address;
CELL * cell;
size_t size;
char * str;

if(params != nilCell)
	{
	cell = evaluateExpression(params);
#ifndef WIN_32
	if(isNil(cell))
		{
		getInteger(params->next, &address.num);
		if(munmap((void *)address.ptr, pagesize) == -1)
			return(nilCell);
		else return(trueCell);
		}
#endif
	getIntegerExt(cell, &address.num, FALSE);
	params = params->next;
#ifdef WIN_32
	if((address.ptr = winMapView(address.num, pagesize)) == NULL)
		return(nilCell);
#endif
	if(params != nilCell) /* write to shared memory */
		{
		cell = evaluateExpression(params);
       	if(cell->type == CELL_NIL)
	        {
	        *address.ptr = CELL_NIL;
#ifdef WIN_32
			UnmapViewOfFile(address.ptr);
#endif
	        return(nilCell);
	        }
        if(cell->type == CELL_TRUE)
            {
            *address.ptr = CELL_TRUE;
#ifdef WIN_32
			UnmapViewOfFile(address.ptr);
#endif
            return(trueCell);
            }
		if(cell->type == CELL_STRING)
			{
			getStringSize(cell, &str, &size, FALSE);
			if(size > (pagesize - 2 * sizeof(long) - 1)) 
				size = pagesize - 2 * sizeof(long) - 1;
			*address.ptr = cell->type;
			*(address.ptr + 1) = size;
			memcpy((char *)(address.num + 2 * sizeof(long)), str, size);
			*(char *)(address.num + 2 * sizeof(long) + size) = 0;
			/* fall thru to address.ptr == CELL_STRING, to return sized string */

			goto return_new_string_cell;
			}
		if(cell->type == CELL_LONG)
			{
			*address.ptr = cell->type;
			*(address.ptr + 1) = sizeof(long);
			*(address.ptr + 2) = cell->contents;
#ifdef WIN_32
			UnmapViewOfFile(address.ptr);
#endif
			return(copyList(cell));
			}
#ifndef NEWLISP64
		if(cell->type == CELL_INT64)
			{
			*address.ptr = cell->type;
			*(address.ptr + 1) = sizeof(INT64);
			memcpy(address.ptr + 2, (void *)&cell->aux, sizeof(INT64));
#ifdef WIN_32
			UnmapViewOfFile(address.ptr);
#endif
			return(copyList(cell));
			}
		if(cell->type == CELL_FLOAT)
			{
			*address.ptr = cell->type;
			*(address.ptr + 1) = sizeof(double);
			*(address.ptr + 2) = cell->aux;
			*(address.ptr + 3) = cell->contents;
#ifdef WIN_32
			UnmapViewOfFile(address.ptr);
#endif
			return(copyList(cell));
			}

#else /* NEWLISP64 */
		if(cell->type == CELL_FLOAT)
			{
			*address.ptr = cell->type;
			*(address.ptr + 1) = sizeof(double);
			*(address.ptr + 2) = cell->contents;
			return(copyList(cell));
			}
#endif /* NEWLISP64 */
		return(errorProcExt(ERR_ILLEGAL_TYPE, cell));
		}
	if(*address.ptr == CELL_NIL) /* rrad from share memory */
		{
#ifdef WIN_32 
		UnmapViewOfFile(address.ptr);
#endif
        return(nilCell);
        }
    if(*address.ptr == CELL_TRUE)
      	{
#ifdef WIN_32 
		UnmapViewOfFile(address.ptr);
#endif
        return(trueCell);	
        }
	if(*address.ptr == CELL_LONG)
		{
		cell = stuffInteger(*(address.ptr + 2));
#ifdef WIN_32 
		UnmapViewOfFile(address.ptr);
#endif
		return(cell);
		}
#ifndef NEWLISP64
	if(*address.ptr == CELL_INT64)
		{
		cell = stuffInteger64(*(INT64 *)(address.ptr + 2));
#ifdef WIN_32 
		UnmapViewOfFile(address.ptr);
#endif
		return(cell);
		}
#endif
		
	if(*address.ptr == CELL_FLOAT)
		{
#ifndef NEWLISP64
		cell = getCell(CELL_FLOAT);
		cell->aux = *(address.ptr + 2);
		cell->contents = *(address.ptr + 3);
#else
		cell = getCell(CELL_FLOAT);
		cell->contents = *(address.ptr + 2);
#endif
#ifdef WIN_32
		UnmapViewOfFile(address.ptr);
#endif
		return(cell);
		}
	if(*address.ptr == CELL_STRING)
		{
return_new_string_cell:
		cell = makeStringCell(allocMemory(cell->aux), *(address.ptr + 1));

		memcpy((char *)cell->contents, (char*)(address.num + 2 * sizeof(long)), cell->aux);
#ifdef WIN_32
		UnmapViewOfFile(address.ptr);
#endif
		return(cell);
		}
	return(nilCell);
	}

#ifndef WIN_32
if((address.ptr = (UINT*)mmap(
	0, pagesize, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0)) == (void*)-1)
		return(nilCell);

memset((char *)address.num, 0, pagesize);
return(stuffInteger(address.num));

#else
if((handle = winSharedMemory(pagesize)) == 0)
	return(nilCell);

if((address.ptr = winMapView(handle, pagesize)) == NULL)
	return(nilCell);

memset((char *)address.num, 0, pagesize);
return(stuffInteger(handle));
#endif
}
#endif /* no OS2 */

/* ------------------------------ time and date functions -------------------- */

CELL * p_systemInfo(CELL * params)
{
CELL * cell;

cell = stuffIntegerList(
	9,
	cellCount,
	MAX_CELL_COUNT,
	symbolCount,
	(UINT)recursionCount,
	(UINT)envStackIdx,
	(UINT)MAX_CPU_STACK,
	(UINT)getpid(),
	(UINT)version,
	(UINT)opsys);

if(params != nilCell)	
	{
	pushResult(cell);
	return(copyCell(implicitIndexList(cell, params)));
	}

return(cell);
}


CELL * p_systemError(CELL * params)
{
UINT init;

if(params != nilCell)
	{
	getInteger(params, (UINT*)&init);
	errno=(int)init;
	}

return(stuffInteger((UINT)errno));
}


CELL * p_date(CELL * params)
{
time_t t;
struct timeval tv;
struct tm * ltm;
char * ct;
char * fmt;
ssize_t offset;
ssize_t tme;

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


int milliSecTime(void)
{
struct timeval tv;
struct tm * ttm;

gettimeofday(&tv, NULL);

ttm = localtime((time_t *)&tv.tv_sec);

return (ttm->tm_hour * 3600000 + 
       ttm->tm_min * 60000 + ttm->tm_sec * 1000 + 
       tv.tv_usec/1000);
}


/* returns a differerence of 2 timeval structs in milliseconds
*/
int timediff(struct timeval out, struct timeval in )
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
UINT64 timediff64(struct timeval out, struct timeval in )
{
	if( (out.tv_usec -= in.tv_usec) < 0 )   {
		out.tv_sec--;
		out.tv_usec += 1000000;
	}
	out.tv_sec -= in.tv_sec;

return(out.tv_sec*1000000 + out.tv_usec);
}

#ifndef WIN_32
CELL * p_parseDate(CELL * params)
{
struct tm ttm;
char * dateStr;
char * formatStr;
size_t dateValue;

params = getString(params, &dateStr);
params = getString(params, &formatStr);

memset (&ttm, 0, sizeof (ttm));

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
int resultIdxSave;

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
return(stuffInteger((UINT)timediff(end, start)));
}


CELL * p_timeOfDay(CELL * params)
{
return(stuffInteger(milliSecTime()));
}


CELL * p_now(CELL * params)
{
struct timeval tv;
struct tm *ttm;
struct tm *ltm;
struct timezone tzp;
ssize_t offset;

gettimeofday(&tv, &tzp);

if(params != nilCell)
	{
	getInteger(params, (UINT*)&offset);
	offset *= 60;
        tv.tv_sec += offset;
	}

ltm = localtime((time_t *)&tv.tv_sec);

ttm = gmtime((time_t *)&tv.tv_sec);

return(stuffIntegerList(
    11,
    (UINT)ttm->tm_year + 1900,
    (UINT)ttm->tm_mon + 1,
    (UINT)ttm->tm_mday,
    (UINT)ttm->tm_hour,
    (UINT)ttm->tm_min,
    (UINT)ttm->tm_sec,
    (UINT)tv.tv_usec,
    (UINT)ttm->tm_yday + 1,
    (UINT)ttm->tm_wday + 1,
/* Note, that on SOLARIS tzp.tz_minuteswest and
   tzp.tz_dsttime might not work correctly 
   and contain garbage 
*/
    (UINT)tzp.tz_minuteswest,
#ifdef WIN_32
/*    (UINT)ltm->tm_isdst */
    (UINT)tzp.tz_dsttime 
#else
    (UINT)ltm->tm_isdst
#endif
    ));
}


UINT seconds(void)
{
struct timeval tv;

gettimeofday(&tv, NULL);
return(tv.tv_sec);
}


CELL * p_dateValue(CELL * params)
{
struct timeval tv;
ssize_t year, month, day, hour, min, sec;
size_t dateValue;

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


size_t calcDateValue(int year, int month, int day, int hour, int min, int sec)
{
size_t dateValue;

dateValue = 367 * year - (7 * (year + ((month + 9) / 12)))/4 
            + (275 * month)/9 + day + 1721013;

dateValue = dateValue * 24 * 3600 + hour * 3600 + min * 60 + sec 
            - 413319296; /* correction for 1970-1-1 */

#ifdef NEWLISP64
dateValue = dateValue % 0x80000000;
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


CELL * p_sleep(CELL * params)
{
size_t milliSecs;

getInteger(params, (UINT *)&milliSecs);

mySleep(milliSecs);

return(stuffInteger((UINT)milliSecs));
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
	if(lastEntry == NULL)
		{
		lastEntry = stuffString(*env);
		envList->contents = (UINT)lastEntry;
		}
	else
		{
		lastEntry->next = stuffString(*env);
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
#if defined  (WIN_32) || (OS2)
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

#if defined  (_BSD) || (MAC_OSX)
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
#ifdef WIN_32
unsigned long result;
#else
int result;
#endif

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

#if defined(WIN_32) || defined(TRU64)
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


/* eof */

