/* win32-path.c - directory and file functions working on UTF-16
   file and path names

    Copyright (C) 2007,2008 Michael Sabin

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

#include <wchar.h>
#include <windows.h>
#include "newlisp.h"
#include "protos.h"

/*
utf8_to_utf16
Uses the Windows API to convert a UTF-8 string to a UTF-16 string.
Returns a pointer to a WCHAR string, or NULL if there was an error
(like if an invalid utf8 string was provided).
Note that the returned string must be free()ed.
*/
WCHAR * utf8_to_utf16(const char *utf8str)
{
	int size = -1;
	WCHAR *utf16str = NULL;

	size = MultiByteToWideChar(
		CP_UTF8,
		0,			/* no flags=ignore errors if possible */
		utf8str,
		-1,			/* read until NULL character */
		NULL,		
		0			/* just calculate size */
	);
	
	if (size == 0) return (NULL);
	
	utf16str = (WCHAR*)allocMemory((size+1) * sizeof(WCHAR));
	
	size = MultiByteToWideChar(
		CP_UTF8,
		0,
		utf8str,
		-1,
		utf16str,
		size
	);	
	
	if (size == 0) 
	{
		free(utf16str);
		return(NULL);
	}
	else
		return(utf16str);
}


/*
utf16_to_utf8ptr
Used in win32_realpath.
Generally wrapped by utf16_to_utf8.
*/
int utf16_to_utf8ptr(const WCHAR *utf16str, char * utf8str, int size)
{
	
	if (size < 1) return(-1);
	
	size = WideCharToMultiByte(
		CP_UTF8,
		0,
		utf16str,
		-1,
		utf8str,
		size,
		NULL,
		NULL
	);
	
	if (size == 0) 
		return(-1);
	else
		return(0);
}

/*
utf16_to_utf8
Uses the Windows API to convert a UTF-16 string to a UTF-8 string.
Returns a pointer to a char string, or NULL if there was an error
(like if an invalid utf16 string was provided).
Note that the returned string must be free()ed.
*/
char * utf16_to_utf8(const WCHAR *utf16str)
{
	int size = -1;
	char *utf8str = NULL;
	
	size = WideCharToMultiByte(
		CP_UTF8,
		0,
		utf16str,
		-1,
		0,
		0,
		NULL,
		NULL
	);
	
	if (size == 0) return (NULL);	
	
	utf8str = (char*)allocMemory((size+1) * sizeof(char));
	
	if (utf16_to_utf8ptr(utf16str, utf8str, size) == -1)
	{
		free(utf8str);
		return(NULL);
	}
	else
		return(utf8str);
}


/* 
win32_realpath
Identical interface as realpath
for both ANSI and UTF-16 path names on Windows.
Has the following functional differences
* If GetFullPathNameW, first converts the UTF-8 file name to UTF-16 (WCHAR)
* Uses GetFullPathNameA or GetFullPathNameW to receive the char/WCHAR path
* If GetFullPathNameW, converts the UTF-16 WCHAR path back to UTF-8
*/
char *win32_realpath(const char *filepath, char *realpath)
{

#ifdef USE_WIN_UTF16PATH
	
	WCHAR * utf16filepath;
	WCHAR utf16realpath[MAX_PATH + 2];
	int err;
	
	utf16filepath = utf8_to_utf16(filepath);
	err = GetFullPathNameW(utf16filepath, MAX_PATH, utf16realpath, NULL);
	free(utf16filepath);
	
	if (err == 0)
		return(NULL);
	
	if (utf16_to_utf8ptr(utf16realpath, realpath, MAX_PATH) == -1)
		return(NULL);
	
#else

	if(GetFullPathNameA(filepath, MAX_PATH, realpath, NULL) == 0)
		return(NULL);
	
#endif

	return (realpath);	
}



/*
fileSizeW
Same behavior as fileSize() in nl-filesys.c
but accepts a WCHAR path.
Primarily for use in p_fileInfo()
*/
INT64 fileSizeW(WCHAR * pathName)
{
int handle;
INT64 size;

handle = _wopen(pathName,O_RDONLY | O_BINARY, 0);
size = lseek(handle, 0, SEEK_END);
close(handle);
if(size == -1) size = 0;
return(size);
}


/* ---------------------------------------------------------------------------- 
Wrappers for wide-character functions.
Same interface as the standard functions.
Adds the following functionality:
* first converts the UTF-8 string to UTF-16
* if there a conversion error, return fail
* calls the wide-character function
---------------------------------------------------------------------------- */

int	rename_utf16(const char* a, const char* b)
{
	int i;
	WCHAR * utf16a;
	WCHAR * utf16b;
	
	utf16a = utf8_to_utf16(a);
	if (utf16a == NULL) return (-1); 
	utf16b = utf8_to_utf16(b);
	if (utf16b == NULL) return (-1); 
	
	i = _wrename(utf16a, utf16b);
	free(utf16a);
	free(utf16b);
	return i;
}

int stat_utf16(const char* a, struct stat* b)
{
	int i;
	WCHAR * utf16a = utf8_to_utf16(a);
	if (utf16a == NULL) return (-1); 
	
	i = _wstat(utf16a, (struct _stat*)b);
	free(utf16a);
	return i;
}

int chdir_utf16(const char* a)
{
	int i;
	WCHAR * utf16a = utf8_to_utf16(a);
	if (utf16a == NULL) return (-1); 
	
	i = _wchdir(utf16a);
	free(utf16a);
	return i;
}

int open_utf16(const char* a, int b, int c)
{
	int i;
	WCHAR * utf16a = utf8_to_utf16(a);
	if (utf16a == NULL) return (-1);
	
	i = _wopen(utf16a, b, c);
	free(utf16a);
	return i;
}

int mkdir_utf16(const char* a)
{
	int i;
	WCHAR * utf16a = utf8_to_utf16(a);
	if (utf16a == NULL) return (-1);
	
	i = _wmkdir(utf16a);
	free(utf16a);
	return i;
}

int rmdir_utf16(const char* a)
{
	int i;
	WCHAR * utf16a = utf8_to_utf16(a);
	if (utf16a == NULL) return (-1);
	
	i = _wrmdir(utf16a);
	free(utf16a);
	return i;
}

int unlink_utf16(const char* a)
{
	int i;
	WCHAR * utf16a = utf8_to_utf16(a);
	if (utf16a == NULL) return (-1);
	
	i = _wunlink(utf16a);
	free(utf16a);
	return i;
}

_WDIR * opendir_utf16(const char* a)
{
	_WDIR * dir;
	WCHAR * utf16a = utf8_to_utf16(a);
	if (utf16a == NULL) return (NULL);
	
	dir = _wopendir(utf16a);
	free(utf16a);
	return dir;
}

/* eof */
