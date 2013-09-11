/*  win32-path.c 

    directory and file functions working on UTF-16 file and path names

    Copyright (C) 2007,2008,2010 Michael Sabin

    This software is provided 'as-is', without any express or implied
    warranty.  In no event will the authors be held liable for any damages
    arising from the use of this software.

    Permission is granted to anyone to use this software for any purpose,
    including commercial applications, and to alter it and redistribute it
    freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
       claim that you wrote the original software. If you use this software
       in a product, an acknowledgment in the product documentation would be
       appreciated but is not required.
    2. Altered source versions must be plainly marked as such, and must not be
       misrepresented as being the original software.
    3. This notice may not be removed or altered from any source distribution.
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
Used in win32_realpath,
but otherwise wrapped by utf16_to_utf8.
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
fileSize_utf16
Same behavior as fileSize() in nl-filesys.c
but accepts a UTF-8 string which is converted to UTF-16
and the wide-character open function is used.
*/
INT64 fileSize_utf16(char * pathName8)
{
	int handle;
	INT64 size = 0;

	WCHAR * pathName16 = utf8_to_utf16(pathName8);
	if (pathName16)
	{
		handle = _wopen(pathName16, O_RDONLY | O_BINARY, 0);
		size = lseek(handle, 0, SEEK_END);
		close(handle);
		if(size == -1) size = 0;
		free(pathName16);
	}
	return(size);
}


/* ---------------------------------------------------------------------------- 
Wrappers for wide-character functions.
Same interface as the standard functions, but accepts UTF-8 strings.
Adds the following functionality:
* first converts the UTF-8 string to UTF-16
* if there a conversion error, return fail
* calls the wide-character function
Note that errno is not set on UTF conversion failures.
---------------------------------------------------------------------------- */

int	rename_utf16(const char* oldname8, const char* newname8)
{
	int i = -1;
	WCHAR * oldname16;
	WCHAR * newname16;
	
	oldname16 = utf8_to_utf16(oldname8);
	if (oldname16)
	{
		newname16 = utf8_to_utf16(newname8);
		if (newname16)
		{
			i = _wrename(oldname16, newname16);
			free(oldname16);
		}
		free(newname16);
	}
	return i;
}

int stat_utf16(const char* filename8, struct stat* buf)
{
	int i = -1;
	WCHAR * filename16 = utf8_to_utf16(filename8);
	if (filename16)
	{
		i = _wstat(filename16, (struct _stat*)buf);
		free(filename16);
	}
	return i;
}

int chdir_utf16(const char* filename8)
{
	int i = -1;
	WCHAR * filename16 = utf8_to_utf16(filename8);
	if (filename16)
	{
		i = _wchdir(filename16);
		free(filename16);
	}
	return i;
}

int open_utf16(const char* filename8, int flags, int mode)
{
	int i = -1;
	WCHAR * filename16 = utf8_to_utf16(filename8);
	if (filename16)
	{
		i = _wopen(filename16, flags, mode);
		free(filename16);
	}
	return i;
}

int mkdir_utf16(const char* filename8)
{
	int i = -1;
	WCHAR * filename16 = utf8_to_utf16(filename8);
	if (filename16)
	{
		i = _wmkdir(filename16);
		free(filename16);
	}
	return i;
}

int rmdir_utf16(const char* filename8)
{
	int i = -1;
	WCHAR * filename16 = utf8_to_utf16(filename8);
	if (filename16)
	{
		i = _wrmdir(filename16);
		free(filename16);
	}
	return i;
}

int unlink_utf16(const char* filename8)
{
	int i = -1;
	WCHAR * filename16 = utf8_to_utf16(filename8);
	if (filename16)
	{
		i = _wunlink(filename16);
		free(filename16);
	}
	return i;
}

_WDIR * opendir_utf16(const char* dirname8)
{
	_WDIR * dir = NULL;
	WCHAR * dirname16 = utf8_to_utf16(dirname8);
	if (dirname16)
	{
		dir = _wopendir(dirname16);
		free(dirname16);
	}
	return dir;
}

/* eof */
