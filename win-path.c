/*  win-path.c 

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
        0,          /* no flags=ignore errors if possible */
        utf8str,
        -1,         /* read until NULL character */
        NULL,       
        0           /* just calculate size */
    );
    
    if (size == 0) return (NULL);
    
    utf16str = (WCHAR*)callocMemory((size+1) * sizeof(WCHAR));
    
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
Used in win_realpath,
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
    
    utf8str = (char*)callocMemory((size+1) * sizeof(char));
    
    if (utf16_to_utf8ptr(utf16str, utf8str, size) == -1)
    {
        free(utf8str);
        return(NULL);
    }
    else
        return(utf8str);
}

/* wrapper to return a cell */

#if SUPPORT_UTF8
CELL * utf8_from_mbcs(void * str)
{
WCHAR * utf16str;
char * utf8str;

utf16str = ansi_mbcs_to_utf16(str); 
utf8str = utf16_to_utf8(utf16str);
free(utf16str); 

if(utf8str != NULL)
	return(makeStringCell(utf8str, strlen(utf8str)));
else
	return(nilCell);
}
#endif

/*
mbcs_to_utf16
Uses the Windows API to convert a FileSystem/CommandArgs OEM CodePage string to a UTF-16 string.
Returns a pointer to a WCHAR string, or NULL if there was an error
*/
WCHAR * ansi_mbcs_to_utf16(const char *mbcsStr)
{
    int size = -1;
    WCHAR *utf16str = NULL;

    size = MultiByteToWideChar(
        CP_OEMCP,
        0,          /* no flags=ignore errors if possible */
        mbcsStr,
        -1,         /* read until NULL character */
        NULL,       
        0           /* just calculate size */
    );
    
    if (size == 0) return (NULL);
    
    utf16str = (WCHAR*)callocMemory((size+1) * sizeof(WCHAR));
    
    size = MultiByteToWideChar(
        CP_OEMCP,
        0,
        mbcsStr,
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
win_realpath
Identical interface as realpath
for both ANSI and UTF-16 path names on Windows.
Has the following functional differences
* If GetFullPathNameW, first converts the UTF-8 file name to UTF-16 (WCHAR)
* Uses GetFullPathNameA or GetFullPathNameW to receive the char/WCHAR path
* If GetFullPathNameW, converts the UTF-16 WCHAR path back to UTF-8
*/
char *win_realpath(const char *filepath, char *realpath)
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

return(isFile(realpath, 0) ? 0 : realpath);
}

/*
win_getModulePath - added by LM for version 10.4.7
used in newlisp.c:loadStartup(..) and newlisp.c:linkUnlink(...)
gets the full path for a loaded module (executable).
EXEName must be allocated by caller with PATH_MAX typically
*/

char * win_getExePath(char * EXEName)
{
#ifdef SUPPORT_UTF8
 WCHAR wEXEName[PATH_MAX] ;
 GetModuleFileNameW(NULL, wEXEName, PATH_MAX);
 utf16_to_utf8ptr(wEXEName, EXEName, PATH_MAX);
#else
 GetModuleFileName(NULL, EXEName, PATH_MAX);
#endif

return(EXEName);
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

int rename_utf16(const char* oldname8, const char* newname8)
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
    struct _stat st;
    WCHAR * filename16 = utf8_to_utf16(filename8);
    if (filename16)
    {
        i = _wstat(filename16, &st);
        free(filename16);
    }

    if (i == 0) {
    /* FIXME: incompatible type 'struct _stat' and 'struct stat' in MinGW64 ? */
    buf->st_dev   = st.st_dev;
    buf->st_ino   = st.st_ino;
    buf->st_mode  = st.st_mode;
    buf->st_nlink = st.st_nlink;
    buf->st_uid   = st.st_uid;
    buf->st_gid   = st.st_gid;
    buf->st_rdev  = st.st_rdev;
    buf->st_size  = st.st_size;
    buf->st_atime = st.st_atime;
    buf->st_mtime = st.st_mtime;
    buf->st_ctime = st.st_ctime;
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
