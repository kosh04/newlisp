/* nl-utf8.c --- functions for UTF-8 unicode support

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

//
// portions are copied from pcre.c by: Philip Hazel <ph10@cam.ac.uk>
// and Copyright (c) 1997-2003 University of Cambridge
//
*/


#include "newlisp.h"
#include <wchar.h>
#include <wctype.h>
#include "protos.h"

/* from win-path.c */
CELL * utf8_from_mbcs(void * mbcs_str);

/*************************************************
*    Macros and tables for character handling    *
*        by Philip Hazel <ph10@cam.ac.uk>        *
*************************************************/

/* These are the breakpoints for different numbers of bytes in a UTF-8
character. */

static const int utf8_table1[] =
  { 0x7f, 0x7ff, 0xffff, 0x1fffff, 0x3ffffff, 0x7fffffff};

/* These are the indicator bits and the mask for the data bits to set in the
first byte of a character, indexed by the number of additional bytes. */

static const int utf8_table2[] = { 0,    0xc0, 0xe0, 0xf0, 0xf8, 0xfc};
static const int utf8_table3[] = { 0xff, 0x1f, 0x0f, 0x07, 0x03, 0x01};

/* Table of the number of extra characters, indexed by the first character
masked with 0x3f. The highest number for a valid UTF-8 character is in fact
0x3d. */

static const char utf8_table4[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5 };

/* Get the next UTF-8 character, advancing the pointer. This is called when we
know we are in UTF-8 mode. */

#define GETCHARINC(c, eptr) \
  c = (unsigned char)*eptr++; \
  if ((c & 0xc0) == 0xc0) \
    { \
    int gcaa = utf8_table4[c & 0x3f];  /* Number of additional bytes */ \
    int gcss = 6*gcaa; \
    c = (c & utf8_table3[gcaa]) << gcss; \
    while (gcaa-- > 0) \
      { \
      gcss -= 6; \
      c |= (*eptr++ & 0x3f) << gcss; \
      } \
    }

/* This function takes an integer value in the range 0 - 0x7fffffff
and encodes it as a UTF-8 character in 0 to 6 bytes.

Arguments:
  cvalue     the character value
  buffer     pointer to buffer for result - at least 6 bytes long

Returns:     number of characters placed in the buffer
*/

int wchar_utf8(int cvalue, char *buffer)
{
register int i, j;
for (i = 0; i < sizeof(utf8_table1)/sizeof(int); i++)
  if (cvalue <= utf8_table1[i]) break;
buffer += i;
for (j = i; j > 0; j--)
 {
 *buffer-- = 0x80 | (cvalue & 0x3f);
 cvalue >>= 6;
 }
*buffer = utf8_table2[i] | cvalue;
return i + 1;
}


/* ---------------------- UTF-8 utility fuctions --------------------------- */

/* get utf8 string from unicode wide character
 * 
 * int wchar_utf8(int wchar, char * utf8str)
 * 
 * the string is not nullterminated for contiguos filling
 * of longer strings
 * returns number of bytes placed in utf8str
*/


/* get a unicode wide character from the utf8 string
 * return advanced utf8 string pointer
*/

char * utf8_wchar(char * utf8str, int * chr)
{
GETCHARINC(*chr, utf8str)

return(utf8str);
}

/* return the number of characters encoded in utf8 string
 * without counting the zero terminator
 * new limit param in 10.4.5 to avoid overrun on invalid utf8 strings
*/

size_t utf8_wlen(char * utf8str, char * limit)
{
int gcaa;
int c;
size_t count = 0;

while((c = *utf8str++) != 0 && utf8str < limit)
    {
    count++;
    if ((c & 0xc0) == 0xc0)
        {
        gcaa = utf8_table4[c & 0x3f];
        utf8str += gcaa;
        }
    }

/* now handled by while loop 10.6.1
if(utf8str > limit)
    errorProc(ERR_INVALID_UTF8);
*/

return(count);
}

/* return ptr to character at index, added by LM 2012-06-10
*/

char * utf8_index(char * utf8str, int idx)
{
int c;

while((c = *utf8str) != 0 && idx-- != 0)
    {
    if ((c & 0xc0) == 0xc0)
        utf8str += utf8_table4[c & 0x3f] + 1;
    else    
        utf8str++;
    }

return(utf8str);
}


/* return the length of the first utf8 character
*/

int utf8_1st_len(char * utf8str)
{
int c;

if((c = *utf8str) != 0)
    {
    if((c & 0xc0) == 0xc0)
        return(utf8_table4[c & 0x3f] + 1);
    else return(1);
    }

return(0);
}


/* convert utf8 string to vector of maxwc wide characters
 * unicode vector is zero terminated
 * return number of unicode characters (excluding zero int)
*/

int utf8_wstr(int * unicode, char * utf8str, int maxwc)
{
int wchar;
int count = 0;

while(maxwc-- && *utf8str != 0)
    {
    count++;
    GETCHARINC(wchar, utf8str);
/*  utf8str = utf8_wchar(utf8str, &wchar); */
    *(unicode++) = wchar;
    }
*unicode = 0;

return(count);
}

/* convert zero terminated unicode vector into utf8 string
 * return number of bytes stored in utr8 string excluding terminator
 * don't use more then maxstr bytes (excluding  zero terminator)
*/

int wstr_utf8(char * utf8str, int * unicode, int maxstr)
{
int len, size = 0;

while(*unicode != 0 && size < maxstr)
    {
    len = wchar_utf8(*unicode, utf8str);
    utf8str += len;
    size += len;
    unicode++;
    }

*utf8str = 0;

return(size);
}

/* -------------------------------------- newLISP API -----------------------------------*/

CELL * p_unicode(CELL * params)
{
char * utf8str;
size_t size;
int * unicode;

getStringSize(params, &utf8str, &size, TRUE);
unicode = allocMemory((size + 1) * sizeof(int));

size = utf8_wstr(unicode, utf8str, size);
unicode = reallocMemory(unicode, (size + 1) * sizeof(int) + 1);

/*
cell = getCell(CELL_STRING);
cell->contents = (UINT)unicode;
cell->aux = (size + 1) * sizeof(int) + 1;
*/

return(makeStringCell((char *)unicode, (size + 1) * sizeof(int)));
}


CELL * p_utf8(CELL * params)
{
int * unicode;
size_t size;
char * utf8str;

params = getStringSize(params, (void *)&unicode, &size, TRUE);
#ifdef WINDOWS
if(getFlag(params)) /* its a MBCS string */
    return(utf8_from_mbcs((void *)unicode));
#endif
    

utf8str = callocMemory(size * UTF8_MAX_BYTES + 1);

size = wstr_utf8(utf8str, unicode, size);
utf8str = reallocMemory(utf8str, size + 1);
*(utf8str + size) = 0;

return(makeStringCell(utf8str, size));
}


CELL * p_utf8len(CELL * params)
{
char * str;
size_t size;

getStringSize(params, &str, &size, TRUE);

return(stuffInteger(utf8_wlen(str, str + size + 1)));
}

/* reads a UTF-8 character */
CELL * p_readUTF8(CELL * params)
{
UINT handle;
int utf8C, gcaa, gcss;
char chr;

getInteger(params, &handle);
if(read((int)handle, &chr, 1) <= 0)
    return(nilCell);

utf8C = chr;
    
if((chr & 0xc0) == 0xc0)
    {
    gcaa = utf8_table4[chr & 0x3f];  /* Number of additional bytes */
    gcss = 6*gcaa;
    utf8C = (chr & utf8_table3[gcaa]) << gcss;
    while (gcaa-- > 0) \
        {
        gcss -= 6;
        
        if(read((int)handle, &chr, 1) <= 0)
            return(nilCell);

        utf8C |= (chr & 0x3f) << gcss;
        }
    }

return(stuffInteger(utf8C));
}

/* eof */
