/* nl-liststr.c --- newLISP primitives handling lists and strings


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
#include "pcre.h"
#include "protos.h"

extern CELL * lastCellCopied;
extern SYMBOL * sysSymbol[];

/* used only on string indices */
size_t adjustNegativeIndex(ssize_t index, size_t length)
{
if(index < 0) index = length + index;
else if((index+1) > length) errorProc(ERR_STRING_INDEX_OUTOF_BOUNDS);

if(index < 0) errorProc(ERR_STRING_INDEX_OUTOF_BOUNDS);

return(index);
}

size_t adjustCount(ssize_t count, ssize_t length)
{
if(length <= 1 || count == 0 || length == count)
	return(0);

if(count > 0)
    count = count % length;
else
    {
    count = -count % length;
    count = length - count;
    }
return(count);
}

#ifdef LINUX
extern char * strcasestr(char * haystack, char * needle);
#endif
CELL * p_member(CELL * params)
{
CELL * key;
CELL * list;
long options  = -1;
char * ptr;
ssize_t pos;

key = evaluateExpression(params);

params = params->next;
getDefaultOrEval(params, &list);

if(params->next != nilCell)
	getInteger(params->next, (UINT *)&options);

if(isList(list->type))
	list = (CELL *)list->contents;
else if (list->type == CELL_STRING)
	{
	if(key->type != CELL_STRING)
		return(errorProcExt(ERR_STRING_EXPECTED, key));
	if(options == -1)
		{
		ptr = strstr((char *)list->contents, (char *) key->contents);
		if(ptr) return(stuffString(ptr));
		}	
	else
		{
		pos = searchBufferRegex((char*)list->contents, 0, (char *)key->contents, list->aux - 1, options, 0);
		if(pos != -1) return(stuffString((char *)list->contents + pos));
		}
	return(nilCell);
	}
else 
	return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, params->next));

while(list != nilCell)
	{
	if(compareCells(key, list) == 0) break;
	list = list->next;
	}

if(list == nilCell) return(nilCell);
return(makeCell(CELL_EXPRESSION, (UINT)copyList(list)));
}

CELL * p_length(CELL * params)
{
size_t length;
SYMBOL * symbol;

params = evaluateExpression(params);
length = 0;
switch(params->type)
	{
	case CELL_LONG:
		length = sizeof(UINT); break;
#ifndef NEWLISP64
	case CELL_INT64:
		length = sizeof(INT64); break;
#endif
	case CELL_FLOAT:
		length = sizeof(double); break;
	case CELL_STRING:
		length = params->aux - 1; break;
	case CELL_CONTEXT:
		symbol = translateCreateSymbol( ((SYMBOL*)params->contents)->name, CELL_NIL,
			(SYMBOL*)params->contents, TRUE);
		params = (CELL *)symbol->contents;
		if(params->type == CELL_STRING)
			length = params->aux - 1;
		else if(isList(params->type))
			length = listlen((CELL *)params->contents);
		break;
	case CELL_SYMBOL:
		symbol = (SYMBOL *)params->contents;
		length = strlen(symbol->name);
		break;
	case CELL_DYN_SYMBOL:
		length = strlen((char *)params->contents);
		break;
	case CELL_EXPRESSION:
	case CELL_LAMBDA:
	case CELL_MACRO:
	        length = listlen((CELL *)params->contents);
		break;
	case CELL_ARRAY:
		length = (params->aux - 1) / sizeof(UINT);
	default:
		break;
	}
return(stuffInteger(length));
}


CELL * p_append(CELL * params)
{
CELL * list = NULL;
CELL * firstCell = NULL;
CELL * copy = NULL;
CELL * cell;

while(params != nilCell)
    {
    getDefaultOrEval(params, &cell);
    if(!isList(cell->type))
        {
        if(copy == NULL)
			{
			if(cell->type == CELL_STRING)
            	return(appendString(cell, params->next, NULL, 0, FALSE, TRUE));
			else if(cell->type == CELL_ARRAY)
            	return(appendArray(cell, params->next));
			return(errorProcExt(ERR_ARRAY_LIST_OR_STRING_EXPECTED, params));
			}
		
        return(errorProcExt(ERR_LIST_EXPECTED, params));
        }

	if(list == NULL)
        list = getCell(cell->type);

    copy = copyList((CELL *)cell->contents);

    params = params->next;
    if(copy == nilCell) continue;

    if(firstCell == NULL) list->contents = (UINT)copy;
    else firstCell->next = copy;

    firstCell = lastCellCopied;
    }

if(list == NULL)
	return(getCell(CELL_EXPRESSION));

return(list);
}


CELL * appendString(CELL * cell, CELL * list, char * joint, size_t jointLen, int trailJoint, int evalFlag)
{ 
CELL * result;
STREAM stream = {0, NULL, NULL, 0, 0};
char * sPtr;
size_t len;

openStrStream(&stream, MAX_LINE, 0);
writeStreamStr(&stream, (char *)cell->contents, cell->aux - 1);
while(list != nilCell)
	{
	if(joint == NULL)
		{
		list = getStringSize(list, &sPtr, &len, evalFlag);		
		writeStreamStr(&stream, sPtr, len);
		}
	else
		{
		list = getStringSize(list, &sPtr, &len, FALSE);
		if(jointLen) writeStreamStr(&stream, joint, jointLen);
		writeStreamStr(&stream, sPtr, len);
		}
	}

if(trailJoint)
	writeStreamStr(&stream, joint, jointLen);

/*
result = getCell(CELL_STRING);
result->contents = (UINT)allocMemory(stream.position + 1);
*((char *)result->contents + stream.position) = 0;
result->aux = stream.position + 1;
memcpy((void *)result->contents, stream.buffer, stream.position);
*/

result = stuffStringN(stream.buffer, stream.position);

closeStrStream(&stream);

return(result);
}


CELL * p_chop(CELL * params)
{
size_t number = 1;
size_t length = 0;
CELL * next;
#ifdef SUPPORT_UTF8
char * ptr;
#endif

next = params->next;
getDefaultOrEval(params, &params);

if(next != nilCell)
	getInteger(next, (UINT *)&number);

if(params->type == CELL_STRING)
	{
#ifndef SUPPORT_UTF8
	length = params->aux - 1;
	if(number > length) number = length;
	length = length - number;
	return stuffStringN((char *)params->contents, length);
#else
	length = utf8_wlen((char *)params->contents);
	if(number > length) number = length;
	length = length - number;
	ptr = (char *)params->contents;
	while(length--)
		ptr += utf8_1st_len(ptr);
	return stuffStringN((char *)params->contents, ptr - (char *)params->contents);
#endif
	}

if(!isList(params->type))
    return(errorProc(ERR_LIST_OR_STRING_EXPECTED));	

length = listlen((CELL *)params->contents);
if(number > length) number = length;

return(sublist((CELL *)params->contents, 0, length - number));
}

CELL * setNthStr(CELL * cellStr, CELL * new, ssize_t index, int typeFlag);
CELL * setNth(CELL * params, int typeFlag);

CELL * p_nth(CELL * params) {return setNth(params, 0);} 
CELL * p_nthSet(CELL * params) {return setNth(params, 1);}
CELL * p_setNth(CELL * params) {return setNth(params, 2);}

CELL * setNth(CELL * params, int typeFlag)
{
ssize_t index;
CELL * list; 
CELL * next;
CELL * cell = NULL;

/* new syntax, distinguished by type of first arg and number of args */
next = params->next;
if( (params->type == CELL_EXPRESSION) &&
	( (!typeFlag && next == nilCell) || (typeFlag && next->next == nilCell) ))
	{
	params = getListSpec(params, &list, typeFlag);

NTH_EVAL_IMPLICIT:
	if(isList(list->type))
		{
		if(!typeFlag)
			return(copyCell(implicitIndexList(list, params)));
		else if(typeFlag == 1)
			return(updateCell(implicitIndexList(list, params), next));
		else
			{
			deleteList(updateCell(implicitIndexList(list, params), next));
			return(copyCell(list));
			}
		}

	else if(list->type == CELL_ARRAY)
		{
		if(!typeFlag)
			return(copyCell(implicitIndexArray(list, params)));
		else if(typeFlag == 1)
			return(updateCell(implicitIndexArray(list, params), next));
		else
			{
			deleteList(updateCell(implicitIndexArray(list, params), next));
			return(copyCell(list));
			}
		}
	
	else if(list->type == CELL_STRING)
		{
		getInteger(params, (UINT *)&index);
		return(setNthStr(list, next, index, typeFlag));
		}

	return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, list));
	}

list = evaluateExpression(params);
if(!isNumber(list->type))
	return(errorProcExt(ERR_NUMBER_EXPECTED, params));

/* old syntax , fake new syntax*/
while(isNumber(list->type))
	{
	if(cell == NULL)
		{
		cell = makeCell(CELL_EXPRESSION, (UINT)copyCell(list));
		next = (CELL *)cell->contents;
		}
	else
		{
		next->next = copyCell(list);
		next = next->next;
		}

	params = params->next;
	if(typeFlag)
		list = evalCheckProtected(params, NULL);
	else
		getDefaultOrEval(params, &list);
	}

next = params->next;

if(list->type == CELL_STRING)
	{
	getInteger((CELL *)cell->contents, (UINT *)&index);
	deleteList(cell);
	return(setNthStr(list, next, index, typeFlag));
	}

params = makeCell(CELL_QUOTE, (UINT)cell);

pushResult(params);

goto NTH_EVAL_IMPLICIT;
}	


#define INSERT_BEFORE 0
#define INSERT_AFTER 1
#define INSERT_END 2
CELL * p_push(CELL * params)
{
CELL * newCell;
CELL * list;
CELL * cell = NULL;
SYMBOL * context;
SYMBOL * sPtr;
int insert = 0, evalFlag = 0;
ssize_t index;

newCell = evaluateExpression(params);
params = params->next;

if(isSymbol(params->type))
	{
	if(params->type == CELL_SYMBOL)
		sPtr = (SYMBOL *)params->contents;
	else
		sPtr = getDynamicSymbol(params);

	list = (CELL*)sPtr->contents;
	if(list->type == CELL_CONTEXT)
		{
		context = (SYMBOL *)list->contents;
		sPtr= translateCreateSymbol(context->name, CELL_NIL, context, TRUE);	
		list = (CELL *)sPtr->contents;
		}

	if(isProtected(sPtr->flags))
		return(errorProcExt(ERR_SYMBOL_PROTECTED, params));

	if(isNil(list)) 
		{
		deleteList((CELL*)sPtr->contents);
		cell = copyCell(newCell);
		sPtr->contents = (UINT)makeCell(CELL_EXPRESSION, (UINT)cell);
		pushResultFlag = FALSE;
		return(cell);
		}
	}
else
	list = evalCheckProtected(params, NULL);


if(!isList(list->type))
	{
	if(list->type == CELL_STRING)
		return(pushOnString(newCell, list, params->next));
	else
		return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, params));
	}

if(params->next == nilCell) 
	{
	params = params->next;
	index = 0;
	}
else 
	{
	cell = ((CELL*)params->next)->next;
	params = evaluateExpression(params->next);
	if(isList(params->type))
		{
		evalFlag = FALSE;
		params = getIntegerExt((CELL*)params->contents, (UINT*)&index, FALSE);
		}
	else 
		{
		evalFlag = TRUE;
		getIntegerExt(params, (UINT*)&index, FALSE);
		params = cell;
		}
	}

if(index == -1) 
	{
	if(params == nilCell)
		{
		newCell = copyCell(newCell);
		cell = (CELL*)list->aux;	
		list->aux = (UINT)newCell;
		if(cell != nilCell && cell != trueCell)
			{
			cell->next = newCell;
			return(copyCell(newCell));
			}

		if(list->contents == (UINT)nilCell)
			{
			list->contents = (UINT)newCell;
			return(copyCell(newCell));
			}

		list = (CELL *)list->contents;
		while(list->next != nilCell)
			list = list->next;
		list->next = newCell;
		return(copyCell(newCell));
		}

	/* index = MAX_LONG; */
	}
	
list->aux = (UINT)nilCell; /* undo last element optimization */

while(isList(list->type))
	{
	cell = list;
	list = (CELL *)list->contents;

	if(index < 0) 
		{
		index = listlen(list) + index;
		if(index == -1) 
			{
			index = 0;
			insert = INSERT_BEFORE;
			}
		else if(index >= 0) insert = INSERT_AFTER;
		else errorProc(ERR_LIST_INDEX_OUTOF_BOUNDS);
		}
	else insert = INSERT_BEFORE;

	while(index--) 
		{
		if(list == nilCell)
			{
			if(index >= 0) errorProc(ERR_LIST_INDEX_OUTOF_BOUNDS);
			insert = INSERT_END;
			break;
			}
		cell = list;
		list = list->next;
		}

	if(params == nilCell || !isList(list->type))  break;
	params = getIntegerExt(params, (UINT*)&index, evalFlag);
	}

newCell = copyCell(newCell);
if(insert == INSERT_BEFORE || list == nilCell)
	{
	if(list == (CELL*)cell->contents)
		{
		cell->contents = (UINT)newCell;
		newCell->next = list;
		}
	else
		{
		cell->next = newCell;
		newCell->next = list;
		}
	}

else if(insert == INSERT_AFTER || insert == INSERT_END)
    {
    cell = list->next;
    list->next = newCell;
    newCell->next = cell;
    }

return(copyCell(newCell));
}


CELL * p_pop(CELL * params)
{
CELL * list;
CELL * cell = NULL;
ssize_t index;
int evalFlag = FALSE;

list = evalCheckProtected(params, NULL);

if(!isList(list->type))
	{
	if(list->type == CELL_STRING)
		return(popString(list, params->next));
	else
		return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, params));
	}

/* leave last element optimization if popping first for queues */
if(params->next == nilCell)
	{
	cell = (CELL *)list->contents;
	list->contents = (UINT)cell->next;
	if(cell->next == nilCell) /* check if only one element in list */
		list->aux = (UINT)nilCell; /* undo last element optimization */
	cell->next = nilCell;
	return(cell);
	}
else
	{
	list->aux = (UINT)nilCell; /* undo last element optimization */
	cell = ((CELL*)params->next)->next;
	params = evaluateExpression(params->next);
	if(isList(params->type))
		{
		evalFlag = FALSE;
		params = getIntegerExt((CELL*)params->contents, (UINT*)&index, FALSE);
		}
	else 
		{
		evalFlag = TRUE;
		getIntegerExt(params, (UINT*)&index, FALSE);
		params = cell;
		}
	}

while(isList(list->type))
    {
    cell = list;
    list = (CELL *)list->contents;

   	if(index < 0) index = convertNegativeOffset(index, list);

   	while(index--) 
       	{
		cell = list;
       	list = list->next;
       	}
	if(list == nilCell) 
		errorProc(ERR_LIST_INDEX_OUTOF_BOUNDS);

    if(params == nilCell || !isList(list->type))  break;
    params = getIntegerExt(params, (UINT*)&index, evalFlag);
    }

if(list == (CELL*)cell->contents)
    cell->contents = (UINT)list->next;
else
    cell->next = list->next;

list->next = nilCell;
return(list);
}


CELL * setNthStr(CELL * cellStr, CELL * new, ssize_t index, int typeFlag)
{
char * newStr;
char * oldStr;
size_t newLen, oldLen, len;
char * str;

oldStr = (char*)cellStr->contents;
oldLen = cellStr->aux - 1;

if(oldLen == 0) return(copyCell(cellStr));

#ifndef SUPPORT_UTF8

index = adjustNegativeIndex(index, oldLen);

if(!typeFlag)
	return(stuffStringN(oldStr + index, 1));

deleteList((CELL*)sysSymbol[0]->contents);
sysSymbol[0]->contents = (UINT)stuffStringN(oldStr + index, 1);
len = 1;

#else

index = adjustNegativeIndex(index, utf8_wlen((char *)cellStr->contents));
str = oldStr;

while(index--) 
	{
	len = utf8_1st_len(str);
	str += len;
	}
len = utf8_1st_len(str);

if(!typeFlag)
	return(stuffStringN(str, len));

deleteList((CELL*)sysSymbol[0]->contents);
sysSymbol[0]->contents = (UINT)stuffStringN(str, len);
index = str - oldStr;

#endif

getStringSize(new, &newStr, &newLen, TRUE);
/* get back oldStr in case it changed during eval of replacement */
oldStr = (char *)cellStr->contents;
oldLen = cellStr->aux - 1;
if(oldLen == 0) return(copyCell(cellStr));
index = adjustNegativeIndex(index, oldLen);

str = allocMemory(oldLen + newLen - len + 1);
*(str + oldLen + newLen - len) = 0;

memcpy(str, oldStr, index);
memcpy(str + index, newStr, newLen);
memcpy(str + index + newLen, oldStr + index + len, oldLen - index - len);

cellStr->contents = (UINT)str;
cellStr->aux = oldLen + newLen - len + 1;

if(typeFlag != 2) 
	{
	new = stuffStringN(oldStr + index, len);
	freeMemory(oldStr);
	return(new);
	}

freeMemory(oldStr);
return(copyCell(cellStr));
}


CELL * popString(CELL * str, CELL * params)
{
char * ptr;
char * newPtr;
ssize_t index = 0;
ssize_t len = 1;
CELL * result;

if(str->aux < 2)
	return(stuffString(""));

if(params != nilCell)
	{
	params = getInteger(params, (UINT*)&index);
	if(params != nilCell) 
		{
		getInteger(params, (UINT*)&len);
		if(len < 1) len = 0;
		}
	}

ptr = (char *)str->contents;

#ifndef SUPPORT_UTF8
index = adjustNegativeIndex(index, str->aux - 1);
#else
index = adjustNegativeIndex(index, utf8_wlen(ptr));
#endif

if((index + len) > (str->aux - 2))
	len = str->aux - 1 - index;

newPtr = callocMemory(str->aux - len);

memcpy(newPtr, ptr, index);
memcpy(newPtr + index, ptr + index + len, str->aux - len - index);
str->aux = str->aux - len;
str->contents = (UINT)newPtr;
result = stuffStringN(ptr + index, len);
free(ptr);
return(result);
}


CELL * pushOnString(CELL * newStr, CELL * str, CELL * idx)
{
ssize_t index = 0;
char * ptr;
char * newPtr;
int minusFlag;
int len;
#ifdef SUPPORT_UTF8
char * sptr;
int wChar;
#endif

if(idx != nilCell) getInteger(idx, (UINT*)&index);
ptr = (char *)str->contents;

if(newStr->type != CELL_STRING)
	return(errorProcExt(ERR_STRING_EXPECTED, newStr));

if(index == -1)
	{
	appendCellString(str, (char *)newStr->contents, newStr->aux - 1);
	return(copyCell(newStr));
	}

minusFlag = (index < 0);

#ifndef SUPPORT_UTF8
len = str->aux - 1;
#else
len = utf8_wlen(ptr);
#endif

/* convert index into characters to skip before the new one is inserted */
if(index < 0) index = len + index + 1;
else if(index > len) index = len;
if(index < 0) index = 0;

newPtr = callocMemory(str->aux + newStr->aux - 1);
#ifndef SUPPORT_UTF8
memcpy(newPtr, ptr, index);
memcpy(newPtr + index, (char*)newStr->contents, newStr->aux - 1);
memcpy(newPtr + index + newStr->aux - 1, ptr + index, str->aux - index);
#else
sptr = ptr;
while(index--) /* skip characters to split point) */
	sptr = utf8_wchar(sptr, &wChar);
memcpy(newPtr, ptr, sptr - ptr);
memcpy(newPtr + (sptr - ptr), (char*)newStr->contents, newStr->aux - 1);
memcpy(newPtr + (sptr - ptr) + newStr->aux - 1, sptr, str->aux - (sptr - ptr)  );
#endif

str->contents = (UINT)newPtr;
str->aux = str->aux + newStr->aux - 1;
*(newPtr + str->aux - 1) = 0;
free(ptr);

return(copyCell(newStr));
}


CELL * p_select(CELL * params)
{
size_t n = 0, idx  = 0; 
ssize_t index;
CELL * list, * cell;
CELL * result = NULL;
CELL * head;
int evalFlag = TRUE;
char * str, * newStr;
#ifdef SUPPORT_UTF8
int * wstr;
int * wnewStr;
size_t len;
#endif

params = getDefaultOrEval(params, &head);
cell = evaluateExpression(params);
if(isList(cell->type))
	{
	evalFlag = FALSE;
	cell = params = (CELL *)cell->contents;
	}

if(head->type == CELL_STRING)
	{
	if((n = listlen(params)) == 0) return(stuffString(""));
	
	str = (char *)head->contents;
#ifndef SUPPORT_UTF8
	newStr = (char *)allocMemory(n + 1);
	idx = 0;
	while(params->type != CELL_NIL)
		{
		if(idx == 0)
			{
			getIntegerExt(cell, (UINT *)&index, FALSE);
			params = params->next;
			}
		else
			params = getIntegerExt(params, (UINT *)&index, evalFlag);
		index = adjustNegativeIndex(index, head->aux -1);
		*(newStr + idx++) = *(str + index);
		}
	*(newStr + n) = 0;
#else
	wstr = allocMemory(head->aux * sizeof(int));
	len = utf8_wstr(wstr, str, head->aux - 1);
	wnewStr = allocMemory((n + 1) * sizeof(int));
	idx = 0;
	while(params->type != CELL_NIL)
		{
		if(idx == 0)
			{
			getIntegerExt(cell, (UINT *)&index, FALSE);
			params = params->next;
			}
		else
			params = getIntegerExt(params, (UINT *)&index, evalFlag);
		index = adjustNegativeIndex(index, len);
		*(wnewStr + idx++) = *(wstr + index);
		}
	*(wnewStr + n) = 0;
	newStr = allocMemory(UTF8_MAX_BYTES * n + 1);
	n = wstr_utf8(newStr, wnewStr, UTF8_MAX_BYTES * n);
	newStr = reallocMemory(newStr, n + 1);
#endif
	result = getCell(CELL_STRING);
	result->aux = n + 1;
	result->contents = (UINT)newStr;	
	return(result);
	}

if(!isList(head->type))
	return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, head));
head = (CELL *)head->contents;
list = head;
n = 0;
while(params->type != CELL_NIL)
	{
	if(n++ == 0)
		{
		getIntegerExt(cell, (UINT *)&index, FALSE);
		params = params->next;
		}
	else
		params = getIntegerExt(params, (UINT *)&index, evalFlag);
	if(index < 0) index = convertNegativeOffset(index, head);
	if(index < idx) list = head, idx = 0;
	while(idx < index  && list->next != nilCell) list = list->next, idx++; 
	if(result == NULL)
		{
		result = getCell(CELL_EXPRESSION);
		cell = copyCell(list);
		result->contents = (UINT)cell;
		}
	else
		{
		cell->next = copyCell(list);
		cell = cell->next;
		}
	}

return((result == NULL) ? getCell(CELL_EXPRESSION) : result);
}	


CELL * p_slice(CELL * params)
{
CELL * cell;
ssize_t offset;
ssize_t length;

params = getDefaultOrEval(params, &cell);
params = getInteger(params, (UINT *)&offset);
if(params != nilCell)
	getInteger(params, (UINT *)&length);
else
	length = MAX_LONG;

if(isList(cell->type))
	return(sublist((CELL *)cell->contents, offset, length));
else if(cell->type == CELL_STRING)
	return(substring((char *)cell->contents, cell->aux - 1, offset, length));
else if(cell->type == CELL_ARRAY)
	return(subarray(cell, offset, length));

return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, params));
}


CELL * sublist(CELL * list, ssize_t offset, ssize_t length)
{
CELL * subList;
CELL * cell;

if(offset < 0) 
	offset = convertNegativeOffset(offset, list);

if(length < 0)
	{
	length = listlen(list) - offset + length;
	if(length < 0) length = 0;
	}

subList = getCell(CELL_EXPRESSION);
if(length == 0) return(subList);

while(offset-- && list != nilCell)
	list = list->next;

if(list == nilCell) return(subList);

cell = copyCell(list);
subList->contents = (UINT)cell;
--length;
while(length--) 
	{
	list = list->next;
	if(list == nilCell) break;
	cell->next = copyCell(list);
	cell = cell->next;
	}

return(subList);
}


CELL * p_reverse(CELL * params)
{
CELL * cell;
CELL * previous;
CELL * next;
char * str;
size_t len, tmp;
char * left;
char * right;

cell = params;
params = evalCheckProtected(params, NULL);


if(isList(params->type))
	{
	params->aux = (UINT)nilCell; /* undo last element optimization */

	previous = cell = (CELL*)params->contents;
	next = cell->next;
	cell->next = nilCell;
	while(cell!= nilCell)
		{
		previous = cell;
		cell = next;
		next = cell->next;
		if(cell != nilCell) cell->next = previous;
		}
	params->contents = (UINT)previous;
	}

else if(params->type == CELL_STRING)
	{
	str = (char *)params->contents;
	len = params->aux - 1;
	left = str;
	right = left + len - 1;
	while(left < right)
		{
		tmp = *left;
		*left = *right;
		*right = tmp;
		left++;
		right--;
		}
	}
else return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, cell));

return(copyCell(params));
}


CELL * p_join(CELL * params)
{
char * joint = NULL;
CELL * list;
size_t jointLen = 0;
int trailJoint = 0;

params = getListHead(params, &list);
if(list == nilCell)
	return(stuffString(""));

if(list->type != CELL_STRING)
	return(errorProcExt(ERR_STRING_EXPECTED, list));

if(params != nilCell)
	{
	params = getStringSize(params, &joint, &jointLen, TRUE);
	trailJoint = getFlag(params);
	}

return(appendString(list, list->next, joint, jointLen, trailJoint, FALSE));
}


CELL * p_find(CELL * params) 
{
char * key;
char * second;
ssize_t found;
CELL * next;
CELL * keyCell;
CELL * funcCell;
size_t size;
long options;

keyCell = evaluateExpression(params);
params = params->next;
params = getDefaultOrEval(params, &next);

if(keyCell->type == CELL_STRING && next->type == CELL_STRING)
	{
	key = (char *)keyCell->contents;
	second = (char *)next->contents;
	size = next->aux - 1;

	if(params != nilCell)
            {
            params = getInteger(params, (UINT*)&options);
            found = searchBufferRegex(second, 0, key, (int)size, options, NULL);
            if(found == -1) return(nilCell);
            }
        else
            {
            found = searchBuffer(second, size, key, keyCell->aux - 1, TRUE);
            if(found == -1) return(nilCell);
            }
	}
else
	{
    /* list mode with optional functor */

	if(!isList(next->type)) return(nilCell);
	next = (CELL *)next->contents;
	found = 0;

	if(params != nilCell)
		funcCell = evaluateExpression(params);
	else funcCell = NULL;

   	/* do regex when first arg is string and option# is present */
   	if(funcCell && isNumber(funcCell->type) && keyCell->type == CELL_STRING)
       	{
       	getIntegerExt(funcCell, (UINT*)&options, FALSE);
       	key = (char *)keyCell->contents;
       	while(next != nilCell)
           	{
           	if(next->type == CELL_STRING)
               	{
               	second = (char *)next->contents;
               	if(searchBufferRegex(second, 0, key, next->aux - 1 , options, NULL) != -1)
                   	break;
               	}
           	found++;
           	next = next->next;
           	}
       	if(next == nilCell) return(nilCell);
		else return(stuffInteger(found));
       	}

    while(next != nilCell)
		{
		if(compareFunc(keyCell, next, funcCell) == 0)
			{
			if(funcCell)
				{
				deleteList((CELL*)sysSymbol[0]->contents);
				sysSymbol[0]->contents = (UINT)copyCell(next);
				}
			break;
			}
		found++;
		next = next->next;
		}
	if(next == nilCell) return(nilCell);
	}

return(stuffInteger(found));
}

/* ------- find-all ---- finds all strings matching a pattern in a list ----- */

CELL * findAllString(char * pattern, char * str, size_t size, CELL * params)
{
long options = 0;
ssize_t findPos = -1;
int len;
int offset = 0;
CELL * result = nilCell;
CELL * cell = NULL;
CELL * exprCell;
CELL * exprRes;
int errNo;

exprCell = params;
params = params->next;
if(params != nilCell)
	getInteger(params, (UINT *)&options);
	
while( (findPos = searchBufferRegex(str, offset, pattern, (int)size, options, &len)) != -1)
	{
	if(exprCell != nilCell)
		{
		if((exprRes = evaluateExpressionSafe(exprCell, &errNo)) == NULL)
        	{
			pushResult(result); /* push for later deletion */
       		longjmp(errorJump, errNo);
       		}
		exprRes = copyCell(exprRes);
		}
	else
		exprRes = stuffStringN(str + findPos, len);

	if(findPos == offset && len == 0) break;
		
	if(result == nilCell)
		{
		cell = exprRes;
		result = makeCell(CELL_EXPRESSION, (UINT)cell);
		}
	else
		{
		cell->next = exprRes;
		cell = cell->next;
		}

	offset = (findPos + len);
	}

if(result == nilCell)
	return(getCell(CELL_EXPRESSION));

return(result);
}


CELL * findAllList(CELL * pattern, CELL * list, CELL * exprCell)
{
CELL * result = nilCell;
CELL * cell = NULL;
CELL * exprRes;
CELL * match;
CELL * funcCell;
int errNo;
int resultIdxSave;

funcCell = evaluateExpression(exprCell->next);
resultIdxSave = resultStackIdx;

if(funcCell == nilCell && !isList(pattern->type))
	return(errorProcExt(ERR_LIST_EXPECTED, pattern));
	
while(list != nilCell)
	{
	if(funcCell == nilCell)
		{
		/* match only takes lists*/
		if(!isList(list->type))
			goto CONTINUE_NEXT;

		match = patternMatchL((CELL *)pattern->contents, (CELL *)list->contents, TRUE);

		if(match == NULL || match == nilCell)
			goto CONTINUE_NEXT;

		deleteList(match);
		}
	else
		{
		cleanupResults(resultIdxSave);
		if(compareFunc(pattern, list, funcCell) != 0)
			goto CONTINUE_NEXT;
		}

	deleteList((CELL*)sysSymbol[0]->contents);
	sysSymbol[0]->contents = (UINT)copyCell(list);

	if(exprCell != nilCell)
		{
		if((exprRes = evaluateExpressionSafe(exprCell, &errNo)) == NULL)
       		{
			pushResult(result); /* push for later deletion */
   			longjmp(errorJump, errNo);
   			}
		}
	else 
		exprRes = list;

	exprRes = copyCell(exprRes);


	if(result == nilCell)
		{
		cell = exprRes;
		result = makeCell(CELL_EXPRESSION, (UINT)cell);
		}
	else
		{
		cell->next = exprRes;
		cell = cell->next;
		}
	
	CONTINUE_NEXT:	
	list = list->next;
	}

if(result == nilCell)
	return(getCell(CELL_EXPRESSION));

return(result);
}


CELL * p_findAll(CELL * params)
{
CELL * key;
CELL * space;

key = evaluateExpression(params);
params = getDefaultOrEval(params->next, &space);

if(key->type == CELL_STRING && space->type == CELL_STRING)
	return(findAllString((char *)key->contents, 
			(char *)space->contents,  (size_t) space->aux - 1, params));

if(!isList(space->type))
	return(errorProcExt(ERR_LIST_EXPECTED, space));

return(findAllList(key, (CELL *)space->contents, params));
}


void swap(UINT * left, UINT * right)
{
UINT tmp;

tmp = *left;
*left = *right;
*right = tmp;
}

SYMBOL * getSymbolCheckProtected(CELL * params)
{
SYMBOL * sPtr = NULL;

if(params->type == CELL_DYN_SYMBOL)
	sPtr = getDynamicSymbol(params);
else if(params->type == CELL_SYMBOL)
	sPtr = (SYMBOL *)params->contents;
else fatalError(ERR_SYMBOL_EXPECTED, params, FALSE);

if(isProtected(sPtr->flags))
	fatalError(ERR_SYMBOL_PROTECTED, params, FALSE);

return sPtr;
}

CELL * p_swap(CELL * params)
{
ssize_t first, second, num;
char * str;
CELL * envelope;
CELL * list;
CELL * firstCell;
CELL * secondCell;
SYMBOL * lsym;
SYMBOL * rsym;

if(((CELL *)params->next)->next == nilCell)
	{
	lsym = getSymbolCheckProtected(params);
	rsym = getSymbolCheckProtected(params->next);
	swap(&lsym->contents, &rsym->contents);
	return(copyCell((CELL*)rsym->contents));
	}

params = getInteger(params, (UINT*)&first);
params = getInteger(params, (UINT*)&second);

envelope = evalCheckProtected(params, NULL);

if(envelope->type == CELL_STRING)
	{
	first = adjustNegativeIndex(first, envelope->aux - 1);
	second = adjustNegativeIndex(second, envelope->aux - 1);
	str = (char *)envelope->contents;
	num = str[first];
	str[first] = str[second];
	str[second] = num;
	return(copyCell(envelope));
	}

if(!isList(envelope->type))
	return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, params));

envelope->aux = (UINT)nilCell; /* undo last element optimization */

list = (CELL *)envelope->contents;

if(first < 0) first = convertNegativeOffset(first, list);
if(second < 0) second = convertNegativeOffset(second, list);

if(first > second) swap((UINT*)&first, (UINT*)&second);
second = second - first;
	
firstCell = list;
while(first--)
	{
	if(firstCell->next == nilCell) break;
	firstCell = firstCell->next;
	}	
if(first >= 0) return(errorProc(ERR_LIST_INDEX_OUTOF_BOUNDS));

secondCell = firstCell;

while(second--)
	{
	if(secondCell->next == nilCell) break;
	secondCell = secondCell->next;
	}
if(second >= 0) return(errorProc(ERR_LIST_INDEX_OUTOF_BOUNDS));

swap(&firstCell->type, &secondCell->type);
swap(&firstCell->contents, &secondCell->contents);
swap(&firstCell->aux, &secondCell->aux);

return(copyCell(envelope));
}


CELL * p_dup(CELL * params)
{
CELL * list;
CELL * expr;
char * str;
ssize_t n, len;

expr = evaluateExpression(params);
params = params->next;
if(params != nilCell)
	getInteger(params, (UINT *)&n);
else n = 2;

if(n < 0) n = 0;

if(expr->type == CELL_STRING && !getFlag(params->next) )
	{
	len = expr->aux - 1;
	list = getCell(CELL_STRING);
	str = allocMemory(len * n + 1);
	list->contents = (UINT)str;
	list->aux = (len * n + 1);
	*(str + len * n) = 0;
	while(n--) 
		{
		memcpy(str, (char *)expr->contents, len);
		str += len;
		}
	return(list);
	}

list = getCell(CELL_EXPRESSION);
if(n-- > 0) 
	{
	list->contents = (UINT)copyCell(expr);

	params = (CELL *)list->contents;
	while(n--)
		{
		params->next = copyCell(expr);
		params = params->next;
		}
	}

return(list);
}


#define STARTS_WITH 0
#define ENDS_WITH 1

CELL * startsEndsWith(CELL * params, int type)
{
char * string;
char * key;
char * keydollar;
long options = -1;
size_t slen, pos;
int klen;
CELL * cell, * list;

cell = params->next;
getDefaultOrEval(params, &list);
if(list->type == CELL_STRING)
    {
    string = (char *)list->contents;
    getString(cell, &key);
    }
else
    {
    if(!isList(list->type))
        errorProcExt(ERR_LIST_OR_STRING_EXPECTED, params);

    cell = evaluateExpression(cell);

    list = (CELL *)list->contents;
   
    if(type == ENDS_WITH)
        while(list->next != nilCell) list = list->next;

    if(compareCells(list, cell) == 0) return(trueCell);
    else return(nilCell);
    }

if(cell->next != nilCell)
	{
	cell = evaluateExpression(cell->next);
	if(isNil(cell) )
		options = 1;
	else 
		getIntegerExt(cell, (UINT*)&options, FALSE);
	}

klen = strlen(key);
slen = strlen(string);

if(type == STARTS_WITH)
	{
	if(options == -1) 
		{
		if(strncmp(string, key, (size_t)klen) == 0)
			return(trueCell);
		}
	else  
		{
        if(searchBufferRegex(string, 0, key, slen, options, 0) == 0)
			return(trueCell);
		}
	return(nilCell);
	}


if((options == -1) && (klen > slen)) return(nilCell);

if(options == -1) 
	{
	if(strncmp(string + slen - klen, key, klen) == 0)
		return(trueCell);
	}
else
	{
	/* append $ to the pattern for anchoring at the end */
	keydollar = malloc(klen + 4);
	*keydollar = '(';
	memcpy(keydollar + 1, key, klen);
	memcpy(keydollar + 1 + klen, ")$", 2);
	*(keydollar + klen + 3) = 0;
	klen = klen + 3;
    if((pos = searchBufferRegex(string, 0, keydollar, slen, options, &klen)) != -1)	
		{
		free(keydollar);
		if(pos + klen == slen)
			return(trueCell);
		}
	free(keydollar);
	}

return(nilCell);
}

CELL * p_startsWith(CELL * params) { return startsEndsWith(params, STARTS_WITH); }
CELL * p_endsWith(CELL * params) { return startsEndsWith(params, ENDS_WITH); }

CELL * p_replace(CELL * params)
{
CELL * keyCell;
CELL * repCell;
CELL * funcCell = NULL;
CELL * list;
CELL * cell;
CELL * newList;
char * keyStr;
char * buff;
char * newBuff;
UINT cnt; 
size_t newLen;
long options;
int resultIdxSave;

keyCell = evaluateExpression(params);
params = params->next;

newList = cell = evalCheckProtected(params, NULL);

cnt = 0;
resultIdxSave = resultStackIdx;
if(isList(cell->type))
	{
	cell->aux = (UINT)nilCell; /* undo last element optimization */

	list = (CELL *)cell->contents;

	if(params->next != nilCell)
		{
		params = params->next;
		repCell = params;
		if(params->next != nilCell)
			funcCell = evaluateExpression(params->next);
		}
	else
		repCell = NULL;
COMPARE_START:
	if(compareFunc(keyCell, list, funcCell) == 0)
		{
		if(repCell != NULL)
			{
			deleteList((CELL*)sysSymbol[0]->contents);
			sysSymbol[0]->contents = (UINT)copyCell(list);
			cell->contents = (UINT)copyCell(evaluateExpression(repCell));
			cell = (CELL*)cell->contents;
			cell->next = list->next;
			}
		else /* remove mode */
			cell->contents = (UINT)list->next;

		list->next = nilCell;
		deleteList(list);
		cnt++;

		if(repCell != NULL)
			list = cell;
		else /* remove mode */
			{
			list = (CELL*)cell->contents;
			goto COMPARE_START;
			}		
		}
	
	while(list->next != nilCell)
		{
		if(compareFunc(keyCell, list->next, funcCell) == 0)
			{
			cell = list->next;	/* cell = old elmnt */
			if(repCell != NULL)
				{
				deleteList((CELL*)sysSymbol[0]->contents);
				sysSymbol[0]->contents = (UINT)copyCell(cell);
				list->next = copyCell(evaluateExpression(repCell));
				list = list->next;
				}
			list->next = cell->next;
			cell->next = nilCell;
			deleteList(cell);
			cnt++;
			}		
		else	
			list = list->next;
		cleanupResults(resultIdxSave);
		}

	deleteList((CELL*)sysSymbol[0]->contents);	
	sysSymbol[0]->contents = (UINT)stuffInteger(cnt);
	return(copyCell(newList));
	}

if(cell->type == CELL_STRING)
	{
	if(keyCell->type != CELL_STRING)
		return(errorProc(ERR_STRING_EXPECTED));
	keyStr = (char *)keyCell->contents;
	buff = (char *)cell->contents;
	repCell = params->next;

	if(repCell == nilCell)
		return(errorProc(ERR_MISSING_ARGUMENT));
			
	options = -1;
	if(repCell->next != nilCell)
            getInteger(repCell->next, (UINT*)&options);

	newBuff = replaceString(keyStr, keyCell->aux - 1, 
	                       buff, (size_t)cell->aux -1, repCell, &cnt, options, &newLen);
	if(newBuff != NULL)
	    {
	    freeMemory(buff);
	    cell->contents = (UINT)newBuff;
	    cell->aux = newLen + 1;
	    }

	deleteList((CELL*)sysSymbol[0]->contents);	
	sysSymbol[0]->contents = (UINT)stuffInteger(cnt);
	return(copyCell(cell));
	}

return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, params));
}



CELL * p_rotate(CELL * params)
{
CELL * cell;
CELL * previous;
CELL * last = NULL;
size_t length, index;
size_t count;

cell = params;

if(cell->next != nilCell) getInteger(cell->next, (UINT *)&count);
else count = 1;

params = evalCheckProtected(params, NULL);

if(params->type == CELL_STRING)
	{	
	cell = copyCell(params);	
	length = params->aux - 1;
	if((count = adjustCount(count, length)) == 0) return(cell);
	memcpy((char*)cell->contents, (char *)(params->contents + length - count), count);
	memcpy((char*)(cell->contents + count), (char *)params->contents, length - count);
	memcpy((char*)params->contents, (char*)cell->contents, length);
	return(cell);
	}	

if(!isList(params->type))
	return(errorProcExt(ERR_LIST_EXPECTED, cell));

params->aux = (UINT)nilCell; /* undo last element optimization */

cell = (CELL *)params->contents;
length = 0;
while(cell != nilCell)
	{
	++length;
	last = cell;
	cell = cell->next;
	}

if((count = adjustCount(count, length))== 0) 
	return(copyCell(params));
index = length - count;

previous = cell = (CELL *)params->contents;
while(index--) 
	{
	previous = cell;
	cell = cell->next;
	}

previous->next = nilCell;
last->next = (CELL *)params->contents;
params->contents = (UINT)cell;

return(copyCell(params));
}

/* eof */
