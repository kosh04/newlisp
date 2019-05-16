/* nl-liststr.c --- newLISP primitives handling lists and strings


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
#include "protos.h"

extern CELL * lastCellCopied;
extern CELL * countCell;
extern SYMBOL * sysSymbol[];
extern SYMBOL * countSymbol;
extern void printResultStack();

/* used only on string indices */
size_t adjustNegativeIndex(ssize_t index, size_t length)
{
if(index < 0) index = length + index;
else if((index+1) > length) errorProc(ERR_STRING_INDEX_INVALID);

if(index < 0) errorProc(ERR_STRING_INDEX_INVALID);

return(index);
}

size_t adjustCount(ssize_t count, ssize_t length)
{
if(length <= 1 || count == 0 || length == labs(count))
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
extern char * strcasestr(const char * haystack, const char * needle);
#endif
CELL * p_member(CELL * params)
{
CELL * key;
CELL * list;
INT options  = -1;
char * ptr;
ssize_t pos;

key = evaluateExpression(params);

params = getEvalDefault(params->next, &list);

if(params != nilCell)
    getInteger(params, (UINT *)&options);

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
#ifdef BIGINT
int * bigintPtr;
int * result;
int len;
#else
INT64 number;
double fnum;
#endif

params = evaluateExpression(params);
length = 0;
switch(params->type)
    {
#ifdef BIGINT
    case CELL_LONG:
#ifndef NEWLISP64
    case CELL_INT64:
#endif
    case CELL_FLOAT:
    case CELL_BIGINT:   
        result = getBigintSizeDirect(params, &bigintPtr, &len);
        length = lengthBigint(bigintPtr, len);
        if(result) free(result);
        break; 
#else /* not BIGINT */
    case CELL_LONG:
#ifndef NEWLISP64
    case CELL_INT64:
#endif
        getInteger64Ext(params, &number, FALSE);
        if(number == 0)
            length = 1;
        else
            {
            if(number < 0) number = - number;
            length = log(number) / log(10) + 1.5;
            }
        break;
    case CELL_FLOAT:
        getFloat(params, &fnum);
        if(fnum == 0.0)
            length = 1;
        else
            {
            if(fnum < 0.0) fnum = - fnum;
            length = log(fnum) / log(10) + 1.5;
            }
        break;
#endif /* not BIGINT */
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
        else if(params->type == CELL_ARRAY)
            length = (params->aux -1) / sizeof(UINT);
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
    case CELL_FEXPR:
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
    params = getEvalDefault(params, &cell);
    if(!isList(cell->type))
        {
        if(copy == NULL)
            {
            if(cell->type == CELL_STRING)
                return(appendString(cell, params, NULL, 0, FALSE, TRUE));
            else if(cell->type == CELL_ARRAY)
                return(appendArray(cell, params));
            return(errorProcExt(ERR_ARRAY_LIST_OR_STRING_EXPECTED, cell));
            }
        
        return(errorProcExt(ERR_LIST_EXPECTED, cell));
        }

    if(list == NULL)
        list = getCell(cell->type);

    copy = copyList((CELL *)cell->contents);

    if(copy == nilCell) continue;

    if(firstCell == NULL) list->contents = (UINT)copy;
    else firstCell->next = copy;

    firstCell = lastCellCopied;
    }

if(list == NULL)
    return(getCell(CELL_EXPRESSION));

symbolCheck = NULL;
list->aux = (UINT)lastCellCopied; /* last element optimization */
return(list);
}


CELL * appendString(CELL * cell, CELL * list, char * joint, size_t jointLen, int trailJoint, int evalFlag)
{ 
CELL * result;
STREAM stream = {NULL, NULL, 0, 0, 0};
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

result = stuffStringN(stream.buffer, stream.position);

closeStrStream(&stream);

symbolCheck = NULL;
return(result);
}


CELL * p_extend(CELL * params)
{
CELL * target;
CELL * head;
CELL * tail;
SYMBOL * symbolRef;
char * pStr;
size_t size;

params = getEvalDefault(params, &target);
if((symbolRef = symbolCheck))
    {
    if(isProtected(symbolRef->flags))
        return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolRef)));
    if(isNil((CELL *)symbolRef->contents))
        {
        head = evaluateExpression(params); /* extension */
        if(isList(head->type) || head->type == CELL_STRING)
            {
            target = copyCell(head);
            deleteList((CELL*)symbolRef->contents);
            symbolRef->contents = (UINT)target;
            }
        params = params->next;
        }
    }

if(isList(target->type))
    {
    tail = (CELL *)target->aux;
    target->aux = (UINT)nilCell;
    if(tail == nilCell)
        {
        tail = (CELL *)target->contents;
        while(tail->next != nilCell)
            tail = tail->next;
        }

    while(params != nilCell)
        {   
        params = getListHead(params, &head);
        if(head == nilCell) continue;
        if(target->contents == (UINT)nilCell)
            {
            target->contents = (UINT)copyList(head);
            tail = lastCellCopied;
            }
        else
            {
            tail->next = copyList(head);
            target->aux = (UINT)lastCellCopied;
            tail = (CELL *)target->aux;
            }
        }
        
    }   
else if(target->type == CELL_STRING)
    {
    while(params != nilCell)
        {
        params = getStringSize(params, &pStr, &size, TRUE);
        appendCellString(target, pStr, size);
        }
    }
else return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, target));

symbolCheck = symbolRef;
pushResultFlag = FALSE;
return(target);
}



CELL * p_chop(CELL * params)
{
size_t number = 1;
size_t length = 0;
CELL * next;
#ifdef SUPPORT_UTF8
char * ptr;
#endif

next = getEvalDefault(params, &params);

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
    length = utf8_wlen((char *)params->contents, (char *)params->contents + params->aux);
    if(number > length) number = length;
    length = length - number;
    ptr = (char *)params->contents;
    ptr = utf8_index(ptr, length);
    return stuffStringN((char *)params->contents, ptr - (char *)params->contents);
#endif
    }

if(!isList(params->type))
    return(errorProc(ERR_LIST_OR_STRING_EXPECTED)); 

length = listlen((CELL *)params->contents);
if(number > length) number = length;

return(sublist((CELL *)params->contents, 0, length - number));
}


CELL * p_nth(CELL * params)
{
CELL * list; 
CELL * cell;
CELL * (*implicitIndexFunc)(CELL *, CELL *);
SYMBOL * symbolRef;

cell = getEvalDefault(params->next, &list); /* list or string to be indexed */
symbolRef = symbolCheck;

params = copyCell(params); /* indices */
pushResult(params);

if(isList(list->type)) 
    implicitIndexFunc = implicitIndexList;
else if(list->type == CELL_ARRAY) 
    implicitIndexFunc = implicitIndexArray;
else if(list->type == CELL_STRING)
    {
    list = implicitIndexString(list, params);
    symbolCheck = symbolRef;
    pushResult(list);
    pushResultFlag = FALSE;
    return(list);
    }
        
else return(errorProcExt(ERR_LIST_EXPECTED, list));

cell = (*implicitIndexFunc)(list, params);
symbolCheck = symbolRef;
pushResultFlag = FALSE;
return(cell);
}


#define INSERT_BEFORE 0
#define INSERT_AFTER 1
#define INSERT_END 2
CELL * p_push(CELL * params)
{
CELL * newCell;
CELL * list;
CELL * cell = NULL;
CELL * listOrg;
SYMBOL * symbolRef;
int insert = 0, evalFlag = 0;
ssize_t index;

newCell = evaluateExpression(params);
params = getEvalDefault(params->next, &list);
listOrg = list;

if((symbolRef = symbolCheck))
    {
    if(isProtected(symbolCheck->flags))
        return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));
    if(isNil((CELL *)symbolCheck->contents))
        {
        deleteList((CELL*)symbolCheck->contents);
        listOrg = makeCell(CELL_EXPRESSION, (UINT)copyCell(newCell));
        symbolCheck->contents = (UINT)listOrg;
        goto PUSH_RETURN;
        }
    }                                   

if(!isList(list->type))
    {
    if(list->type == CELL_STRING)
        {
        pushOnString(newCell, list, params);
        goto PUSH_RETURN;
        }   
    else
        return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, list));
    }

if(params == nilCell) 
    index = 0;
else 
    {
    cell = (CELL*)params->next;
    params = evaluateExpression(params);
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
            cell->next = newCell;
        else if(list->contents == (UINT)nilCell)
            list->contents = (UINT)newCell;
        else
            {
            cell = (CELL *)list->contents;
            while(cell->next != nilCell)
                cell = cell->next;
            cell->next = newCell;
            }
        goto PUSH_RETURN;
        }
    }
    

while(isList(list->type))
    {
    list->aux = (UINT)nilCell; /* undo last element optimization */
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
        else errorProc(ERR_LIST_INDEX_INVALID);
        }
    else insert = INSERT_BEFORE;

    while(index--) 
        {
        if(list == nilCell)
            {
            if(index >= 0) errorProc(ERR_LIST_INDEX_INVALID);
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

PUSH_RETURN:
symbolCheck = symbolRef;
pushResultFlag = FALSE;
return(listOrg);
}


CELL * p_pop(CELL * params)
{
CELL * list;
CELL * envelope = NULL; /* suppress bogus warning on some compilers */
CELL * cell;
ssize_t index;
int evalFlag = FALSE;

params = getEvalDefault(params, &list);
if(symbolCheck && isProtected(symbolCheck->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));

if(!isList(list->type))
    {
    if(list->type == CELL_STRING)
        return(popString(list, params));
    else
        return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, list));
    }

/* no index, popping first */
if(params == nilCell)
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
    cell = (CELL*)params->next;
    params = evaluateExpression(params);
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

/* pop with index */
while(isList(list->type))
    {
    cell = envelope = list;
    list = (CELL *)list->contents;

    if(index < 0) index = convertNegativeOffset(index, list);

    while(index--) 
        {
        cell = list;
        list = list->next;
        }
    if(list == nilCell) 
        errorProc(ERR_LIST_INDEX_INVALID);

    if(params == nilCell || !isList(list->type)) break;
    params = getIntegerExt(params, (UINT*)&index, evalFlag);
    }

if(list->next == nilCell) /* last cell is popped */
    {
    if(list == (CELL*)cell->contents) /* last is also first cell */
        envelope->aux = (UINT)nilCell;        
    else 
        envelope->aux = (UINT)cell; /* cell is previous to last popped */
    }

if(list == (CELL*)cell->contents)
    cell->contents = (UINT)list->next;
else
    cell->next = list->next;

list->next = nilCell;
return(list);
}


CELL * popString(CELL * str, CELL * params)
{
char * ptr;
char * newPtr;
ssize_t index = 0;
ssize_t len = 1;
ssize_t size;
CELL * result;

ptr = (char *)str->contents;

#ifdef SUPPORT_UTF8
size = utf8_wlen(ptr, (char *)str->contents + str->aux);
#else
size = str->aux - 1;
#endif

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

index = adjustNegativeIndex(index, size);
if((index + len) > size)
    len = size - index;
    
#ifdef SUPPORT_UTF8
newPtr = utf8_index(ptr, index);
index = newPtr - ptr;

newPtr = utf8_index(newPtr, len);
len = newPtr - ptr;

if(len > str->aux - 1)
    return(errorProc(ERR_INVALID_UTF8));
len -= index;
#endif

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
int len;
#ifdef SUPPORT_UTF8
char * sptr;
#endif

if(idx != nilCell) getInteger(idx, (UINT*)&index);
ptr = (char *)str->contents;

if(newStr->type != CELL_STRING)
    return(errorProcExt(ERR_STRING_EXPECTED, newStr));

if(index == -1)
    {
    appendCellString(str, (char *)newStr->contents, newStr->aux - 1);
    return(newStr);
    }

#ifndef SUPPORT_UTF8
len = str->aux - 1;
#else
len = utf8_wlen(ptr, ptr + str->aux);
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
sptr = utf8_index(ptr, index);
memcpy(newPtr, ptr, sptr - ptr);
memcpy(newPtr + (sptr - ptr), (char*)newStr->contents, newStr->aux - 1);
memcpy(newPtr + (sptr - ptr) + newStr->aux - 1, sptr, str->aux - (sptr - ptr)  );
#endif

str->contents = (UINT)newPtr;
str->aux = str->aux + newStr->aux - 1;
*(newPtr + str->aux - 1) = 0;
free(ptr);

return(newStr);
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

params = getEvalDefault(params, &head);
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
    free(wstr); free(wnewStr);
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
    while(idx < index  && list != nilCell) list = list->next, idx++; 
    if(list == nilCell) 
        errorProc(ERR_LIST_INDEX_INVALID);
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

params = getEvalDefault(params, &cell);
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
CELL * list;
CELL * previous;
CELL * next;
char * str;
char * left;
char * right;
CELL * * addr;
CELL * * leftA;
CELL * * rightA;
size_t len, tmp;

cell = params;
getEvalDefault(params, &list);
if(symbolCheck && isProtected(symbolCheck->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));

if(isList(list->type))
    {
    list->aux = (UINT)nilCell; /* undo last element optimization */

    previous = cell = (CELL*)list->contents;
    next = cell->next;
    cell->next = nilCell;
    while(cell!= nilCell)
        {
        previous = cell;
        cell = next;
        next = cell->next;
        if(cell != nilCell) cell->next = previous;
        }
    list->contents = (UINT)previous;
    }

else if(list->type == CELL_STRING)
    {
    str = (char *)list->contents;
    len = list->aux - 1;
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

else if(list->type == CELL_ARRAY)
    {
    addr = (CELL * *)list->contents;
    len = (list->aux - 1) / sizeof(UINT);
    leftA = addr;
    rightA = leftA + len - 1;
    while(leftA < rightA)
        {
        cell = *leftA;
        *leftA = *rightA;
        *rightA = cell;
        leftA++;
        rightA--;
        }
    }
    
else return(errorProcExt(ERR_ARRAY_LIST_OR_STRING_EXPECTED, cell));

pushResultFlag = FALSE;
return(list);
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
char * str;
ssize_t found;
CELL * next;
CELL * keyCell;
CELL * funcCell;
size_t size;
INT options = -1;
size_t offset = 0;
UINT * resultIdxSave;

keyCell = evaluateExpression(params);
params = getEvalDefault(params->next, &next);

if(keyCell->type == CELL_STRING && next->type == CELL_STRING)
    {
    key = (char *)keyCell->contents;
    str = (char *)next->contents;
    size = next->aux - 1;

    if(params != nilCell)
        {
        if(params->next != nilCell)
            getInteger(params->next, (UINT*)&offset);
        if(offset > size) offset = size;
        params = evaluateExpression(params);
        if(!isNil(params))
            /* 10.6.1 also accept string for options */
            parseRegexOptions(params, (UINT *)&options, FALSE);
        }

    if(options == -1)
        found = searchBuffer(str + offset, size - offset, key, keyCell->aux - 1, TRUE);
    else
        found = searchBufferRegex(str, offset, key, size, options, NULL) - offset;
    if(found < 0) return(nilCell);
    }
else
    {
    /* list mode with optional functor */
    if(!isList(next->type)) 
        return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, next));
    next = (CELL *)next->contents;
    found = 0;

    if(params != nilCell)
        funcCell = evaluateExpression(params);
    else funcCell = NULL;

    /* do regex when first arg is string and option# is present */
    if(funcCell && 
        (isNumber(funcCell->type) || funcCell->type == CELL_STRING) &&
        keyCell->type == CELL_STRING)
        {
        /* 10.6.1 also accept string for options */
        parseRegexOptions(funcCell, (UINT *)&options, FALSE);
        key = (char *)keyCell->contents;
        while(next != nilCell)
            {
            if(next->type == CELL_STRING)
                {
                if(searchBufferRegex((char *)next->contents, 0, 
                        key, next->aux - 1 , options, NULL) != -1) break;
                }
            found++;
            next = next->next;
            }
        if(next == nilCell) return(nilCell);
        else return(stuffInteger(found));
        }

    resultIdxSave = resultStackIdx;
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
        cleanupResults(resultIdxSave);
        }
    if(next == nilCell) return(nilCell);
    }

return(stuffInteger(found + offset));
}

/* ------- find-all ---- finds all strings matching a pattern in a list ----- */

CELL * findAllString(char * pattern, char * str, size_t size, CELL * params)
{
INT options = 0;
ssize_t findPos = -1;
ssize_t lastPos = -1;
CELL * result = nilCell;
CELL * exprCell;
CELL * exprRes;
CELL * cell = NULL;
UINT * resultIdxSave;
jmp_buf errorJumpSave;
int errNo; 
size_t len;
size_t offset = 0;

exprCell = params;
if((params = params->next) != nilCell)
    /* 10.6.1 also accept string for options */
    parseRegexOptions(params, (UINT *)&options, TRUE);

resultIdxSave = resultStackIdx;
countCell->contents = 0;
    
if(exprCell != nilCell)
    {
    memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
    if((errNo = setjmp(errorJump)) != 0)
        {
        memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
        if(result != nilCell) deleteList(result);
        longjmp(errorJump, errNo);
        }   
    }

while( (findPos = searchBufferRegex(str, offset, pattern, size, options, &len)) 
       != -1)
    {
    countCell->contents++;
    if(exprCell != nilCell)
        {
        itSymbol->contents = sysSymbol[0]->contents;
        exprRes = evaluateExpression(exprCell);
        exprRes = copyCell(exprRes);
        }
    else
        exprRes = stuffStringN(str + findPos, len);

    if(lastPos == findPos)
        {
        ++findPos;
        pushResult(exprRes);
        goto FINDALL_CONTINUE;
        }   

    lastPos = findPos;

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

    FINDALL_CONTINUE:
    offset = (findPos + len);
    if(findPos > size) break;
    cleanupResults(resultIdxSave);
    }

itSymbol->contents = (UINT)nilCell;

if(exprCell != nilCell)
    memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));

return(result == nilCell ? getCell(CELL_EXPRESSION) : result);
}


CELL * findAllList(CELL * pattern, CELL * list, CELL * exprCell)
{
CELL * result;
CELL * exprRes;
CELL * match;
CELL * funcCell;
UINT * resultIdxSave;
jmp_buf errorJumpSave;
int errNo;

funcCell = evaluateExpression(exprCell->next);
resultIdxSave = resultStackIdx;
countCell->contents = 0;

result = getCell(CELL_EXPRESSION);

memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
if((errNo = setjmp(errorJump)) != 0)
    {
    memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
    deleteList(result);
    longjmp(errorJump, errNo);
    }

while(list != nilCell)
    {
    if(funcCell == nilCell)
        {
        /* added in 10.4.7, this makes exp in func in 3rd syntax optional */
        if(!isList(pattern->type))
            {
            if(compareFunc(pattern, list, NULL) != 0)
                goto CONTINUE_NEXT;
            }
        else 
            {
            /* match only takes lists*/
            if(!isList(list->type))
                goto CONTINUE_NEXT;

            match = patternMatchL((CELL *)pattern->contents, (CELL *)list->contents, TRUE);

            if(match == NULL || match == nilCell)
                goto CONTINUE_NEXT;

            deleteList(match);
            }
        }
    else
        {
        cleanupResults(resultIdxSave);
        if(compareFunc(pattern, list, funcCell) != 0)
            goto CONTINUE_NEXT;
        }

    countCell->contents++;
    itSymbol->contents = (UINT)list;

    if(exprCell != nilCell)
        exprRes = evaluateExpression(exprCell);
    else 
        exprRes = list;

    addList(result, copyCell(exprRes));
    /* increment $count here */

    CONTINUE_NEXT:  
    list = list->next;
    }

memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));

itSymbol->contents = (UINT)nilCell;
return(result);
}


CELL * p_findAll(CELL * params)
{
CELL * key;
CELL * space;

key = evaluateExpression(params);
params = getEvalDefault(params->next, &space);

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


CELL * getRefCheckProtected(CELL * params)
{
CELL * ref;

ref = evaluateExpression(params);

if(ref == nilCell || ref == trueCell)
    errorProcExt(ERR_IS_NOT_REFERENCED, ref);

if(symbolCheck != NULL)
    {
    if(isProtected(symbolCheck->flags))
        return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));
    }

return(ref);
}


CELL * p_swap(CELL * params)
{
CELL * firstCell;
CELL * secondCell;

firstCell = getRefCheckProtected(params);
secondCell = getRefCheckProtected(params->next);
swap(&firstCell->type, &secondCell->type);
swap(&firstCell->contents, &secondCell->contents);
swap(&firstCell->aux, &secondCell->aux);
pushResultFlag = FALSE;
return(secondCell);
}


CELL * p_dup(CELL * params)
{
CELL * list;
CELL * expr;
char * str;
ssize_t n, len;

expr = evaluateExpression(params);
if((params = params->next) != nilCell)
    getInteger(params, (UINT *)&n);
else 
    {
    n = 2;
    symbolCheck = NULL;
    }

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
INT options = -1;
size_t slen, pos;
size_t klen;
CELL * cell, * list;

cell = params->next;
getEvalDefault(params, &list);
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
    /* 10.6.1 also accept string for options */
    parseRegexOptions(cell, (UINT *)&options, FALSE);
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
        if(pos + klen == slen)
            {
            free(keydollar);
            return(trueCell);
            }
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
size_t newLen;
INT options;
UINT * resultIdxSave;
SYMBOL * refSymbol;

keyCell = copyCell(evaluateExpression(params));
pushResult(keyCell);
params = getEvalDefault(params->next, &cell);
newList = cell;
refSymbol = symbolCheck;
if(symbolCheck && (isProtected(symbolCheck->flags) || isBuiltin(symbolCheck->flags)))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));

countCell->contents = 0;
resultIdxSave = resultStackIdx;
if(isList(cell->type))
    {
    cell->aux = (UINT)nilCell; /* undo last element optimization */

    list = (CELL *)cell->contents;

    if(params != nilCell)
        {
        repCell = params;
        if(params->next != nilCell)
            funcCell = evaluateExpression(params->next);
        }
    else
        repCell = NULL;

COMPARE_START:
    if(compareFunc(keyCell, list, funcCell) == 0)
        {
        countCell->contents++;
        if(repCell != NULL)
            {
            itSymbol->contents = (UINT)list;
            cell->contents = (UINT)copyCell(evaluateExpression(repCell));
            cell = (CELL*)cell->contents;
            cell->next = list->next;
            }
        else /* remove mode */
            cell->contents = (UINT)list->next;

        list->next = nilCell; /* decouple  and delete old */
        deleteList(list);

        if(repCell != NULL)
            list = cell;
        else /* remove mode */
            {
            list = (CELL*)cell->contents;
            if(list != nilCell)
                goto COMPARE_START;
            }       
        }
    
    while(list->next != nilCell)
        {
        if(compareFunc(keyCell, list->next, funcCell) == 0)
            {
            countCell->contents++;
            cell = list->next;  /* cell = old elmnt */
            if(repCell != NULL)
                {
                itSymbol->contents = (UINT)cell;
                list->next = copyCell(evaluateExpression(repCell));
                list = list->next;
                }
            list->next = cell->next;
            cell->next = nilCell;
            deleteList(cell);
            }       
        else    
            list = list->next;

        cleanupResults(resultIdxSave);
        }

    itSymbol->contents = (UINT)nilCell;
    symbolCheck = refSymbol;
    pushResultFlag = FALSE;
    return(newList);
    }

if(cell->type == CELL_STRING)
    {
    if(keyCell->type != CELL_STRING)
        return(errorProc(ERR_STRING_EXPECTED));
    keyStr = (char *)keyCell->contents;
    buff = (char *)cell->contents;
    repCell = params;

    if(repCell == nilCell)
        return(errorProc(ERR_MISSING_ARGUMENT));
            
    options = -1;
    if(repCell->next != nilCell)
            /* 10.6.1 also accept string for options */
            parseRegexOptions(repCell->next, (UINT *)&options, TRUE);

    newBuff = replaceString(keyStr, keyCell->aux - 1, 
                           buff, (size_t)cell->aux -1, repCell, &countCell->contents, options, &newLen);
    if(newBuff != NULL)
        {
        freeMemory(buff);
        cell->contents = (UINT)newBuff;
        cell->aux = newLen + 1;
        }

    symbolCheck = refSymbol;
    pushResultFlag = FALSE;
    return(cell);
    }

return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, cell));
}



CELL * p_rotate(CELL * params)
{
CELL * cell;
CELL * list;
CELL * previous;
CELL * last = NULL;
char * str;
size_t length, index;
size_t count;

cell = params;

if(cell->next != nilCell) getInteger(cell->next, (UINT *)&count);
else count = 1;

getEvalDefault(params, &list);
if(symbolCheck && isProtected(symbolCheck->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));

if(list->type == CELL_STRING)
    {   
    length = list->aux - 1;
    if((count = adjustCount(count, length)) != 0) 
        {
    	str = allocMemory(list->aux);  
    	memcpy(str, (char *)(list->contents + length - count), count);
    	memcpy(str + count, (char *)list->contents, length - count);
    	memcpy((char*)list->contents, str, length);
    	free(str);
        }
    pushResultFlag = FALSE;
    return(list);
    }   

if(!isList(list->type))
    return(errorProcExt(ERR_LIST_EXPECTED, cell));

list->aux = (UINT)nilCell; /* undo last element optimization */

cell = (CELL *)list->contents;
length = 0;
while(cell != nilCell)
    {
    ++length;
    last = cell;
    cell = cell->next;
    }

if((count = adjustCount(count, length))== 0) 
    {
    pushResultFlag = FALSE;
    return(list);
    }
    
index = length - count;

previous = cell = (CELL *)list->contents;
while(index--) 
    {
    previous = cell;
    cell = cell->next;
    }

previous->next = nilCell;
last->next = (CELL *)list->contents;
list->contents = (UINT)cell;

pushResultFlag = FALSE;
list->aux = (UINT)previous; /* last element optimization */
return(list);
}

/* eof */
