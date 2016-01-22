/* n-list.c

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

extern SYMBOL * starSymbol;
extern SYMBOL * plusSymbol;
extern CELL * countCell;

extern CELL * firstFreeCell;

/* following used in count, difference, intersect, unique and sort 8.6.2 */
CELL * * listToSortedVector(CELL * list, ssize_t * length, CELL * func, int indexFlag);
CELL * resortVectorToList(CELL * * vector, ssize_t length);
void binsort(CELL * * x, ssize_t n, CELL * pCell);


CELL * p_map(CELL * params)
{
CELL * argsPtr;
CELL * arg;
CELL * argCell;
CELL * funcPtr;
CELL * cell;
CELL * expr;
CELL * results;
CELL * res;
CELL * cellIdx;
UINT * resultIdxSave;

funcPtr = evaluateExpression(params);

/* get first of argument lists */
params = getEvalDefault(params->next, &cell);

argsPtr = cell = (cell->type == CELL_ARRAY) ?  
    arrayList(cell, FALSE) : copyCell(cell);

if(!isList(cell->type))
    return(errorProcExt(ERR_LIST_EXPECTED, cell));

/* get rest of argument lists */
while (params != nilCell)
    {
    params = getEvalDefault(params, &results);
    cell->next = (results->type == CELL_ARRAY) ? 
        arrayList(results, FALSE) : copyCell(results);

    cell = cell->next;
    if(!isList(cell->type))
        return(errorProcExt(ERR_LIST_EXPECTED, results));
    }

results = getCell(CELL_EXPRESSION);
res = NULL;

cellIdx = initIteratorIndex();

expr = getCell(CELL_EXPRESSION);
cell = copyCell(funcPtr);
expr->contents = (UINT)cell;

/* prepeare for deletion now, in case of error 
   in mapped function execution */

pushResult(argsPtr);
pushResult(expr);
pushResult(results);

resultIdxSave = resultStackIdx;

while(argsPtr->contents != (UINT)nilCell) /* for all instances of a arg */
    {
    arg = argsPtr;
    while(arg != nilCell)              /* for all args */
        {
        argCell = (CELL *)arg->contents; /* pop out first */
        if(argCell == nilCell) break;
        arg->contents = (UINT)argCell->next;
        argCell->next = nilCell; /* unlink */
        if(isSelfEval(argCell->type))
            cell = cell->next = argCell;
        else
            cell = cell->next = makeCell(CELL_QUOTE, (UINT)argCell);
        arg = arg->next;
        }

    cell = evaluateExpression(expr);
    cell = copyCell(cell);
    while(resultStackIdx > resultIdxSave)
            deleteList(popResult());

    if(res == NULL)
        results->contents = (UINT)cell;
    else
        res->next = cell;
    res = cell;
    if(cellIdx->type == CELL_LONG) cellIdx->contents += 1; 
    cell = (CELL *)expr->contents;
    deleteList(cell->next);
    }

cell->next = nilCell; /* decouple */
recoverIteratorIndex(cellIdx);

symbolCheck = NULL;
pushResultFlag = FALSE; /* results are already pushed */
return(results);
}


CELL * explodeList(CELL * list, CELL * params)
{
ssize_t len = 1;
ssize_t count = 1;
CELL * cell = NULL;
CELL * last = NULL;
CELL * result = NULL;
CELL * * lastChunk = NULL;
int flag = FALSE;

if(params != nilCell)
    {
    params = getInteger(params, (UINT*)&len);
    flag = getFlag(params);
    }

result = getCell(CELL_EXPRESSION);

if(len <= 0) return(result);

while(list != nilCell)
    {
    if(result->contents == (UINT)nilCell)
        {
        cell = getCell(CELL_EXPRESSION);
        lastChunk = (CELL * *)&result->contents;
        result->contents = (UINT)cell;
        cell->contents = (UINT)copyCell(list);
        last = (CELL*)cell->contents;
        }
    else
        {
        if(count < len)
            {
            last->next = copyCell(list);
            last = last->next;
            count++;
            }
        else
            {
            cell->next = getCell(CELL_EXPRESSION);
            lastChunk = (CELL * *)&cell->next;
            cell = cell->next;
            cell->contents = (UINT)copyCell(list);
            last = (CELL*)cell->contents;
            count = 1;
            }
        
        }
    list = list->next;
    }
    
if(flag && count < len)
    {
    if(lastChunk)
        {
        deleteList(*lastChunk);
        *lastChunk = nilCell;
        }
    }
    
return(result);
}


/* ---------------------- set primitives --------------------------------- */

CELL * setInterDiff(CELL * params, int mode);


#define SET_INTER 0
#define SET_DIFF 1
#define SET_UNIQUE 2

CELL * p_intersect(CELL * params)
{
if(params->next == nilCell)
    return(setInterDiff(params, SET_UNIQUE));
else
    return(setInterDiff(params, SET_INTER));
}

CELL * p_difference(CELL * params)
{
return(setInterDiff(params, SET_DIFF));
}


CELL * p_union(CELL * params)
{
CELL * result;
CELL * appnd;

appnd = makeCell(CELL_QUOTE, (UINT)p_append(params));
result = setInterDiff(appnd, SET_UNIQUE);
deleteList(appnd);

return(result);
}


CELL * p_unique(CELL * params)
{
return(setInterDiff(params, SET_UNIQUE));
}


CELL * setInterDiff(CELL * params, int mode)
{
CELL * listA;
CELL * listB = NULL;
CELL * * vectorA;
CELL * * vectorB = NULL;
CELL * * vectorResult;
ssize_t lengthA, lengthB = 0;
ssize_t i = 0, j = 0, k = 0, top = 0;
CELL * cell = NULL;
CELL * result;
int listMode = FALSE;
int cmp, flag = FALSE;

params = getListHead(params, &listA);
if(listA == nilCell)
    return(getCell(CELL_EXPRESSION));

if(mode != SET_UNIQUE)
    {
    params = getListHead(params, &listB);
    listMode = getFlag(params);

    if(listA == listB)
        {
        flag = TRUE;
        listA = copyList(listB);
        }

    if(listB == nilCell) 
        {
        if(mode == SET_INTER)
            return(getCell(CELL_EXPRESSION));
        listB = NULL;
        }
    }

vectorA = listToSortedVector(listA, &lengthA, NULL, TRUE);

vectorResult = callocMemory(lengthA * sizeof(CELL *));

if(listB)
    vectorB = listToSortedVector(listB, &lengthB, NULL, 0);

result = getCell(CELL_EXPRESSION);

while(i < lengthA)
    {
    if(listB) switch(mode)
        {
        case SET_INTER:
            cmp = compareCells(vectorA[i], vectorB[j]);
            if(cmp == 0) break;
            if(cmp < 0)
                {
                ++i;
                continue;
                }      
            if(j < (lengthB - 1)) ++j;
            else ++i;
            continue;

        case SET_DIFF:
            cmp = compareCells(vectorA[i], vectorB[j]);
            if(cmp == 0)
                {
                ++i;
                continue;
                }
            if(cmp < 0) break;
            if(j < (lengthB - 1)) ++j;
            else break;

            continue;
                            
        case SET_UNIQUE:
        default:
            break;
        }

    /* if not in result or if list mode is specified */
    if( (k == 0) || (compareCells(vectorA[i], vectorResult[top]) != 0) || (listMode == TRUE) )
        {
        top = k;
        vectorResult[k++] = vectorA[i];
        }

    ++i;
    }


if(k > 0)
    {
    binsort(vectorResult, k, (CELL*)0xFFFFFFFF);
    cell = copyCell(vectorResult[0]);
    result->contents = (UINT)cell;

    /* relinking */
    for(i = 1; i < k; i++)
        {
        cell->next = copyCell(vectorResult[i]);
        cell = cell->next;
        }
    cell->next = nilCell;
    }

free(vectorResult);

cell = resortVectorToList(vectorA, lengthA);

if(vectorB) free(vectorB);

if(flag) deleteList(listA);

return(result);
}    

/* ----------------------------------------------------------------------- */

CELL * p_match(CELL * params)
{
CELL * cell;
CELL * next;
CELL * result;

params = getEvalDefault(params, &cell);
if(!isList(cell->type)) return(nilCell);
params = getEvalDefault(params, &next);
if(!isList(next->type)) return(nilCell);

result = patternMatchL((CELL *)cell->contents, (CELL *)next->contents, getFlag(params));

return(result ? result : getCell(CELL_EXPRESSION));
}

CELL * linkMatches(CELL * * matchList, CELL * matchPtr, CELL * elmnt)
{
if(*matchList == NULL)
    {
    *matchList = makeCell(CELL_EXPRESSION, (UINT)elmnt);
    matchPtr = (CELL *)(*matchList)->contents;
    }
else
    {
    matchPtr->next = elmnt;
    }

while(matchPtr->next != nilCell)
    matchPtr = matchPtr->next;

return(matchPtr);
}


CELL * patternMatchL(CELL * pattern, CELL * list, int flag)
{
CELL * match;
CELL * matchList = NULL;
CELL * matches = NULL;
CELL * starList = NULL;
CELL * stars = NULL;

MATCH_LIST:
switch(pattern->type)
    {
    case CELL_NIL:
    /* end of pattern and list */
    if(list->type == CELL_NIL) 
        {
        if(starList) deleteList(starList);
        return(matchList);
        }
        
    goto NO_MATCH_RETURN;

    case CELL_QUOTE:
    case CELL_EXPRESSION:
    case CELL_FEXPR:
    case CELL_LAMBDA:
        /* compare subexpressions */
        if(list->type == pattern->type)
            {
            if((match = patternMatchL((CELL*)pattern->contents, (CELL*)list->contents, flag)) != nilCell)
                {
                if(match != NULL)
                    {
                    if(flag)
                        matches = linkMatches(&matchList, matches, match);
                    else
                        {
                        matches = linkMatches(&matchList, matches, (CELL*)match->contents);
                        match->contents = (UINT)nilCell;
                        deleteList(match);
                        }
                    }
                pattern = pattern->next;
                list = list->next;
                goto MATCH_LIST;
                }
            }

        goto NO_MATCH_RETURN;

    case CELL_SYMBOL:
        if(pattern->contents == (UINT)questionSymbol)   /* '?' */
            {
            if(list == nilCell) goto NO_MATCH_RETURN;
            if(!flag) matches = linkMatches(&matchList, matches, copyCell(list));
            break;
            }

        if(pattern->contents == (UINT)starSymbol ||
           pattern->contents == (UINT)plusSymbol)   /* '*'  and '+' */
            {
            if(starList == NULL) 
                {
                starList = getCell(CELL_EXPRESSION);
                }

            if(stars == NULL && pattern->contents == (UINT)plusSymbol)
                goto WILD_CARD_GREP;
            
            if(pattern->next == nilCell)
                {
                if(stars == NULL)
                    starList->contents = (UINT)copyList(list);
                else 
                    stars->next = copyList(list);

                linkMatches(&matchList, matches, starList);
                return(matchList);
                }

            if((match = patternMatchL(pattern->next, list, flag)) != nilCell)
                {
                matches = linkMatches(&matchList, matches, starList);
                if(match != NULL)
                    {
                    matches->next = (CELL*)match->contents;
                    match->contents = (UINT)nilCell;
                    deleteList(match);
                    }
                return(matchList);
                }

            if(list->next == nilCell)
            goto NO_MATCH_RETURN;

WILD_CARD_GREP:
            if(pattern->contents == (UINT)plusSymbol)
                if(list == nilCell) goto NO_MATCH_RETURN;

            if(stars == NULL)
                {
                starList->contents = (UINT)copyCell(list);
                stars = (CELL*)starList->contents;
                }
            else 
                {
                stars->next = copyCell(list);
                stars = stars->next;
                }

            list = list->next;
            goto MATCH_LIST;
            }
    default:
        if(compareCells(pattern, list) != 0)
        goto NO_MATCH_RETURN;

        break;  
    }

if(flag) matches = linkMatches(&matchList, matches, copyCell(list));


pattern = pattern->next;
list = list->next;
goto MATCH_LIST;

NO_MATCH_RETURN:
if(starList != NULL) deleteList(starList);
if(matchList != NULL) deleteList(matchList);
return(nilCell);
}


CELL * p_assoc(CELL * params)
{
CELL * key;
CELL * list;
int listMode;

key = evaluateExpression(params);
if((listMode = isList(key->type)))
    key = (CELL *)key->contents;

if(key == nilCell) key = copyCell(nilCell);

getEvalDefault(params->next, &list);
if(!isList(list->type))
    return(errorProcExt(ERR_LIST_EXPECTED, list));

list = (CELL *)list->contents;

while(key != nilCell)
    {
    while(list != nilCell)
        {
        if(isList(list->type))
            {
            if(compareCells(key, (CELL *)list->contents) == 0)
                break;
            }
        list = list->next;
        }

    if(!listMode || (key = key->next) == nilCell) break;
    list = ((CELL *)list->contents)->next;
    }

if(list == nilCell) return(nilCell);

pushResultFlag = FALSE;
return(list);
}


CELL * p_lookup(CELL * params)
{
CELL * key;
CELL * list;
int listMode;
ssize_t index;
CELL * deflt = nilCell;
SYMBOL * symbolRef;

key = evaluateExpression(params);
if((listMode = isList(key->type)))
    key = (CELL *)key->contents;

if(key == nilCell) key = copyCell(nilCell);

params = getEvalDefault(params->next, &list);
symbolRef = symbolCheck;

if(!isList(list->type))
    return(errorProcExt(ERR_LIST_EXPECTED, list));

list = (CELL *)list->contents;

if(params != nilCell)
    deflt = getInteger(params, (UINT *)&index);
else index = -1;

while(key != nilCell)
    {
    while(list != nilCell)
        {
        if(isList(list->type))
            if(compareCells(key, (CELL *)list->contents) == 0) break;
        list = list->next;
        }

    if(!listMode || (key = key->next) == nilCell) break;
    list = ((CELL *)list->contents)->next;
    }

if(list == nilCell)
    return(copyCell(evaluateExpression(deflt)));    

list = (CELL*)list->contents;

if(index < 0) index = convertNegativeOffset(index, list);

while(index--) 
        {
        if(list->next == nilCell) break;
        list = list->next;
        }

symbolCheck = symbolRef;
pushResultFlag = FALSE;
return(list);
}


/* bind an association list, works like:
   (define (bind L) (dolist (i L) (apply set i))) 
   L => ((x 1) (y 2) (z 3))
*/

CELL * p_bind(CELL * params)
{
CELL * list;

params = getListHead(params, &list);

return(copyCell(bindList(list, getFlag(params))));
}


CELL * bindList(CELL * params, int evalFlag)
{
SYMBOL * lref = NULL;
CELL * cell;
CELL * old;

while(params != nilCell)
    {
    if(params->type != CELL_EXPRESSION)
        return(errorProcExt(ERR_LIST_EXPECTED, params));

    cell = (CELL *)params->contents;
    lref = getSymbolCheckProtected(cell);
    old = (CELL *)lref->contents;
    if(evalFlag)
        lref->contents = (UINT)copyCell(evaluateExpression(cell->next));
    else
        lref->contents = (UINT)copyCell(cell->next);
    deleteList(old);
    params = params->next;
    }

if(lref == NULL)
    return(nilCell);

return((CELL *)lref->contents);
}


CELL * p_count(CELL * params)
{
CELL * items;
CELL * list;
CELL * result;
CELL * * vectorItems;
CELL * * vectorList;
ssize_t lengthItems, lengthList;
ssize_t i = 0, j = 0, idx; 
int cmp;
int flag = FALSE;
CELL * cell;
ssize_t * counts;

params = getListHead(params, &items);
getListHead(params, &list);

result = getCell(CELL_EXPRESSION);

if(items == nilCell)
    return(result);
    
if(items == list)
    {
    flag = TRUE;
    items = copyList(list);
    }
    
vectorItems = listToSortedVector(items, &lengthItems, NULL, TRUE);
vectorList = listToSortedVector(list, &lengthList, NULL, TRUE);

counts = (ssize_t *)callocMemory(lengthItems * sizeof(ssize_t));

if(vectorList)
while(i < lengthList)
    {
    cmp = compareCells(vectorList[i], vectorItems[j]);
    if(cmp == 0)
        {
        idx = (ssize_t)vectorItems[j]->next;
        counts[idx] += 1;
        ++i;
        continue;
        }
    if(cmp < 0)
        {
        ++i;
        continue;
        }
    if(j < (lengthItems - 1)) j++;
    else i++;
    }


cell = stuffInteger(counts[0]);
result->contents = (UINT)cell;
for(i = 1; i < lengthItems; i++)
    {
    cell->next = stuffInteger(counts[i]);
    cell = cell->next;
    }
freeMemory(counts);

cell = resortVectorToList(vectorItems, lengthItems);
if(vectorList) cell = resortVectorToList(vectorList, lengthList);

if(flag) deleteList(items);

return(result);
}


CELL * p_popAssoc(CELL * params)
{
CELL * key;
CELL * list;
CELL * original = NULL;
CELL * previous = NULL;
int listMode;

key = evaluateExpression(params);
if((listMode = isList(key->type)))
    key = (CELL *)key->contents;

if(key == nilCell) key = copyCell(nilCell);

getEvalDefault(params->next, &list);

if(!isList(list->type))
    return(errorProcExt(ERR_LIST_EXPECTED, list));
if(symbolCheck && isProtected(symbolCheck->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));

list->aux = (UINT)nilCell; /* undo last element optimization */
original = list;
list = (CELL *)list->contents;

while(key != nilCell)
    {
    while(list != nilCell)
        {
        if(isList(list->type))
            {
            list->aux = (UINT)nilCell; /* undo last element optimization */
            if(compareCells(key, (CELL *)list->contents) == 0) break;
            }
        previous = list;
        list = list->next;
        }

    if(!listMode || (key = key->next) == nilCell) break;
    previous = (CELL *)list->contents;
    list = ((CELL *)list->contents)->next;
    }

if(list == nilCell) return(nilCell); /* key not found */

if(previous == NULL)
    original->contents = (UINT)list->next;
else
    previous->next = list->next;

/* unlink */
list->next = nilCell;
return(list);   
}

void binsort(CELL * * x, ssize_t n, CELL * pCell)
{
ssize_t i,j,k,l,m,kf,lf;
CELL * expr;
CELL * cell;
UINT * resultIndexSave;
jmp_buf errorJumpSave;
int errNo;
CELL * * y;

y = allocMemory(n * sizeof(CELL *));

m = 1;
while(m < n)
    {
    for(i = 0; i < n; i += 2*m)
        {
        k = i; l = i + m;
        if(l >= n)
            {
            kf = lf = n;
            l = lf + 1;
            }
        else 
            {
            kf = k + m - 1;
            lf = l + m - 1;
            }

        if(lf >= n) lf = n - 1;

        for(j = i; j <= lf; j++)
            {
            if(k > kf)
                {
                y[j] = x[l++]; 
                continue;
                }
            if(l > lf)
                {
                y[j] = x[k++]; 
                continue;
                }
                
            if(pCell == NULL)
                {
                if(compareCells((CELL*)x[k], (CELL*)x[l]) <= 0)
                    y[j] = x[k++];
                else
                    y[j] = x[l++];
                continue;
                }
            if(pCell == (CELL*)0xFFFFFFFF)
                {
                if(((CELL*)x[k])->next <= ((CELL*)x[l])->next)
                    y[j] = x[k++];
                else
                    y[j] = x[l++];
                continue;
                }

            resultIndexSave = resultStackIdx;
            expr = makeCell(CELL_EXPRESSION, (UINT)copyCell(pCell));

            cell = (CELL *)expr->contents;
            cell->next = makeCell(CELL_QUOTE, (UINT)copyCell((CELL*)x[k]));
            cell = cell->next;

            cell->next = makeCell(CELL_QUOTE, (UINT)copyCell((CELL*)x[l]));
            
            /* do result stack cleanup, and free memory under
               error conditions */
            memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
            if((errNo = setjmp(errorJump)) != 0)
                {
                memcpy(errorJump, errorJumpSave, (sizeof(jmp_buf)));
                deleteList(expr);
                cleanupResults(resultIndexSave);
                free(x); /* allocates by parent routine */
                free(y);
                longjmp(errorJump, errNo);
                }                                       

            cell = evaluateExpression(expr);

            memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
            if(!isNil(cell) && !isEmpty(cell))
                y[j] = x[k++];
            else
                y[j] = x[l++];

            deleteList(expr);
            cleanupResults(resultIndexSave);
            }
        }

    for(i = 0; i < n; i++) x[i] = y[i]; 
    m = m * 2;
    }
    
free(y);
}

CELL * * listToSortedVector(CELL * list, ssize_t * length, CELL * func, int indexFlag);

CELL * p_sort(CELL * params)
{
CELL * list;
CELL * cell;
CELL * * vector;
ssize_t length, i;
SYMBOL * refSymbol;

list = params;

getEvalDefault(params, &cell);
refSymbol = symbolCheck;

if(symbolCheck && isProtected(symbolCheck->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));

if(isList(cell->type))
    {
    if(cell->contents == (UINT)nilCell)
        return(getCell(CELL_EXPRESSION));
    
    cell->aux = (UINT)nilCell; /* undo last element optimization */

    vector = listToSortedVector((CELL *)cell->contents, &length, list->next, 0);

    /* relink cells */
    list = vector[0];
    --length;
    i = 1;
    while(length--)
        {
        list->next = vector[i];
        list = list->next;
        i++;
        }
    list->next = nilCell;

    cell->contents = (UINT)vector[0];
    freeMemory(vector);
    }
else if(cell->type == CELL_ARRAY)
    {
    vector = (CELL **)cell->contents;
    length = (cell->aux - 1) / sizeof(UINT);
    if(list->next == nilCell)
        binsort(vector, length, NULL);
    else
        binsort(vector, length, list->next);
    }
else
    return(errorProcExt(ERR_LIST_OR_ARRAY_EXPECTED, list));

symbolCheck = refSymbol;
pushResultFlag = FALSE;
return(cell);
}


CELL * * listToSortedVector(CELL * list, ssize_t * length, CELL * func, int indexFlag)
{
CELL * * vector;
CELL * prev;
ssize_t i;

if((*length = listlen(list)) == 0) return(NULL);

/* build vector */
vector = allocMemory(*length * sizeof(CELL *));
for(i = 0; i < *length; i++)
    {
    vector[i] = prev = list;
    list = list->next;
    if(indexFlag) prev->next = (void *)i;
    }
if(func != nilCell && func != NULL)
    {
    func = evaluateExpression(func);
    if(func->type == CELL_SYMBOL) 
        func = (CELL*)((SYMBOL *)func->contents)->contents;
    func = copyCell(func);
    binsort(vector, *length, func);
    deleteList(func);
    } 
else
    binsort(vector, *length, NULL);

return(vector);
}


CELL * resortVectorToList(CELL * * vector, ssize_t length)
{
CELL * list;
ssize_t i;

binsort(vector, length, (CELL*)0xFFFFFFFF);
list  = vector[0];
for(i = 1; i < length; i++)
    {
    list->next = vector[i];
    list = list->next;
    }
list->next = nilCell;
list = vector[0];
free(vector);

return(list);
}

/* called with params containing the indices 
   or list of indices */

CELL * implicitIndexList (CELL * list, CELL * params)
{
CELL * cell;
ssize_t index;
int evalFlag;

cell = evaluateExpression(params);

if(isNumber(cell->type))
    {
    getIntegerExt(cell, (UINT *)&index, FALSE);
    params = params->next;
    evalFlag = TRUE;
    }
else if(isList(cell->type))
    {
    params = (CELL*)cell->contents;
    if(params == nilCell) return(list);
    params = getIntegerExt(params, (UINT *)&index, FALSE);
    evalFlag = FALSE;
    }
else return(errorProcExt(ERR_LIST_INDEX_INVALID, params));

while(isList(list->type))
    {
    /* 10.3.1 catch changes in the list to be indexed 
       caused by circular reference in index expression 
       e.g (lst (set 'lst foo)) , when foo is a vector 
    */
    if(list->type == 255)
        return(errorProc(ERR_LIST_REFERENCE_CHANGED));

    /* last element optimization */
    if(index == -1 && list->aux != (UINT)nilCell) 
        list = (CELL *)list->aux;
    else
        {
        list = (CELL *)list->contents;
        if(index < 0) 
            index = convertNegativeOffset(index, list);

        while(index--)  list = list->next;

        if(list == nilCell) 
            errorProc(ERR_LIST_INDEX_INVALID);
        }

    if(params == nilCell || !isList(list->type))  break;
    params = getIntegerExt(params, (UINT *)&index, evalFlag);
    }

return(list);
}


CELL * p_sequence(CELL * params)
{
double fromFlt, toFlt, interval, step, cntFlt;
INT64 fromInt64 = 0, toInt64 = 0, stepCnt, i;
CELL * sequence;
CELL * cell;
int intFlag;

if((intFlag = (((CELL*)params->next)->next == nilCell)))
    {
    params = getInteger64Ext(params, &fromInt64, TRUE);
    getInteger64Ext(params, &toInt64, TRUE);
    stepCnt = (fromInt64 > toInt64) ? fromInt64 - toInt64 : toInt64 - fromInt64;
    cell = stuffInteger64(fromInt64);
    }
else
    {   
    params = getFloat(params, &fromFlt);
    params = getFloat(params, &toFlt);
    getFloat(params, &step);

    if(isnan(fromFlt) || isnan(toFlt) || isnan(step))
        return(errorProc(ERR_INVALID_PARAMETER_NAN));

    step = (step < 0) ? -step : step;
    step = (fromFlt > toFlt) ? -step : step;
    cntFlt = (fromFlt < toFlt) ? (toFlt - fromFlt)/step : (fromFlt - toFlt)/step;
    stepCnt = (cntFlt > 0.0) ? floor(cntFlt + 0.0000000001) : floor(-cntFlt + 0.0000000001);
    cell = stuffFloat(fromFlt);
    }
sequence = makeCell(CELL_EXPRESSION, (UINT)cell);

for(i = 1; i <= stepCnt; i++)
    {
    if(intFlag)
        {
        if(fromInt64 > toInt64)
            cell->next = stuffInteger(fromInt64 - i);
        else
            cell->next = stuffInteger(fromInt64 + i);
        }
    else
        {
        interval = fromFlt + i * step;
        cell->next = stuffFloat(interval);
        }
    cell = cell->next;
    }

sequence->aux = (UINT)cell; /* last element optimization */
return(sequence);
}


#define FILTER_FILTER 0
#define FILTER_INDEX 1
#define FILTER_CLEAN 2
#define FILTER_FOR_ALL 3
#define FILTER_EXISTS 4

/* on EMSCRIPTEN, when compiling with -O1 or -O2, this is necessary
   optimization messes up setjmp/longjmp
*/

#ifdef EMSCRIPTEN
CELL * filterIndex(CELL * pCell, CELL * args, int mode);
#else
CELL * filterIndex(CELL * params, int mode);
#endif


CELL * p_filter(CELL * params)
{
#ifdef EMSCRIPTEN
CELL * pCell;
CELL * args;
pCell = evaluateExpression(params);
getEvalDefault(params->next, &args);
if(!isList(args->type))
    return(errorProcExt(ERR_LIST_EXPECTED, params->next));
return filterIndex(pCell, args, FILTER_FILTER);
#else
return filterIndex(params, FILTER_FILTER);
#endif
}

CELL * p_index(CELL * params)
{
#ifdef EMSCRIPTEN
CELL * pCell;
CELL * args;
pCell = evaluateExpression(params);
getEvalDefault(params->next, &args);
if(!isList(args->type))
    return(errorProcExt(ERR_LIST_EXPECTED, params->next));
return filterIndex(pCell, args, FILTER_INDEX);
#else
return filterIndex(params, FILTER_INDEX);
#endif
}

CELL * p_clean(CELL * params)
{
#ifdef EMSCRIPTEN
CELL * pCell;
CELL * args;
pCell = evaluateExpression(params);
getEvalDefault(params->next, &args);
if(!isList(args->type))
    return(errorProcExt(ERR_LIST_EXPECTED, params->next));
return filterIndex(pCell, args, FILTER_CLEAN);
#else
return filterIndex(params, FILTER_CLEAN);
#endif
}

CELL * p_exists(CELL * params)
{
#ifdef EMSCRIPTEN
CELL * pCell;
CELL * args;
pCell = evaluateExpression(params);
getEvalDefault(params->next, &args);
if(!isList(args->type))
    return(errorProcExt(ERR_LIST_EXPECTED, params->next));
return filterIndex(pCell, args, FILTER_EXISTS);
#else
return filterIndex(params, FILTER_EXISTS);
#endif
}

CELL * p_forAll(CELL * params)
{
#ifdef EMSCRIPTEN
CELL * pCell;
CELL * args;
pCell = evaluateExpression(params);
getEvalDefault(params->next, &args);
if(!isList(args->type))
    return(errorProcExt(ERR_LIST_EXPECTED, params->next));
return filterIndex(pCell, args, FILTER_FOR_ALL);
#else
return filterIndex(params, FILTER_FOR_ALL);
#endif
}

#ifdef EMSCRIPTEN
CELL * filterIndex(CELL * pCell, CELL * args, int mode)
#else
CELL * filterIndex(CELL * params, int mode)
#endif
{
CELL * expr;
#ifndef EMSCRIPTEN
CELL * pCell;
CELL * args;
#endif
CELL * resultList = NULL;
CELL * result;
CELL * cell;
UINT * resultIndexSave;
jmp_buf errorJumpSave;
ssize_t count;
int errNo, trueFlag;

#ifndef EMSCRIPTEN
pCell = evaluateExpression(params);
getEvalDefault(params->next, &args);

if(!isList(args->type))
    return(errorProcExt(ERR_LIST_EXPECTED, params->next));
#endif

args = (CELL *)args->contents;

result = NULL;
count = 0;
resultIndexSave = resultStackIdx;

memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
if((errNo = setjmp(errorJump)) != 0)
    {
    memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
    if(resultList) deleteList(resultList);
    longjmp(errorJump, errNo);
    }

while(args != nilCell)
    {
    expr = makeCell(CELL_EXPRESSION, (UINT)copyCell(pCell));
    cell = (CELL *)expr->contents;
    cell->next = makeCell(CELL_QUOTE, (UINT)copyCell(args));

    pushResult(expr);

    cell = evaluateExpression(expr);

    trueFlag = !isNil(cell) && !isEmpty(cell);

    cleanupResults(resultIndexSave);

    if(mode == FILTER_EXISTS && trueFlag)    
        {
        memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
        return(copyCell(args));
        }

    else if (mode == FILTER_FOR_ALL)
        {
        if(trueFlag) goto CONTINUE_FOR_ALL;
        memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
        return(nilCell);
        }

    if((trueFlag && mode != FILTER_CLEAN) || (!trueFlag && mode == FILTER_CLEAN))
        {
        if(result == NULL)
            {
            resultList = makeCell(CELL_EXPRESSION, (mode == FILTER_INDEX) ? 
                        (UINT)stuffInteger((UINT)count): (UINT)copyCell(args));
            result = (CELL*)resultList->contents;
            }
        else
            {
            result->next = (mode == FILTER_INDEX) ?
                        stuffInteger(count): copyCell(args);
            result = result->next;
            }
        }

    CONTINUE_FOR_ALL:
    args = args->next;
    count++;
    }

memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));

if(mode == FILTER_EXISTS)
    return(nilCell);

if(mode == FILTER_FOR_ALL)
    return(trueCell);

if(resultList == NULL)
    return(getCell(CELL_EXPRESSION));

return(resultList);
}


#define MAX_REF_STACK 256
typedef struct {
    size_t * base;
    size_t idx;
    } REFSTACK;

#define pushRef(A) (refStack->base[refStack->idx++] = (UINT)(A))
#define popRef() (--refStack->idx)

CELL * makeIndexVector(REFSTACK * refStack)
{
CELL * vector;
CELL * next;
int i;

next = stuffInteger(refStack->base[0]);

vector = makeCell(CELL_EXPRESSION, (UINT)next);

for(i = 1; i < refStack->idx; i++)
    {
    next->next = stuffInteger(refStack->base[i]);
    next = next->next;
    }   
    
return(vector);
}

#define REF_SINGLE 0
#define REF_ALL 1

#define REF_INDEX 0
#define REF_CONTENTS 1

void ref(CELL * keyCell, CELL * list, CELL * funcCell, CELL * * head, 
                        CELL * * next, REFSTACK * refStack, int mode)
{
size_t idx = 0;
UINT * resultIdxSave = resultStackIdx;
CELL * item;

while(list != nilCell)
    {
    if(compareFunc(keyCell, list, funcCell) == 0)
        {
        if(refStack->base)
            {
            if(refStack->idx < MAX_REF_STACK) pushRef(idx);
            else errorProc(ERR_NESTING_TOO_DEEP);
            item = makeIndexVector(refStack);
            popRef();
            }       
        else
            {
            if(mode == REF_SINGLE)
                item = list;
            else
                item = copyCell(list);
            }

        if(*next == NULL)
            *next = *head = item;
        else
            {
            (*next)->next = item;
            *next = (*next)->next;
            }   

        if(mode == REF_SINGLE) return;
        countCell->contents++;
        }

    if(isList(list->type))
        {
        if(refStack->base)
            {
            if(refStack->idx < MAX_REF_STACK) pushRef(idx);
            else errorProc(ERR_NESTING_TOO_DEEP);
            }
        ref(keyCell, (CELL*)list->contents, funcCell, head, next, refStack, mode);
        if(refStack->base) popRef();
        if(mode == REF_SINGLE && *head != NULL) 
            return;
        }

    idx++;
    cleanupResults(resultIdxSave);
    list = list->next;
    }
}


CELL * reference(CELL * params, int mode)
{
CELL * head = NULL;
CELL * keyCell;
CELL * list;
CELL * funcCell = NULL;
CELL * next = NULL;
REFSTACK refStack = {NULL, 0};
int flag = 0;
SYMBOL * symbolRef;

keyCell = evaluateExpression(params);
params = getEvalDefault(params->next, &list);
symbolRef = symbolCheck;

if(params != nilCell)
    {
    funcCell = evaluateExpression(params);
    flag = getFlag(params->next);
    }

if(!flag)
    refStack.base = alloca((MAX_REF_STACK + 2) * sizeof(size_t));

if(!isList(list->type))
    return(errorProcExt(ERR_LIST_EXPECTED, list));

ref(keyCell, (CELL *)list->contents, funcCell, &head, &next, &refStack, mode);

if(mode == REF_SINGLE) 
    {
    if(head == NULL) return(nilCell);
    if(flag) 
        {
        symbolCheck = symbolRef;
        pushResultFlag = FALSE;
        }
    return(head);
    }

list = getCell(CELL_EXPRESSION);
    
list->contents = (UINT)((head == NULL) ? nilCell : head);

return(list);
}

CELL * p_ref(CELL * params)
{
return(reference(params, REF_SINGLE));
}

CELL * p_refAll(CELL * params)
{
countCell->contents = 0;
return(reference(params, REF_ALL));
}


#define SETREF_SINGLE 1
#define SETREF_ALL 2

CELL * modRef(CELL * key, CELL * list, CELL * func, CELL * new, int mode, int * count)
{
CELL * result;
UINT * resultIdxSave = resultStackIdx;

while(list != nilCell)
    {
    if(compareFunc(key, list, func) == 0)
        {
        *count += 1;
        updateCell(list, new);
        if(mode == SETREF_SINGLE) return(list);
        countCell->contents++;
        }
    else if(isList(list->type))
        {
        result = modRef(key, (CELL *)list->contents, func, new, mode, count);
        if(result != nilCell) return(result);
        }

    cleanupResults(resultIdxSave);
    list = list->next;
    }

return(nilCell);
}


CELL * setRef(CELL * params, int mode)
{
CELL * key;
CELL * list;
CELL * new = NULL;
CELL * funcCell = NULL;
SYMBOL * refSymbol;
int count = 0;

key = evaluateExpression(params);
new = getEvalDefault(params->next, &list);
refSymbol = symbolCheck;
if(!isList(list->type))
    return(errorProcExt(ERR_LIST_EXPECTED, list));

if(symbolCheck && isProtected(symbolCheck->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));
    
if(new->next != nilCell)
    funcCell = evaluateExpression(new->next);

modRef(key, (CELL *)list->contents, funcCell, new, mode, &count);

if(count == 0)
    return(nilCell);

symbolCheck = refSymbol;
pushResultFlag = FALSE;
return(list);
}


CELL * p_setRef(CELL * params)
{
return(setRef(params, SETREF_SINGLE));
}

CELL * p_setRefAll(CELL * params)
{
countCell->contents = 0;
return(setRef(params, SETREF_ALL));
}


/* update a cell in-place and put a copy of previous content
   in $it to be used in replacement expressions. 
   Now only used in modeRef(), could be inlined.
*/
void updateCell(CELL * cell, CELL * val)
{
CELL * new;

itSymbol->contents = (UINT)cell;

if(val != nilCell)
    {
    new = copyCell(evaluateExpression(val)); 

    /* delete contents of original cell */
    if(isEnvelope(cell->type))
        {
        if(cell->type == CELL_ARRAY)
            deleteArray(cell);
        else
            deleteList((CELL *)cell->contents);
        }
    else if(cell->type == CELL_STRING || cell->type == CELL_DYN_SYMBOL 
#ifdef BIGINT
            || cell->type == CELL_BIGINT
#endif
            ) 
        freeMemory( (void *)cell->contents);

    cell->type = new->type;
    cell->aux = new->aux;
    cell->contents = new->contents;

    /* free the cell  */
    new->type = CELL_FREE;
    new->aux = 0;
    new->contents = 0;
    new->next = firstFreeCell;
    firstFreeCell = new;
    --cellCount;
    }

itSymbol->contents = (UINT)nilCell;
}

void flat(CELL * list, CELL * result, CELL * * next, UINT recursion)
{
while(list != nilCell)
    {
    if(isList(list->type) && recursion != 0)
        flat((CELL*)list->contents, result, next, recursion - 1);
    else
        {
        if(*next == NULL)
            {
            *next = copyCell(list);
            result->contents = (UINT)*next;
            }
        else
            {
            (*next)->next = copyCell(list);
            *next = (*next)->next;
            }   
        }

    list = list->next;
    }
}


CELL * p_flat(CELL * params)
{
CELL * list;
CELL * result;
CELL * next = NULL;
UINT recursion = -1;

params = getListHead(params, &list);
if(params != nilCell)
    getInteger(params, &recursion);

result = getCell(CELL_EXPRESSION);

flat(list, result, &next, recursion);

return(result);
}

/* 
    (collect <expr> [int-max-count]) 
    collect results of evaluating <expr> while not nil
    and optional max count is not reached
*/
CELL * p_collect(CELL * params)
{
CELL * result;
CELL * cell;
UINT * resultIdxSave = resultStackIdx;
UINT count = MAX_LONG;

result = getCell(CELL_EXPRESSION);
if(params->next != nilCell)
    getInteger(params->next, &count);

while(count > 0)
    {
    cell = evaluateExpression(params);
    if(isNil(cell)) break;
    addList(result, copyCell(cell));
    --count;
    cleanupResults(resultIdxSave);
    }

return(result);
}


/* --------------------------------- array routines ------------------------- */


CELL * initArray(CELL * array, CELL * list, CELL * * next);

CELL * p_array(CELL * params)
{
ssize_t index[17];
int p = 0;
CELL * array = NULL;
CELL * list = nilCell;
CELL * next = NULL;
SYMBOL * sPtr;

while(params != nilCell && p < 17)
    {
    list = evaluateExpression(params);
    if(isNumber(list->type))
        {
        getIntegerExt(list, (UINT*)&index[p], FALSE);
        if(index[p] < 1) 
            return(errorProcExt(ERR_WRONG_DIMENSIONS, list));
        else p++;
        }
    else if(list->type == CELL_CONTEXT)
        {
        sPtr = translateCreateSymbol( ((SYMBOL*)list->contents)->name, CELL_NIL,
            (SYMBOL*)list->contents, TRUE);
        list = (CELL *)sPtr->contents;
        }
    else if(isList(list->type)) break;
    else return(errorProcExt(ERR_NUMBER_EXPECTED, list));
    params = params->next;
    }

if(p == 0)
    return(errorProc(ERR_MISSING_ARGUMENT));

index[p] = 0;
if(!isList(list->type)) list = nilCell;

array = makeArray(index, 0);

if(list != nilCell) 
    array = initArray(array, list, &next);

return(array);  
}


CELL * makeArray(ssize_t * index, int p)
{
CELL * array;
CELL * list;
CELL * * addr;
ssize_t size;

array = getCell(CELL_ARRAY);
size = index[p];
array->contents = (UINT)callocMemory(size * sizeof(UINT) + 1);
array->aux = size * sizeof(UINT) + 1;
addr = (CELL * *)array->contents;

p++;
if(index[p] > 0)
    {
    list = makeArray(index, p);
    while(size--) *(addr++) = copyCell(list);
    deleteList(list); 
    return(array);
    }
else
    while(size--) *(addr++) = copyCell(nilCell);

return(array);
}


CELL * initArray(CELL * array, CELL * list, CELL * * next)
{
CELL * * addr;
int size;

size = (array->aux - 1) / sizeof(UINT);
addr = (CELL * *)array->contents;

while(size--)
    {
    if((*addr)->type == CELL_ARRAY)
        {
        *(addr) = initArray(*addr, list, next);
        addr++;
        continue;
        }

    if(*next == NULL || *next == nilCell)
        {
        deleteList(*addr);
        *(addr++) = copyCell((CELL *)list->contents);
        *next = (CELL*)list->contents;
        *next = (*next)->next;
        }
    else    
        {
        deleteList(*addr);
        *(addr++) = copyCell(*next);
        *next = (*next)->next;
        }
    }

return(array);
}

CELL * p_arrayList(CELL * params)
{
CELL * array;

getEvalDefault(params, &array);

if(array->type != CELL_ARRAY)
    return(errorProcExt(ERR_ARRAY_EXPECTED, params));

return(arrayList(array, TRUE));
}

CELL * arrayList(CELL * array, int flag)
{
CELL * list = NULL;
CELL * * addr;
CELL * new;
CELL * cell;
ssize_t size;

addr = (CELL * *)array->contents;
size = (array->aux - 1) / sizeof(UINT);

while(size--)
    {
    cell = *(addr++);
    if((cell->type == CELL_ARRAY) && flag)
        new = arrayList(cell, flag);
    else
        new = copyCell(cell);
    if(list == NULL)
        {
        array = list = makeCell(CELL_EXPRESSION, (UINT)new);
        list = new;
        }
    else
        {
        list->next = new;
        list = new;
        }
    }
        
return(array);
}

CELL * arrayTranspose(CELL * array)
{
ssize_t n, m, i, j;
CELL * cell;
CELL * * addr;
CELL * * newAddr;
CELL * * row;
CELL * * newRow;
CELL * newArray;

addr = (CELL * *)array->contents;
n = (array->aux - 1) / sizeof(CELL *);

cell = *addr;
if(cell->type != CELL_ARRAY)
    return(errorProcExt(ERR_WRONG_DIMENSIONS, array));
m = (cell->aux - 1) / sizeof(CELL *);

newArray = getCell(CELL_ARRAY);
newArray->aux = m  * sizeof(CELL *) + 1;
newAddr = (CELL * *)callocMemory(newArray->aux);
newArray->contents = (UINT)newAddr;

for(j = 0; j < m; j++)
    {
    /* create new row vector */
    cell = getCell(CELL_ARRAY);
    cell->aux = n * sizeof(CELL *) + 1;
    newRow = (CELL * *)callocMemory(cell->aux);
    cell->contents = (UINT)newRow;
    *(newAddr + j) = cell;
    for( i = 0; i < n; i++)
        {
        cell = *(addr + i);
        if(cell->type != CELL_ARRAY)
            *(newRow + i) = copyCell(cell); 
        else
            {
            row = (CELL * *)cell->contents;
            if( (cell->aux - 1) / sizeof(CELL *) < (j + 1))
                *(newRow + i) = nilCell;
            else
                *(newRow + i) = copyCell(*(row + j));
            }
        }
    }   
        
return(newArray);
}


CELL * subarray(CELL * array, ssize_t offset, ssize_t length)
{
CELL * newArray;
ssize_t size, i;
CELL * * newAddr;
CELL * * addr;

size = (array->aux - 1) / sizeof(CELL *);
if(offset < 0) offset = offset + size;
if(offset >= size || offset < 0) 
        return(errorProcExt2(ERR_ARRAY_INDEX_OUTOF_BOUNDS, stuffInteger(offset)));

if(length < 0)
    {
    length = size - offset + length;
    if(length < 0) length = 0;
    }

if(length == MAX_LONG && length > (size - offset))
    length = size - offset;

if(length == 0 || length > (size - offset))
    return(errorProcExt2(ERR_ARRAY_INDEX_OUTOF_BOUNDS, stuffInteger(length)));

addr = (CELL * *)array->contents;
newArray = getCell(CELL_ARRAY);
newArray->aux = length * sizeof(CELL *) + 1;
newAddr = (CELL * *)callocMemory(newArray->aux);
newArray->contents = (UINT)newAddr;

for(i = 0; i < length; i++) 
    *(newAddr + i) = copyCell(*(addr + offset + i));

return(newArray);
}


/* copies an array */
UINT * copyArray(CELL * array)
{
CELL * * newAddr;
CELL * * orgAddr;
CELL * * addr;
ssize_t size;

addr = newAddr = (CELL * *)callocMemory(array->aux);

size = (array->aux - 1) / sizeof(UINT);
orgAddr = (CELL * *)array->contents;

while(size--)
    *(newAddr++) = copyCell(*(orgAddr++));
    
return((UINT*)addr);
}


CELL * appendArray(CELL * array, CELL * params)
{
CELL * cell;
CELL * * addr;
ssize_t size, sizeCell;
ssize_t i;
CELL * * newAddr;
int  deleteFlag = 0; 

if(params == nilCell)
    return(copyCell(array));

START_APPEND_ARRAYS:
size = (array->aux - 1) / sizeof(CELL *);
addr = (CELL * *)array->contents;
cell = evaluateExpression(params);
if(cell->type != CELL_ARRAY)
    return(errorProcExt(ERR_ARRAY_EXPECTED, params));
sizeCell = (cell->aux - 1) / sizeof(CELL *);

newAddr = allocMemory(array->aux + cell->aux -1);

for(i = 0; i < size; i++)
    *(newAddr + i) = copyCell(*(addr + i));

addr = (CELL * *)cell->contents;

for(i = 0; i < sizeCell; i++)
    *(newAddr + size + i) = copyCell(*(addr + i));

cell = getCell(CELL_ARRAY);
cell->aux = (size + sizeCell) * sizeof(CELL *) + 1;
cell->contents = (UINT)newAddr;

if( (params = params->next) != nilCell)
    {
    if(deleteFlag)
        deleteList(array);
    deleteFlag = 1;
    array = cell;
    goto START_APPEND_ARRAYS;
    }

if(deleteFlag)
    deleteList(array);

symbolCheck = NULL;

return(cell);
}


void deleteArray(CELL * array)
{
CELL * * addr;
CELL * * mem;
ssize_t size;

mem = addr = (CELL * *)array->contents;
size = (array->aux - 1) / sizeof(UINT);
while(size--)
    deleteList(*(addr++));

freeMemory((char *)mem);
}

void printArray(CELL * array, UINT device)
{
CELL * list;

list = arrayList(array, TRUE);

printExpression(list, device);

deleteList(list);
}

void printArrayDimensions(CELL * array, UINT device)
{
CELL * * addr;

while(array->type == CELL_ARRAY)
    {
    varPrintf(device, "%d ", (array->aux - 1)/sizeof(CELL *));
    addr = (CELL **)array->contents;
    array = *addr;
    }
}


CELL * implicitIndexArray(CELL * cell, CELL * params)
{
CELL * * addr;
CELL * list;
ssize_t size, index;
int evalFlag;

list = evaluateExpression(params);

if(isNumber(list->type))
    {
    getIntegerExt(list, (UINT *)&index, FALSE);
    params = params->next;
    evalFlag = TRUE;
    }
else if(isList(list->type))
    {
    params = (CELL*)list->contents;
    if(params == nilCell) return(cell);
    params = getIntegerExt(params, (UINT *)&index, FALSE);
    evalFlag = FALSE;
    }
else return(errorProcExt(ERR_ARRAY_INDEX_OUTOF_BOUNDS, params));

while(cell->type == CELL_ARRAY)
    {
    addr = (CELL * *)cell->contents;
    size = (cell->aux - 1) / sizeof(UINT);
    if(index < 0) index = index + size;
    if(index >= size || index < 0) 
        return(errorProcExt2(ERR_ARRAY_INDEX_OUTOF_BOUNDS, stuffInteger(index)));
    cell = *(addr + index);
    if(params == nilCell || cell->type != CELL_ARRAY) break;
    params = getIntegerExt(params, (UINT *)&index, evalFlag);
    }

return(cell);
}


int compareArrays(CELL * left, CELL * right)
{
CELL * * leftAddr;
CELL * * rightAddr;
ssize_t leftS, rightS;
ssize_t result;

leftAddr = (CELL * *)left->contents;
rightAddr = (CELL * *)right->contents;
leftS = (left->aux - 1) / sizeof(UINT);
rightS = (right->aux - 1) / sizeof(UINT);

if(leftS < rightS) return(-1);
if(leftS > rightS) return(1);

result = 0;
while(leftS && result == 0)
    {
    result = compareCells(*(leftAddr++), *(rightAddr++));
    leftS--; 
    }

return(result);
}


int compareFunc(CELL * left, CELL * right, CELL * func)
{
CELL * cell;
CELL * expr;

if(func == NULL) 
    return(compareCells(left, right));

expr = makeCell(CELL_EXPRESSION, (UINT)copyCell(func));

pushResult(expr);

cell = (CELL *)expr->contents;
cell->next = makeCell(CELL_QUOTE, (UINT)copyCell((CELL*)left));
cell = cell->next;
cell->next = makeCell(CELL_QUOTE, (UINT)copyCell((CELL*)right));

cell = evaluateExpression(expr);

return(isNil(cell));
}

/* eof */

