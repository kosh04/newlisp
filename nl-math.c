/* nl-math.c


    Copyright (C) 2015 Lutz Mueller

    This program is free software: you cann redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#include "newlisp.h"
#include "protos.h"

/* turn on for extra debugging output */
/* #define BAYES_DEBUG */
/* #define DEBUG */

#define OP_ADD 1
#define OP_SUBTRACT 2
#define OP_MULTIPLY 3
#define OP_DIVIDE 4
#define OP_BIT_OR 5
#define OP_BIT_AND 6
#define OP_BIT_XOR 7
#define OP_SHIFTL 8
#define OP_SHIFTR 9
#define OP_POW 10
#define OP_MODULO 11
#define OP_SIN 12
#define OP_COS 13
#define OP_TAN 14
#define OP_ASIN 15
#define OP_ACOS 16
#define OP_ATAN 17
#define OP_SINH 18
#define OP_COSH 19
#define OP_TANH 20
#define OP_ASINH 21
#define OP_ACOSH 22
#define OP_ATANH 23
#define OP_SQRT 24
#define OP_LOG 25
#define OP_EXP 26
#define OP_MIN 27
#define OP_MAX 28
#define OP_CEIL 30
#define OP_FLOOR 31
#define OP_NAN 32
#define OP_ERRORFUNC 33
#define OP_SIGNUM 34
#define OP_ISNAN 35
#define OP_ISINF 36

#ifdef BIGINT
#define BIGINT_BASE 1000000000 /* 9 zeros */
#define BIGINT_BASE2 1000000000000000000LL
#endif

/* eliminated in 10.6.1
#ifdef WINDOWS
int _matherr(struct _exception *e) {return 1;}
#endif
*/

#ifdef DEBUG
void debug(int * x, int n, char * txt)
{
int i;

printf("%s ", txt);
for(i = 0; i <= n; i++)
    printf("%d_", x[i]);
printf("\n");
}
#endif

CELL * p_abs(CELL * params) 
{  
CELL * cell;
INT64 intValue;
double floatValue;
#ifdef BIGINT
int * numPtr;
#endif
    
cell = evaluateExpression(params);
#ifdef BIGINT
if(cell->type == CELL_BIGINT)
    {
    cell = copyCell(cell);
    numPtr = (int *)cell->contents;
    if(*numPtr == -1) *numPtr = 1;
    return(cell);
    }
else
#endif 
if(cell->type == CELL_FLOAT)
    {
    floatValue = getDirectFloat(cell);
    if(floatValue < 0.0) floatValue = floatValue * -1.0;
    return(stuffFloat(floatValue));
    }

getInteger64Ext(cell, &intValue, FALSE);
if(intValue < 0) intValue = intValue * -1;

return(stuffInteger64(intValue));
}

CELL * incDecI(CELL * params, int type)
{
CELL * cell;
INT64 adjust = 1;
INT64 lValue = 0;
#ifdef BIGINT
int * lvaluePtr;
int * adjustPtr;
int * resultPtr;
int * freePtr = NULL;
int lvlen, adjlen, reslen;
#endif

cell = evaluateExpression(params);

if(symbolCheck != NULL) 
    {
    if(isProtected(symbolCheck->flags))
        return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));
    }
else
    {
    if(cell == nilCell)
        errorProc(ERR_INVALID_PARAMETER);
    }


if(!isNil(cell)) 
#ifdef BIGINT
    {
    if(cell->type == CELL_BIGINT)
        {
        lvaluePtr = (int *)cell->contents;
        lvlen = cell->aux - 1;

        if(params->next != nilCell)
            freePtr = getBigintSizeDirect(evaluateExpression(params->next) , &adjustPtr, &adjlen);
        else
            freePtr = adjustPtr = intToBigint(1, &adjlen);

        reslen = (lvlen > adjlen) ? lvlen + 2 : adjlen + 2;
        resultPtr = calloc(reslen, sizeof(int));
        if(type < 0)
            subBigint(lvaluePtr, lvlen, adjustPtr, adjlen, resultPtr, &reslen);
        else
            addBigint(lvaluePtr, lvlen, adjustPtr, adjlen, resultPtr, &reslen);
        if(freePtr) freeMemory(freePtr);
        /* update old cell */
        freeMemory((void *)cell->contents);
        cell->contents = (UINT)resultPtr;
        cell->aux = reslen + 1;
        return(copyCell(cell));
        }
    getInteger64Ext(cell, &lValue, FALSE);
    }
#else
    getInteger64Ext(cell, &lValue, FALSE);
#endif
    

if(params->next != nilCell)
    getInteger64Ext(params->next, &adjust, TRUE);

#ifndef NEWLISP64
cell->type = CELL_INT64;
*(INT64 *)&cell->aux = lValue + adjust * type;
#else
cell->type = CELL_LONG;
cell->contents = lValue + adjust * type;
#endif

return(copyCell(cell));
}


CELL * incDecF(CELL * params, int type)
{
CELL * cell;
double adjust = 1.0;
double lValue = 0.0;

cell = evaluateExpression(params);

if(symbolCheck != NULL)
    {
    if(isProtected(symbolCheck->flags))
        return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck))); 
    }
else
    {
    if(cell == nilCell)
        errorProc(ERR_INVALID_PARAMETER);
    }

if(!isNil(cell)) getFloat(cell, &lValue);

if(params->next != nilCell)
    getFloat(params->next, &adjust);

cell->type = CELL_FLOAT;
#ifndef NEWLISP64
*(double *)&cell->aux = lValue + adjust * type;
#else
*(double *)&cell->contents = lValue + adjust * type;
#endif

return(copyCell(cell));
}


CELL * p_incrementI(CELL * params) { return(incDecI(params, 1)); }
CELL * p_decrementI(CELL * params) { return(incDecI(params, -1)); }
CELL * p_incrementF(CELL * params) { return(incDecF(params, 1)); }
CELL * p_decrementF(CELL * params) { return(incDecF(params, -1)); }

CELL * arithmetikOp(CELL * params, int op)
{
INT64 number;
INT64 result;
#ifdef BIGINT
int sizex = 0;
int sizey = 0;
int n;
int * num = NULL;
int * numx;
int * numy;
int * freePtr = NULL;
CELL * next;
#endif
CELL * cell;

if(params == nilCell)
    {
    if(op == OP_ADD)
        return(stuffInteger64(0));
    if(op == OP_MULTIPLY)
        return(stuffInteger64(1));
    }

cell = evaluateExpression(params);
params = params->next;
#ifdef BIGINT
if(cell->type == CELL_BIGINT)
    {
    if(params == nilCell)
        {
        cell = copyCell(cell);
        switch(op)
            {
            case OP_SUBTRACT:
                *(int *)cell->contents *= -1; break;
            case OP_ADD:
            case OP_MULTIPLY:
            case OP_DIVIDE:
            case OP_MODULO:
                break;
            default:
                return(errorProc(ERR_BIGINT_NOT_ALLOWED));
            }
        return(cell);
        }

    NEXT_FIRST_BIGINT:
    /* first OP is CELL_BIGINT */
    numx = (int*)cell->contents;
    sizex = cell->aux - 1;

    /* if 2nd OP is not CELL_BIGINT memory is allocated */
    next = evaluateExpression(params);
    if(next->type == CELL_BIGINT) /* speedup */
        {
        numy = (int*)next->contents;
        sizey = next->aux - 1;
        }
    else
        freePtr = getBigintSizeDirect(next, &numy, &sizey); 

    if(op == OP_MULTIPLY)
        n = sizex + sizey + 2; 
    else
        n = sizex > sizey ? sizex  + 2: sizey + 2;

    switch(op)
        {
        case OP_ADD:
            num = addBigint(numx, sizex, numy, sizey, malloc(n * sizeof(int)), &n); 
            break;
        case OP_SUBTRACT:
            num = subBigint(numx, sizex, numy, sizey, malloc(n * sizeof(int)), &n); 
            break;
        case OP_MULTIPLY:
            num = mulBigint(numx, sizex, numy, sizey, malloc(n * sizeof(int)), &n); 
            break;
        case OP_DIVIDE:         
            num = divModBigint(numx, sizex, numy, sizey, FALSE, &n); 
            break;
        case OP_MODULO:
            num = divModBigint(numx, sizex, numy, sizey, TRUE, &n);
            break;
        default:
            return(errorProc(ERR_BIGINT_NOT_ALLOWED));
        }    

    if(freePtr) 
        {
        freeMemory(freePtr);
        freePtr = NULL;
        } 
    cell = getCell(CELL_BIGINT);
    cell->contents = (UINT)num;
    cell->aux = n + 1; 
    if(params->next != nilCell)
        {
        pushResult(cell);
        params = params->next;
        goto NEXT_FIRST_BIGINT;
        }
    return(cell);
    }
#endif

getInteger64Ext(cell, &result, FALSE);
if(params == nilCell)
    {
    switch(op)
        {
        case OP_SUBTRACT:
            result = - result;
            break;
        case OP_SHIFTL:
            result <<= 1;
            break;
        case OP_SHIFTR:
            result >>= 1;
        default:
            break;
        }
    }

else while(params != nilCell)
    {
    params = getInteger64Ext(params, &number, TRUE);
    switch(op)
        {
        case OP_ADD:            result += number; break;
        case OP_SUBTRACT:       result -= number; break;
        case OP_MULTIPLY:       result *= number; break;
        case OP_DIVIDE:         
            if(number == 0) return(errorProc(ERR_MATH));
                                result /= number; break;
        case OP_BIT_OR:         result |= number; break;
        case OP_BIT_AND:        result &= number; break;
        case OP_BIT_XOR:        result ^= number; break;
        case OP_SHIFTL:         result <<= number; break;
        case OP_SHIFTR:         result >>= number; break;
        case OP_MODULO:
            if(number == 0) return(errorProc(ERR_MATH));
            result %= number; break;
        default:
            break;
        }
    }

return(stuffInteger64(result));
}


CELL * p_add(CELL * params) { return(arithmetikOp(params, OP_ADD)); }
CELL * p_subtract(CELL * params) { return(arithmetikOp(params, OP_SUBTRACT)); }
CELL * p_multiply(CELL * params) { return(arithmetikOp(params, OP_MULTIPLY)); }
CELL * p_divide(CELL * params) { return(arithmetikOp(params, OP_DIVIDE)); }

CELL * p_bitOr(CELL * params) { return(arithmetikOp(params, OP_BIT_OR)); }
CELL * p_bitAnd(CELL * params) { return(arithmetikOp(params, OP_BIT_AND)); }
CELL * p_bitXor(CELL * params) { return(arithmetikOp(params, OP_BIT_XOR)); }
CELL * p_shiftLeft(CELL * params) { return(arithmetikOp(params, OP_SHIFTL)); }
CELL * p_shiftRight(CELL * params) { return(arithmetikOp(params, OP_SHIFTR)); }
CELL * p_modulo(CELL * params) { return(arithmetikOp(params, OP_MODULO)); }


CELL * p_bitNot(CELL * params)
{
INT64 number;
getInteger64Ext(params, &number, TRUE);
return(stuffInteger64(~number));
}


/* ----------------------- float arithmetik ----------------------------- */

CELL * getFloat(CELL * params, double *);
CELL * floatOp(CELL * params, int op);
int compareFloats(CELL * left, CELL * right);
int compareInts(CELL * left, CELL * right);

CELL * p_addFloat(CELL * params) { return(floatOp(params, OP_ADD)); }
CELL * p_subFloat(CELL * params) { return(floatOp(params, OP_SUBTRACT)); }
CELL * p_mulFloat(CELL * params) { return(floatOp(params, OP_MULTIPLY)); }
CELL * p_divFloat(CELL * params) { return(floatOp(params, OP_DIVIDE)); }
CELL * p_minFloat(CELL * params) { return(floatOp(params, OP_MIN)); }
CELL * p_maxFloat(CELL * params) { return(floatOp(params, OP_MAX)); }
CELL * p_powFloat(CELL * params) { return(floatOp(params, OP_POW)); }
CELL * p_modFloat(CELL * params) { return(floatOp(params, OP_MODULO)); }

CELL * floatOp(CELL * params, int op)
{
double number;
double result;

if(params == nilCell)
    {
    if(op == OP_ADD)
        {
        result = 0.0;
        goto END_FLOAT_ARITHMETIK;
        }
    if(op == OP_MULTIPLY)
        {
        result = 1.0;
        goto END_FLOAT_ARITHMETIK;
        }
    }

params = getFloat(params, &result);

if(params == nilCell)
    {
    switch(op)
        {
        case OP_SUBTRACT:
            result = - result; break;
        case OP_DIVIDE:
            result = 1.0 / result; break;
        case OP_POW:
            result = result * result; break;
        case OP_MODULO:
            result = result - (int)result; break;
        default: break;
        }
    goto END_FLOAT_ARITHMETIK;
    }

else while(params != nilCell)
    {
    params = getFloat(params, &number);
    switch(op)
        {
        case OP_ADD:            result += number; break;
        case OP_SUBTRACT:       result -= number; break;
        case OP_MULTIPLY:       result *= number; break;
        case OP_DIVIDE:         result /= number; break;
        case OP_MIN: if(number < result) result = number; break;
        case OP_MAX: if(number > result) result = number; break;
        case OP_POW: result = pow(result, number); break;
        case OP_MODULO: 
            result = fmod(result, number);
        default: break;
        }
    }

END_FLOAT_ARITHMETIK:
params = getCell(CELL_FLOAT);
#ifndef NEWLISP64
memcpy((void *)&params->aux, (void *)&result, sizeof(double));
#else
*(double *)&params->contents = result;
#endif
return(params);
}


int compareFloats(CELL * left, CELL * right)
{
double leftFloat, rightFloat;

leftFloat = getDirectFloat(left);
rightFloat = getDirectFloat(right);

/* if(isnan(leftFloat) && isnan(rightFloat)) return(0); */
if(isnan(leftFloat) || isnan(rightFloat)) return(9);

if(leftFloat < rightFloat) return(-1);
if(leftFloat > rightFloat) return(1);

return(0);
}

/* should only be called when left and right type are different 
   and only called from compareCells()
*/
int compareInts(CELL * left, CELL * right)
{
INT64 leftnum;
INT64 rightnum;

#ifdef BIGINT
int * leftnumPtr;
int leftlen;
int * rightnumPtr;
int rightlen;
int cmp;

if(left->type == CELL_BIGINT)
    {
    leftnumPtr = (int *)left->contents;
    leftlen = left->aux - 1;
    /* would never get here when right->type == CELL_BIGINT */
    getBigintSizeDirect(right , &rightnumPtr, &rightlen);
    cmp = cmpBigint(leftnumPtr, leftlen, rightnumPtr, rightlen);
    freeMemory(rightnumPtr); 
    return(cmp);
    }
#endif

#ifndef NEWLISP64
if(left->type == CELL_LONG)
    leftnum = (int)left->contents;
else
    leftnum = *(INT64 *)&left->aux;

if(right->type == CELL_LONG)
    rightnum = (int)right->contents;
#ifdef CELL_BIGINT
else if(right->type == CELL_BIGINT)
    getInteger64Ext(right, &rightnum, FALSE);
#endif
else
    rightnum = *(INT64 *)&right->aux;
#else /* NEWLISP64 */
leftnum = (UINT)left->contents;
#ifdef CELL_BIGINT 
if(right->type == CELL_BIGINT)
   getInteger64Ext(right, &rightnum, FALSE);
else
#endif 
rightnum = (UINT)right->contents;
#endif /* NEWLISP64 */

if(leftnum < rightnum) return(-1);
if(leftnum > rightnum) return(1);

return(0);
}


double getDirectFloat(CELL * param)
{
double floatNum = 0.0;

#ifndef NEWLISP64
if(param->type == CELL_FLOAT)
    return(*(double *)&param->aux);
else if(param->type == CELL_LONG)
    floatNum = (INT)param->contents;
else if(param->type == CELL_INT64)
    floatNum = *(INT64 *)&param->aux;
#ifdef BIGINT
else if(param->type == CELL_BIGINT)
    floatNum = bigintCellToFloat(param);
#endif

#else /* NEWLISP64 */
if(param->type == CELL_FLOAT)
    return(*(double *)&param->contents);
else if(param->type == CELL_LONG)
    floatNum = (INT)param->contents;

#ifdef BIGINT
else if(param->type == CELL_BIGINT)
    floatNum = bigintCellToFloat(param);
#endif

#endif

return(floatNum);
}

/* ----------------------------- float functions ----------------------- */


CELL * functionFloat(CELL *, int);

CELL * p_sin(CELL * params) { return(functionFloat(params, OP_SIN)); }
CELL * p_cos(CELL * params) { return(functionFloat(params, OP_COS)); }
CELL * p_tan(CELL * params) { return(functionFloat(params, OP_TAN)); }
CELL * p_asin(CELL * params) { return(functionFloat(params, OP_ASIN)); }
CELL * p_acos(CELL * params) { return(functionFloat(params, OP_ACOS)); }
CELL * p_atan(CELL * params) { return(functionFloat(params, OP_ATAN)); }
CELL * p_sinh(CELL * params) { return(functionFloat(params, OP_SINH)); }
CELL * p_cosh(CELL * params) { return(functionFloat(params, OP_COSH)); }
CELL * p_tanh(CELL * params) { return(functionFloat(params, OP_TANH)); }
CELL * p_asinh(CELL * params) { return(functionFloat(params, OP_ASINH)); }
CELL * p_acosh(CELL * params) { return(functionFloat(params, OP_ACOSH)); }
CELL * p_atanh(CELL * params) { return(functionFloat(params, OP_ATANH)); }
CELL * p_sqrt(CELL * params) { return(functionFloat(params, OP_SQRT)); }
CELL * p_log(CELL * params) { return(functionFloat(params, OP_LOG)); }
CELL * p_exp(CELL * params) { return(functionFloat(params, OP_EXP)); }
CELL * p_ceil(CELL * params) { return(functionFloat(params, OP_CEIL)); }
CELL * p_floor(CELL * params) { return(functionFloat(params, OP_FLOOR)); }
CELL * p_erf(CELL * params) { return(functionFloat(params, OP_ERRORFUNC)); }
CELL * p_sgn(CELL * params) { return(functionFloat(params, OP_SIGNUM)); }
CELL * p_isnan(CELL * params) { return(functionFloat(params, OP_ISNAN)); }
CELL * p_isinf(CELL * params) { return(functionFloat(params, OP_ISINF)); }

CELL * functionFloat(CELL * params, int op)
{
double floatN;
double base;
CELL * cell;

getFloat(params, &floatN);

switch(op)
  {
  case OP_SIN: floatN = sin(floatN); break;
  case OP_COS: floatN = cos(floatN); break;
  case OP_TAN: floatN = tan(floatN); break;

  case OP_ASIN: floatN = asin(floatN); break;
  case OP_ACOS: floatN = acos(floatN); break;
  case OP_ATAN: floatN = atan(floatN); break;

  case OP_SINH: floatN = sinh(floatN); break;
  case OP_COSH: floatN = cosh(floatN); break;
  case OP_TANH: floatN = tanh(floatN); break;

  case OP_ASINH: floatN = asinh(floatN); break;
  case OP_ACOSH: floatN = acosh(floatN); break;
  case OP_ATANH: floatN = atanh(floatN); break;

  case OP_SQRT: floatN = sqrt(floatN); break;
  case OP_LOG:
    if(params->next != nilCell)
      {
      getFloat(params->next, &base);
      floatN = log(floatN) / log(base);
      } 
    else 
      floatN = log(floatN); 
    break;
  case OP_EXP: floatN = exp(floatN); break;
  case OP_CEIL: floatN = ceil(floatN); break;
  case OP_FLOOR: floatN = floor(floatN); break;
  case OP_ERRORFUNC: floatN = erf(floatN); break;
  case OP_SIGNUM:
    if(params->next == nilCell)
        { 
        if(floatN > 0.0) floatN = 1.0;
        else if(floatN < 0.0) floatN = -1.0; 
        else floatN = 0.0;
        break;
        }
    else
        {
        if(floatN < 0.0) cell = params->next;
        else
            {
            params = params->next;
            if(floatN == 0.0) cell = params->next;
            else {
                 params = params->next;
                 cell = params->next;
                 }
            }
        }
            
        return(copyCell(evaluateExpression(cell)));
    case OP_ISNAN:
        return (isnan(floatN) ? trueCell : nilCell);
    case OP_ISINF:
#ifdef SOLARIS
        return((isnan(floatN - floatN)) ? trueCell : nilCell);
#else
        return(isinf(floatN) ? trueCell : nilCell);
#endif
    default: break;
    }

cell = getCell(CELL_FLOAT);
#ifndef NEWLISP64
*(double *)&cell->aux = floatN;
#else
*(double *)&cell->contents = floatN;
#endif
return(cell);
}


CELL * p_atan2(CELL * params)
{
double floatX, floatY;
CELL * cell;

params = getFloat(params, &floatX);
getFloat(params, &floatY);

cell = getCell(CELL_FLOAT);
#ifndef NEWLISP64
*(double *)&cell->aux = atan2(floatX, floatY);
#else
*(double *)&cell->contents = atan2(floatX, floatY);
#endif
return(cell);
}

CELL * p_round(CELL * params)
{
double fNum;
double precision = 1.0;
INT digits = 0;
char * fmt;
char * result;


params = getFloat(params, &fNum);
if(params != nilCell)
    getInteger(params, (UINT*)&digits);

if(digits >= 0)
    {
    precision = pow(10.0, (double)(digits > 20 ? 20 : digits));
    fNum = precision * floor(fNum/precision + 0.5);
    }
else
    {
    fmt = alloca(16);
    result = alloca(32);
    snprintf(fmt, 15, "%%.%df", (int)((digits < -16) ? 16 : -digits));
    snprintf(result, 31, fmt, fNum);
    fNum = atof(result);
    }

return(stuffFloat(fNum)); 
}


CELL * p_rand(CELL * params)
{
INT range;  
INT n;
CELL * dist, * cell;
INT rnum;
double scale;

params = getInteger(params, (UINT *)&range);
scale = range;
if(range == 0) 
    {
    srandom((unsigned)time(NULL));
    return(trueCell);
    }

while((rnum = random()) == MY_RAND_MAX);
rnum = (scale * rnum)/MY_RAND_MAX;

if(params->type == CELL_NIL)
    return(stuffInteger((UINT)rnum));
    
getInteger(params, (UINT *)&n);


cell = stuffInteger((UINT)rnum);
dist = makeCell(CELL_EXPRESSION, (UINT)cell);

--n;
while(n-- > 0)
    {
    while((rnum = random()) == MY_RAND_MAX);
    rnum = (scale * rnum)/MY_RAND_MAX;
    cell->next = stuffInteger((UINT)rnum);
    cell = cell->next;
    }

return(dist);
}


CELL * p_amb(CELL * params)
{
INT len;
CELL * cell;

if((len = listlen(params)) == 0) return(nilCell);

len = random() % len;

while(len--) params = params->next;

cell = evaluateExpression(params);
if(symbolCheck)
    {
    pushResultFlag = FALSE;
    return(cell);
    }

return(copyCell(cell));
}


CELL *  p_seed(CELL * params)
{
INT64 seedNum;

getInteger64Ext(params, &seedNum, TRUE);

srandom((UINT)(seedNum & 0xFFFFFFFF));

return(stuffInteger(seedNum));
}


/* -----------------------  compare ops --------------------------- */

#define OP_LESS 1
#define OP_GREATER 2
#define OP_LESS_EQUAL 3
#define OP_GREATER_EQUAL 4
#define OP_EQUAL 5
#define OP_NOTEQUAL 6

CELL * p_less(CELL * params) { return(compareOp(params, OP_LESS)); }
CELL * p_greater(CELL * params) { return(compareOp(params, OP_GREATER)); }
CELL * p_lessEqual(CELL * params) { return(compareOp(params, OP_LESS_EQUAL)); }
CELL * p_greaterEqual(CELL * params) { return(compareOp(params, OP_GREATER_EQUAL)); }
CELL * p_equal(CELL * params) { return(compareOp(params, OP_EQUAL)); }
CELL * p_notEqual(CELL * params) { return(compareOp(params, OP_NOTEQUAL)); }

CELL * compareOp(CELL * params, int op)
{
CELL * left;
CELL * right;
INT cnt = 0;
int comp;

left = evaluateExpression(params);

while(TRUE)
    {
    if((params = params->next) == nilCell && cnt == 0)
        {
        if(isNumber(left->type))
            right = stuffInteger64(0);
        else if(left->type == CELL_STRING)
            right = stuffString("");
        else if(isList(left->type))
            right = getCell(CELL_EXPRESSION);
        else break;     
        pushResult(right);
        }
    else
        right = evaluateExpression(params);
    ++cnt;
    if((comp = compareCells(left, right)) == 9) 
        return( (op == OP_NOTEQUAL) ? trueCell : nilCell);

    switch(op)
        {
        case OP_LESS:
            if(comp >= 0) return(nilCell);
            break;
        case OP_GREATER:
            if(comp <= 0) return(nilCell);
            break;
        case OP_LESS_EQUAL:
            if(comp > 0) return(nilCell);
            break;
        case OP_GREATER_EQUAL:
            if(comp < 0) return(nilCell);
            break;
        case OP_EQUAL:
            if(comp != 0) return(nilCell);
            break;
        case OP_NOTEQUAL:
            if(comp == 0) return(nilCell);
        default:
            break;
        }

    if(params->next == nilCell) break;
    left = right;
    }

return(trueCell);
}

int compareSymbols(CELL * left, CELL * right)
{
SYMBOL * leftS;
SYMBOL * rightS;
char * lcName;
char * rcName;
char * lsName;
char * rsName;
int comp;

if(left->contents == (UINT)nilSymbol)
    {
    if(right->contents == left->contents)return(0);
    else return(-1);
    }

if(left->contents == (UINT)trueSymbol)
    {
    if(left->contents == right->contents) return(0);
    if(right->contents == (UINT)nilSymbol) return(1);
    return(-1);
    }

if(right->contents == (UINT)nilSymbol || right->contents == (UINT)trueSymbol)
    return(1);

if(left->contents == right->contents) return(0);

/* else compare context- and symbol- names */

if(left->type == CELL_SYMBOL)
    {
    leftS = (SYMBOL *)left->contents;
    lcName = ((SYMBOL *)leftS->context)->name;
    lsName = leftS->name;
    }
else
    {
    lcName = ((SYMBOL *)left->aux)->name;
    lsName = (char *)left->contents;
    }

if(right->type == CELL_SYMBOL)
    {
    rightS = (SYMBOL *)right->contents;
    rcName = ((SYMBOL *)rightS->context)->name;
    rsName = rightS->name;
    }
else
    {
    rcName = ((SYMBOL *)right->aux)->name;
    rsName = (char *)right->contents;
    }

if((comp = strcmp(lcName, rcName)) == 0)
    {
    if((comp = strcmp(lsName, rsName)) == 0) return(0); 
    }

return (comp > 0 ? 1 : -1);
}


/* returns equal: 0 less: -1 greater: 1 or 9 if NaN or Inf equal comparison */
int compareCells(CELL * left, CELL * right)
{
int comp;
if(left->type != right->type)
    {
    if(left->type == CELL_FLOAT && ((right->type & COMPARE_TYPE_MASK) == CELL_INT))
        return(compareFloats(left, right));
    if(((left->type & COMPARE_TYPE_MASK) == CELL_INT) && right->type == CELL_FLOAT)
        return(compareFloats(left, right));

    if((left->type & COMPARE_TYPE_MASK) == CELL_INT && (right->type & COMPARE_TYPE_MASK) == CELL_INT)
        return(compareInts(left, right));

    if(isNil(left))
        {
        if(isNil(right)) return(0);
        else return(-1);
        }

    if(isTrue(left))
        {
        if(isTrue(right)) return(0);
        if(isNil(right)) return(1);
        return(-1);
        }

    if(isNil(right) || isTrue(right))
        return(1);

    if(isSymbol(left->type) && isSymbol(right->type))
        return(compareSymbols(left, right));
        
    if( (left->type == CELL_SYMBOL && right->type == CELL_CONTEXT) ||
        (left->type == CELL_CONTEXT && right->type == CELL_SYMBOL) )
        {
        if((comp = strcmp( ((SYMBOL *)left->contents)->name, 
                           ((SYMBOL *)right->contents)->name)) == 0) return(0);
        return (comp > 0 ? 1 : -1);
        }

    comp = (left->type & COMPARE_TYPE_MASK) - (right->type & COMPARE_TYPE_MASK);
    if(comp == 0) return(0);

    return( comp > 0 ? 1 : -1);
    }

/* left type and right type are the same */
switch(left->type)
    {
    case CELL_STRING:
        comp = left->aux - right->aux;
        if(comp == 0)
            {
            if((comp = memcmp((char *)left->contents, 
                    (char *)right->contents,
                    left->aux)) == 0) return(0);
            }
        else if(comp > 0)
            {
            if((comp = memcmp((char *)left->contents,
                      (char *)right->contents,
                      right->aux - 1)) == 0) 
                return(1);
            }
        else if(comp < 0)
            {
            if((comp = memcmp((char *)left->contents,
                      (char *)right->contents,
                      left->aux - 1)) == 0) 
                return(-1);
            }
            
        return (comp > 0 ? 1 : -1);

    case CELL_SYMBOL:
    case CELL_DYN_SYMBOL:
        return(compareSymbols(left, right));

    case CELL_QUOTE:
    case CELL_EXPRESSION:
    case CELL_LAMBDA:
    case CELL_FEXPR:
        return(compareLists((CELL*)left->contents, (CELL*)right->contents));
    case CELL_ARRAY:
        return(compareArrays((CELL*)left, (CELL*)right));
    case CELL_FLOAT:
        return(compareFloats(left, right));
#ifndef NEWLISP64
    case CELL_INT64:
        if(*(INT64 *)&left->aux > *(INT64 *)&right->aux) return(1);
        if(*(INT64 *)&left->aux < *(INT64 *)&right->aux) return(-1);
        break;
#endif
#ifdef BIGINT
    case CELL_BIGINT:
        comp = cmpBigint((int *)(UINT)left->contents, 
                        left->aux - 1, (int *)(UINT)right->contents, right->aux - 1);
        return(comp);
#endif 
    case CELL_LONG:
    default:
        if((INT)left->contents > (INT)right->contents) return(1);
        if((INT)left->contents < (INT)right->contents) return(-1);
        break;  
    }

return(0);
}

int compareLists(CELL * left, CELL * right)
{
int result;

while(left != nilCell)
    {
    if( (result = compareCells(left, right)) != 0)
        return(result);
    left = left->next;
    right = right->next;
    }
if(right == nilCell) return(0);
return(-1);
}

/* ---------------------------- encryption -----------------------------

  XOR one-pad enryption

*/


void encryptPad(char *encrypted, char *data, char * key, size_t dataLen, size_t keyLen)
{
size_t i;
for(i = 0; i < dataLen; i++)
    *(encrypted + i) = *(data + i) ^ *(key + i % keyLen);
}


CELL * p_encrypt(CELL * params)
{
char * data;
char * key;
char * dataEncrypted;
CELL * encrypted;
size_t dataLen, keyLen;

getStringSize(params, &data, &dataLen, TRUE);
getStringSize(params->next, &key, &keyLen, TRUE);

if(!keyLen) return(errorProcExt(ERR_STRING_EXPECTED, params->next));

dataEncrypted = (char *)allocMemory(dataLen + 1);
*(dataEncrypted + dataLen) = 0;

encryptPad(dataEncrypted, data, key, dataLen, keyLen);

encrypted = getCell(CELL_STRING);
encrypted->contents = (UINT)dataEncrypted;
encrypted->aux = dataLen + 1;

return(encrypted);
}



/* =============================  Fast Fourier Transform ======================== */

CELL * fft(CELL * params, int isign);
CELL * makeListFromFloats(double num1, double num2);
void fastFourierTransform(double data[], unsigned int  nn, int isign);

CELL * p_fft(CELL * params)
{
return fft(params, 1);
}


CELL * p_ifft(CELL * params)
{
return fft(params, -1);
}

CELL * fft(CELL * params, int isign)
{
CELL * listData;
CELL * list;
CELL * cell;
double * data;
unsigned int i, n, N;

getListHead(params, &listData);

n = listlen(listData);

N = 1;
while(N < n) N <<= 1;

data = (double *)allocMemory(N * 2 * sizeof(double));
list = listData;
for(i = 0; i < n*2; i+=2)
    {
    if(isNumber(list->type))
        {
        data[i] = getDirectFloat(list);
        data[i+1] = (double)0.0;
        list = list->next;
        continue;
        }

    if(list->type != CELL_EXPRESSION)
        {
        freeMemory(data);
        return(errorProcExt(ERR_LIST_OR_NUMBER_EXPECTED, list));
        }

    cell = (CELL *)list->contents;
    data[i] = getDirectFloat(cell);
    data[i+1] = getDirectFloat(cell->next);
    list= list->next;
    }
    

for(i = n * 2; i < N * 2; i++)
    data[i] = 0.0;

fastFourierTransform(data, N, isign);

list = listData = getCell(CELL_EXPRESSION);
if(isign == -1) 
    for(i = 0; i < n * 2; i++) data[i] = data[i]/N;

list->contents = (UINT)makeListFromFloats(data[0], data[1]);
list = (CELL*)list->contents;
for(i = 2; i < N * 2; i += 2)
    {
    list->next = makeListFromFloats(data[i], data[i+1]);
    list = list->next;
    }

freeMemory(data);

return(listData);
}


CELL * makeListFromFloats(double num1, double num2)
    {
    CELL * cell;
    CELL * list;

    list = getCell(CELL_EXPRESSION);
    list->contents = (UINT)stuffFloat(num1);
    cell = (CELL *)list->contents;
    cell->next = stuffFloat(num2);
    return(list);
    }


/* Fast Fourier Transform 
// algorithm modified for zero-offset data[] and double precision from
// 
// Numerical Recipes in 'C', 2nd Edition
// W.H. Press, S.A. Teukolsky
// W.T. Vettering, B.P. Flannery
// Cambridge University Press, 1992
*/

#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

void fastFourierTransform(double data[], unsigned int nn, int isign)
{
unsigned int n, mmax, m, j, istep, i;
double wtemp, wr, wpr, wpi, wi, theta;
double tempr, tempi;

n = nn << 1;
j = 1;

for(i = 1; i < n; i += 2)
    {
    if(j > i)
        {
        SWAP(data[j-1], data[i-1]);
        SWAP(data[j], data[i]);
        }
    m = n >> 1;
    while(m >= 2 && j > m)
        {
        j -= m;
        m >>= 1;
        }
    j += m;
    }


mmax = 2;
while(n > mmax)
    {
    istep = mmax << 1;
    theta = isign * (6.28318530717959/mmax);
    wtemp = sin(0.5 * theta);
    wpr = -2.0 * wtemp * wtemp;
    wpi = sin(theta);
    wr = 1.0;
    wi = 0.0;
    for(m = 1; m < mmax; m += 2)
        {
        for(i = m; i <= n; i += istep)
            {   
            j = i + mmax;
            tempr = wr * data[j-1] - wi * data[j];
            tempi = wr * data[j] + wi * data[j-1];
            data[j-1] = data[i-1] - tempr;
            data[j] = data[i] - tempi;
            data[i-1] += tempr;
            data[i] += tempi;
            }
        wtemp = wr;
        wr = wtemp * wpr - wi * wpi + wr;
        wi = wi * wpr + wtemp * wpi + wi;
        }
    mmax = istep;
    }
}


/* --------------------------- probability distributions ----------------- */

#define DIST_RANDOM 0
#define DIST_NORMAL 1

double getRandom(double offset, double scale, int type)
{
int i;
double randnum;

if(type == DIST_RANDOM)
    {
    randnum = random();
    return(scale * randnum/MY_RAND_MAX + offset);
    }

if(type == DIST_NORMAL)
    {
    randnum = 0.0;
    for(i = 0; i < 12; i++)
        randnum += random() % 1024;
    return(scale * (randnum - 6144 + 6)/1024 + offset);
    }

return(0.0);
}

CELL * randomDist(CELL * params, int type)
{
double randnum;
size_t i;
size_t n = 0;
double scale = 1.0;
double offset = 0.0;
CELL * dist;
CELL * cell;

if(params->type != CELL_NIL)
    {
    params = getFloat(params, &offset);
    params = getFloat(params, &scale);
    if(params->type != CELL_NIL)
        getInteger(params, (UINT*)&n);
    }

if( n == 0)
    {
    randnum = getRandom(offset, scale, type);
    return(stuffFloat(randnum));
    }

dist = getCell(CELL_EXPRESSION);
randnum = getRandom(offset, scale, type);
dist->contents = (UINT)stuffFloat(randnum);
cell = (CELL*)dist->contents;
for(i = 1; i < n; i++)
    {
    randnum = getRandom(offset, scale, type);
    cell->next = stuffFloat(randnum);
    cell = cell->next;
    }

return(dist);
}


CELL * p_normal(CELL * params)
{
return(randomDist(params, DIST_NORMAL));
}

CELL * p_random(CELL * params)
{
return(randomDist(params, DIST_RANDOM));
}

CELL * p_randomize(CELL * params)
{
CELL * list;
CELL * cell;
CELL * * vector;
size_t length, i, j;
INT rnum;
double scale;

getListHead(params, &list);

if((length = listlen(list)) <= 1) 
  return(makeCell(CELL_EXPRESSION, (UINT)copyList(list)));
 
/* build index vector */
cell = list; 
vector = allocMemory(length * sizeof(UINT));
for(i = 0; i < length; i++)
    {
    vector[i] = copyCell(cell);
    vector[i]->next = (void *)i;
    cell = cell->next;
    }

/* reorganize randomly */
RANDOMIZE:
for(i = 0; i < (length - 1); i++)
    {
    scale = length - i;
    while((rnum = random()) == MY_RAND_MAX);
    rnum = (scale * rnum)/MY_RAND_MAX;
    j = i + rnum;
    cell = vector[i];
    vector[i] = vector[j];
    vector[j] = cell;
    }

/* check that new sequence is different 
   for default no flag or nil flag */
if(!getFlag(params->next))
    {
    for(i = 0; i < length; i++)
        if(vector[i]->next != (void *)i) break;
  
    if(i == length)  goto RANDOMIZE;
    }

/* relink the list */
cell = list = getCell(CELL_EXPRESSION);
cell->contents  = (UINT)vector[0];
cell = (CELL *)cell->contents;
for(i = 1; i < length; i++)
    {
    cell->next = vector[i];
    cell = cell->next;
    }
cell->next = nilCell;
freeMemory(vector);

return(list);
}

CELL * probability_x(CELL * params, int type);
double probChi2(double chi2, UINT df);
double probT(double t, UINT df);
double probF(double t, UINT df1, UINT df2);
double critical_value(double p, UINT df1, UINT df2, double max_val, int type);
double gammaln(double xx);
double betai(double a, double b, double x);
double gammap(double a, double x);
double betacf(double a, double b, double x);
static double gser(double a, double x, double gln);
double gcf(double a, double x, double gln);

#define SQRT2 1.414213562373095

CELL * p_probabilityZ(CELL * params)
{
double z;
double p;

getFloat(params, &z);

p = 0.5 + erf(z/SQRT2) / 2.0;

return(stuffFloat(p));
}


CELL * p_criticalZ(CELL * params)
{
double p;
double sign = 1.0;
#define Z_EPSILON 0.000001   /* Accuracy of Z approximation */
double minZ = 0.0;
double maxZ = 6.0;
double Zval;
double Z;
        
getFloat(params, &p);

if (p < 0.5)
    {
    p = 1.0 - p;
    sign = -1.0;
    }
        
Zval = 2.0;    /* fair first value */
while ((maxZ - minZ) > Z_EPSILON)
    {
    if ( (0.5 + erf(Zval/SQRT2) / 2.0) < p) minZ = Zval;
    else maxZ = Zval;
    Zval = (maxZ + minZ) * 0.5;
    }

Z = (Zval > 5.999999) ? sign * 6.0 : Zval * sign;

return(stuffFloat(Z));
}


double probChi2(double chi2, UINT df)
{
return(gammap(df/2.0, chi2/2.0));
}


double probT(double t, UINT df)
{
double bta;

bta = betai(df/2.0, 0.5, 1.0/(1.0 + t*t/df));
if(t > 0.0) return(1.0 - 0.5 * bta);
else if(t < 0.0) return(0.5 * bta);
return(0.5);
}

double probF(double f, UINT df1, UINT df2)
{
double prob;

prob = 2.0 * betai(0.5 * df2, 0.5 * df1, df2 / (df2 + df1 * f));

if(prob > 1) prob = 2.0 - prob; 
return(prob / 2.0);
}

#define STAT_CHI2 1
#define STAT_T 2
#define STAT_F 3

CELL * p_probabilityChi2(CELL * params)
{
return(probability_x(params, STAT_CHI2));
}

CELL * p_probabilityT(CELL * params)
{
return(probability_x(params, STAT_T));
}

CELL * p_probabilityF(CELL * params)
{
return(probability_x(params, STAT_F));
}


CELL * probability_x(CELL * params, int type)
{
double x;
UINT df1;
UINT df2;
double prob = 0.0;

params = getFloat(params, &x);
params = getInteger(params, &df1);
if(type == STAT_F)
    getInteger(params, &df2); 

switch(type)
    {
    case STAT_CHI2:
        prob = 1.0 - probChi2(x, df1);
        break;
    case STAT_T:
        prob = 1.0 - probT(x, df1);
        break;
    case STAT_F:
        prob = probF(x, df1, df2);
        break;
    default:
        break;
    }

return(stuffFloat(prob));
}


double critical_value(double p, UINT df1, UINT df2, double max_val, int type)
{
#define NEWTON_EPSILON 0.000001   /* Accuracy of Newton approximation */
double minval = 0.0;
double maxval = max_val;
double critval;
double prob = 0.0;

if (p <= 0.0) return 0.0;
else if (p >= 1.0) return maxval;
    
critval = (df1 + df2) / sqrt(p);    /* fair first value */
while ((maxval - minval) > NEWTON_EPSILON)
    {
    switch(type)
        {
        case STAT_CHI2:
            prob = probChi2(critval, df1);
            break;
        case STAT_T: 
            prob = probT(critval, df1);
            break;
        case STAT_F:
            prob = 1.0 - probF(critval, df1, df2);
            break;
        default:
            break;
        }
    if (prob < p) minval = critval;
    else maxval = critval;
    critval = (maxval + minval) * 0.5;
    }

return critval;
}


CELL * criticalX(CELL * params, int type)
{
double p;
UINT df1;
UINT df2 = 0;
double x;

params = getFloat(params, &p);
params = getInteger(params, &df1);
if(type == STAT_F) getInteger(params, &df2);

x = critical_value((1.0 - p), df1, df2, 99999.0, type);

if(x < NEWTON_EPSILON) x = 0.0;

return(stuffFloat(x));
}


CELL * p_criticalChi2(CELL  * params)
{
return(criticalX(params, STAT_CHI2));
}

CELL * p_criticalT(CELL * params)
{
return(criticalX(params, STAT_T));
}


CELL * p_criticalF(CELL  * params)
{
return(criticalX(params, STAT_F));
}


/* ----------------------- betai and gammaln fucntions ----------------------*/


static int paramError;

CELL * p_beta(CELL * params)
{
double a, b, beta;

params = getFloat(params, &a);
getFloat(params, &b);

beta = exp(gammaln(a) + gammaln(b) - gammaln(a+b));

return(stuffFloat(beta));
}

CELL * p_betai(CELL * params)
{
double a, b, x, result;

params = getFloat(params, &x);
params = getFloat(params, &a);
getFloat(params, &b);

paramError = 0;

result = betai(a,b,x);

if(paramError)
    return(nilCell);
    
return(stuffFloat(result));
}


CELL * p_gammaln(CELL * params)
{
double x, result;

getFloat(params, &x);

paramError = 0;

result = gammaln(x);

if(paramError)
    return(nilCell);
    
return(stuffFloat(result));
}


CELL * p_gammai(CELL * params)
{
double a, x, result;

params = getFloat(params, &a);
getFloat(params, &x);

result = gammap(a, x);

/*
printf("in p_gammai() result = %f\n", result);
*/

return(stuffFloat(result));
}


/* these functions are adapted from:
   Numerical Recipes in C
   W.H.Press, S.A. Teukolsky, W.T. Vettering, B.P. Flannery
   Cambridge University Press (c) 1992
*/

#define ITMAX 100
#define EPS7 3.0e-7

double betai(double a, double b, double x)
{
double bt;

if (x < 0.0 || x > 1.0) 
    {
    paramError = 1;
    return(0.0);
    }

if (x == 0.0 || x == 1.0) 
    bt = 0.0;
else
    bt = exp(gammaln(a+b)-gammaln(a)-gammaln(b)+a*log(x)+b*log(1.0-x));
    
if (x < (a+1.0)/(a+b+2.0))
    return (bt * betacf(a,b,x) / a);
else
    return (1.0 - bt * betacf(b,a,1.0-x) / b);
}


double betacf(double a, double b, double x)
{
double qap,qam,qab,em,tem,d;
double bz,bm,bp,bpp;
double az,am,ap,app,aold;
int m;

bm = az = am = 1.0;
qab=a+b;
qap=a+1.0;
qam=a-1.0;
bz=1.0-qab*x/qap;
for (m=1;m<=ITMAX;m++) 
    {
    em=(double) m;
    tem=em+em;
    d=em*(b-em)*x/((qam+tem)*(a+tem));
    ap=az+d*am;
    bp=bz+d*bm;
    d = -(a+em)*(qab+em)*x/((qap+tem)*(a+tem));
    app=ap+d*az;
    bpp=bp+d*bz;
    aold=az;
    am=ap/bpp;
    bm=bp/bpp;
    az=app/bpp;
    bz=1.0;
    if (fabs(az-aold) < (EPS7 * fabs(az))) return az;
    }

return(paramError = 1);
}


double gammaln(double xx)
{
double x,y,tmp,ser;
static double cof[6] = {
 76.18009172947146,
-86.50532032941677,
 24.01409824083091,
 -1.231739572450155,
  0.1208650973866179e-2,
 -0.5395239384953e-5};

int j;

if(xx == 0.0)
    fatalError(ERR_INVALID_PARAMETER_0, NULL, 0);

y=x=xx;
tmp=x+5.5;
tmp -= (x+0.5)*log(tmp);
ser=1.000000000190015;

for (j=0;j<=5;j++) ser += cof[j]/++y;
    
return -tmp+log(2.5066282746310005*ser/x);
}


#define EPS9 3.0e-9
#define FPMIN 1.0e-307

/* incomplete Gamma function
//
// gammap = P(a,x) 
// gammaq = Q(a,x) = 1.0 - P(a,x)
//
// prob-chi2 = gammap(df/2, chi2/2) 
// of beeing as good as observed (>=)
*/
double gammap(double a, double x)
{
double gln;
gln = gammaln(a);
#ifdef DEBUG
/* horrible error on LLVM 32-bit Apple update Oct 7, 2010, fine on LLVM 64-bit
   result is fine in printf() but gets passed up as NaN to gammai() by return() */
printf("in gammap() result %f\n", (x < (a + 1.0)) ? gser(a, x, gln) : (1.0 - gcf(a ,x , gln)));
#endif
return( (x < (a + 1.0)) ? gser(a, x, gln) : (1.0 - gcf(a ,x , gln)) );
}


static double gser(double a, double x, double gln)
{
double ap, del, sum;
int n;

ap = a;
del = 1.0/a;
sum = del;

for (n = 1 ; n <= ITMAX ; n++)
    {
    ++ap;
    del *= x / ap;
    sum += del;
    if (fabs(del) < fabs(sum) * EPS9)
    return sum * exp(-x + a * log(x) - gln);
    }

return sum * exp(-x + a * log(x) - gln);
}


double gcf(double a, double x, double gln)
{
double b, c, d, h, an, del;
int i;

b = x + 1 - a;
c = 1.0/FPMIN;
d = 1.0/b;
h = d;

for (i = 1 ; i <= ITMAX ; i++)
    {
    an = -i * (i - a);
    b += 2;
    
    d = an * d + b;
    if (fabs(d) < FPMIN) d = FPMIN;

    c = b + an/c;
    if (fabs(c) < FPMIN) c = FPMIN;

    d = 1.0/d;
    del = d * c;
    h *= del;
    if (fabs(del-1.0) < EPS9) break;
    }

return exp(-x + a * log(x) - gln) * h;
}


/* ------------------------------------- Binomial distribution -------------------- */

CELL * p_binomial(CELL * params)
{
INT n, k;
double bico, p, binomial;

params = getInteger(params, (UINT *)&n);
params = getInteger(params, (UINT *)&k);
getFloat(params, &p);

bico = exp(gammaln(n + 1.0) - gammaln(k + 1.0) - gammaln(n - k + 1.0));

binomial = bico * pow(p, (double)k) * pow(1.0 - p, (double)(n - k));

return(stuffFloat(binomial));
}

/* -------------------------------------------------------------------------------- */

CELL * p_series(CELL * params)
{
double fromFlt, factFlt;
ssize_t count, i;
CELL * series;
CELL * cell;
CELL * pCell;
CELL * expr;
CELL * cellIdx;
UINT * resultIdxSave;
jmp_buf errorJumpSave;
int errNo;

cell = evaluateExpression(params);
pCell = evaluateExpression(params->next);
params = params->next;
getInteger(params->next, (UINT *)&count);

series = getCell(CELL_EXPRESSION);
if(count <= 0) return(series);

if(isNumber(pCell->type))
    {
    if(!isNumber(cell->type))
        return(errorProcExt(ERR_NUMBER_EXPECTED, cell));
    fromFlt = getDirectFloat(cell);
    factFlt = getDirectFloat(pCell);
    cell = copyCell(cell);
    series->contents = (UINT)cell;
    for(i = 1; i < count; i++)
        {
        fromFlt *= factFlt;
        cell->next = stuffFloat(fromFlt);
        cell = cell->next;
        }
    }
else /* assumes lambda or primitive */
    {
    cellIdx = initIteratorIndex();
    addList(series, copyCell(cell));
    resultIdxSave = resultStackIdx;
    memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
    if((errNo = setjmp(errorJump)) != 0)
        {
        memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
        deleteList(series);
        longjmp(errorJump, errNo);
        }

    for(i = 1; i < count; i++)
        {
        cell = copyCell(cell);
        cleanupResults(resultIdxSave);  
        expr = makeCell(CELL_EXPRESSION, (UINT)copyCell(pCell));
        ((CELL *)expr->contents)->next = cell;
        pushResult(expr);
        cell = evaluateExpression(expr);
        addList(series, copyCell(cell));
        if(cellIdx->type == CELL_LONG) cellIdx->contents += 1;
        }
    recoverIteratorIndex(cellIdx);
    memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
    }

series->aux = (UINT)cell; /* last element optimization */
return(series);
}

/* ------------------------------- prime numbers ---------------------------- */

/*
* adapted for newLISP from the following code:
*
* factor.c -- print prime factorization of a number
* Ray Gardner -- 1985 -- public domain
* Modified Feb. 1989 by Thad Smith > public domain
*
*/

CELL * p_factor (CELL * params)
{
INT64 d, n;
INT k;
CELL * factList;
int i;

getInteger64Ext(params, &n, TRUE);

if (n < 2) return(nilCell);
      
factList = getCell(CELL_EXPRESSION);

d = 2;
for (k = 0; (n % d) == 0; k++)
    n /= d;
for(i = 0; i < k; i++) addList(factList, stuffInteger64(d));
      
for (d = 3; d * d <= n; d += 2)
    {
    for (k = 0; (n % d) == 0; k++)
        n /= d;
    for(i = 0; i < k; i++) addList(factList, stuffInteger64(d));
    }

if (n > 1)
    addList(factList, stuffInteger64(n));
      
return(factList);
}

/* see http://en.wikipedia.org/wiki/Euclidean_algorithm */

CELL * gcdBig(CELL * a, CELL * b);
CELL * isZero(CELL * cell);

CELL * p_gcd(CELL * params)
{ 
INT64 m, n, r; 
CELL * cell;
#ifdef BIGINT
CELL * x;
CELL * y;
UINT * resultIdxSave = resultStackIdx;
#endif

cell = evaluateExpression(params);
params = params->next;

#ifdef BIGINT
if(cell->type == CELL_BIGINT)
    {
    x = copyCell(cell);
    NEXT_BIG_GCD:
    y = p_bigInt(params);
    cell = gcdBig(x, y);
    cleanupResults(resultIdxSave);  
    deleteList(x);
    deleteList(y);
    if(params->next != nilCell)
        {
        params = params->next;
        x = cell;
        goto NEXT_BIG_GCD;
        }
    *(int*)cell->contents = 1; /* abs */
    return(cell);
    }
#endif

cell = getInteger64Ext(cell, &m, FALSE);
while(params != nilCell)
    {
    params = getInteger64Ext(params, &n, TRUE);
    while (n != 0)
        {
        r = n;
        n = m % n;
        m = r;
        } 
    }

if(m < 0) m = -m;
return(stuffInteger64(m));
} 


/* for bigint use 
(define (gcd-big a b)
	(if (= b 0) a (gcd-big b (% a b))))
*/

#define NEW_BIG_GCD
#ifdef NEW_BIG_GCD 
CELL * gcdBig(CELL * a, CELL * b)
{
CELL * cell;

while (isZero(b) == nilCell)
    {
    a->next = b;
    cell = arithmetikOp(a, OP_MODULO);
    a->next = nilCell;
    pushResult(cell);
    a = b;
    b = cell; 
    }

return(copyCell(a));
}

#else

CELL * gcdBig(CELL * a, CELL * b)
{
CELL * cell;
if(isZero(b) == trueCell) return(copyCell(a));

a->next = b;
b->next = nilCell;

cell = arithmetikOp(a, OP_MODULO);
a->next = nilCell;
pushResult(cell);

return(gcdBig(b, cell));
}
#endif

/* ------------------------------- financial functions ---------------------- */


CELL * p_pmt(CELL * params)
{
INT nper;
double rate, pv;
double fv = 0.0;
double pmt = 0.0;
double inc;
INT type = 0;

params = getFloat(params, &rate);
params = getInteger(params, (UINT *)&nper);
params = getFloat(params, &pv);
if(params != nilCell)
    {
    params = getFloat(params, &fv);
    if(params != nilCell) getInteger(params, (UINT *)&type);
    }

if(rate == 0)
    pmt = (-pv - fv) / nper;
else
    {
    inc = pow(1 + rate, (double)nper);
    pmt = - (pv * inc + fv) / ((1 + rate * type) * ((inc - 1) / rate));
    }
  
return stuffFloat(pmt);
}


CELL * p_pv(CELL * params)
{
INT nper;
double rate, pmt, pv;
double fv = 0.0;
double inc;
INT type = 0;

params = getFloat(params, &rate);
params = getInteger(params, (UINT *)&nper);
params = getFloat(params, &pmt);

if(params != nilCell)
    {
    params = getFloat(params, &fv);
    if(params != nilCell)
        getInteger(params, (UINT *)&type);
    }

if(rate == 0)
    pv = - pmt * nper - fv;
else
    {
    inc = pow(1 + rate, (double)nper);
    pv = (-pmt * (1 + rate * type) * ((inc - 1) / rate) - fv) / inc;
    }
  
return stuffFloat(pv);
}


CELL * p_fv(CELL * params)
{
double rate, pmt, pv;
INT nper, type;
double inc, fv;

params = getFloat(params, &rate);
params = getInteger(params, (UINT *)&nper);
params = getFloat(params, &pmt);
params = getFloat(params, &pv);
if(params != nilCell)
    getInteger(params, (UINT *)&type);
else type = 0;


if(rate == 0)
    fv = - pmt * nper - pv;
else
    {
    inc = pow(1 + rate, (double)nper);
    fv = -pmt * (1 + rate * type) * ((inc - 1) / rate) - pv * inc;
    }
  
return stuffFloat(fv);
}


CELL * p_nper(CELL * params)
{
double rate, pmt, pv;
double fv = 0.0;
double R, c, nper;
INT type = 0;

params = getFloat(params, &rate);
params = getFloat(params, &pmt);
params = getFloat(params, &pv);

if(params != nilCell)
    {
    params = getFloat(params, &fv);
    if(params != nilCell) getInteger(params, (UINT *)&type);
    }
 
if(rate == 0)
    nper = (-pv - fv) / pmt;
else if(rate > -1.0)
    {
    R = 1.0 + rate;
    c = pmt * (type ? R : 1.0) / rate;
    nper = log((c - fv) / (c + pv)) / log(R);
    }
else nper = sqrt(-1.0);   /* NaN */


return stuffFloat(nper);
}


CELL * p_npv(CELL * params)
{
CELL * list;
double rate, cashFlow, fNum;
int count;

params = getFloat(params, &rate);
list = evaluateExpression(params);
if(!isList(list->type))
    return(errorProcExt(ERR_LIST_EXPECTED, params));
list = (CELL *)list->contents;

cashFlow = 0.0;
count = 0;
while(list !=  nilCell)
    {
    fNum = getDirectFloat(list);
    cashFlow += fNum / pow((1.0 + rate), (double)++count);
    list = list->next;
    }

return stuffFloat(cashFlow);
}


/* Internal Rate of Return - irr

   adapted from a C++ program by:
  
   Bernt Arne Odegaard, 1999 http://finance.bi.no/~bernt

   Note, that some data have multiple solutions, this search
   algorithm returns only one

*/


double cfPv(int N, int times[], double amounts[], double rate)
{
double PV = 0.0;
int t;

for(t = 0; t < N; t++)
    PV += amounts[t] / pow((1.0 + rate), (double)times[t]);

return(PV);
}


#define PRECISION 1.0e-5
#define MAX_ITERATIONS 50
#define IRR_ERROR -1.0

double irr(int N, int times[], double amounts[], double x2)
{
double est1, est2;
double x1 = 0.0;
double f, rtb, dx;
double x_mid, f_mid;
int i;
    
est1 = cfPv(N, times, amounts,  x1);
est2 = cfPv(N, times, amounts,  x2);

for(i=0; i<MAX_ITERATIONS; i++)
    {
    if(est1 * est2 < 0.0) break;

    if(fabs(est1) < fabs(est2)) 
        est1 = cfPv(N, times, amounts,  x1 += 1.6 * (x1 - x2));
    else 
        est2 = cfPv(N, times, amounts,  x2 += 1.6 * (x2 - x1));
    }
    
if (est2 * est1 > 0.0) return (IRR_ERROR);

f = cfPv(N, times, amounts, x1);
dx=0;

if(f < 0.0) 
    {
    rtb = x1;
    dx=x2-x1;
    }
else 
    {
    rtb = x2;
    dx = x1-x2;     
    };

for(i = 0; i< MAX_ITERATIONS; i++)
    {
    dx *= 0.5;
    x_mid = rtb + dx;
    f_mid = cfPv(N, times, amounts, x_mid);

    if(f_mid <= 0.0)
        rtb = x_mid;

    if((fabs(f_mid) < PRECISION) || (fabs(dx) < PRECISION)) 
        return x_mid;
    }; 
   
return(IRR_ERROR);
}

CELL * p_irr(CELL * params)
{
CELL * amounts;
CELL * times;
CELL * list;
double result;
double guess = 0.5;
double * amountsVec;
int * timesVec;
int i, N = 0;
INT number;

params = getListHead(params, &amounts);
list = amounts;
if(params != nilCell)
    {
    params = getListHead(params, &times);
    if(params != nilCell)
        getFloat(params, &guess);
    }   

else
    times = NULL;

while(list != nilCell)
    {
    ++N;
    list = list->next;
    }

amountsVec = (double *)allocMemory(N * sizeof(double));
timesVec = (int *)allocMemory(N * sizeof(int));

for(i = 0; i < N; i++)
    {
    if(isNumber(amounts->type))
        {
        amountsVec[i] = getDirectFloat(amounts);
        amounts = amounts->next;
        }
    else 
        {
        freeMemory(amountsVec);
        freeMemory(timesVec);
        return(errorProcExt(ERR_NUMBER_EXPECTED, amounts));
        }

    if(times == NULL) 
        timesVec[i] = i + 1;
    else
        {
        times = getIntegerExt(times, (UINT *)&number, FALSE);
        timesVec[i] = number;
        }
    }

result = irr(N, timesVec, amountsVec, guess);
if(result < 0.00001) 
    result = 0.0;
else
    {
    result = floor((10000 * result + 0.5))/10000;
    }

freeMemory(amountsVec);
freeMemory(timesVec);

if(result == IRR_ERROR)
    return(nilCell);

return(stuffFloat(result));
}

/* ----------------------------------- CRC32 ----------------------- */

/* see also https://github.com/mattsta/crcspeed/blob/master/crc64speed.c
   for fast 64-bt crc and here: https://matt.sh/redis-crcspeed
*/

#ifdef CRC16
unsigned short crc16(unsigned char * buff, int len)
{
int i;
unsigned short acc = 0xFFFF;

for(i = 0; i < len; i++)
    {
    acc ^= buff[i];
    acc  = (acc >> 8) | (acc << 8);
    acc ^= (acc & 0xff00) << 4;
    acc ^= (acc >> 8) >> 4;
    acc ^= (acc & 0xff00) >> 5;
    }

return(acc);
}
#endif

/* Algorithm from: http://www.w3.org/TR/PNG-CRCAppendix.html */

CELL * p_crc32(CELL * params)
{
char * data;
size_t len;
unsigned int crc;

getStringSize(params, &data, &len, TRUE);
crc = update_crc(0xffffffffL, (unsigned char *)data, (int)len) ^ 0xffffffffL;
return(stuffInteger64(crc));
}

/* Update a running CRC with the bytes buf[0..len-1]--the CRC
   should be initialized to all 1's, and the transmitted value
   is the 1's complement of the final running CRC (see the
   crc() routine below)). 
*/
   
unsigned int update_crc(unsigned int crc, unsigned char *buf, int len)
{
/* unsigned int crc_table[256]; */
static unsigned int * crc_table = NULL; 
unsigned int c;
int n, k;

if(crc_table == NULL)
    { 
    /* make crc table */  
    crc_table = (unsigned int *)malloc(sizeof(unsigned int) * 256);
    for (n = 0; n < 256; n++) 
        {
        c = (unsigned int) n;
        for (k = 0; k < 8; k++) 
            {
            if (c & 1)
                c = 0xedb88320L ^ (c >> 1);
            else
            c = c >> 1;
            }
        crc_table[n] = c;
        }
    }

c = crc;   

/* update crc */
for (n = 0; n < len; n++) 
    c = crc_table[(c ^ buf[n]) & 0xff] ^ (c >> 8);

return c;
}


/* ----------------------------------- Bayesian statistics -------------------------- */

/* 

   (bayes-train M1 M2 [M3 ... Mn] D)

   takes two or more lists M1, M2 ... with tokens from a joint set of tokens T.
   Token can be symbols or strings other datatypes are ignored.
   Tokerns are placed in a common dictionary in context D and the frquency 
   is counted  how often they occur in each category Mi.
   
   The M categories represent data models for which a sequence of tokens can be
   classified see (bayes-query ...)
   
   Each token in D is a content addressable symbol in D containing a
   list of ferquencies of that token how often it occurs in each category.
   String tokens are prepended with an undersocre _ before convering them
   to a symbol in D.
   
   The function returns a list of token numbers in the different category models: 
   (N-of-tokens-in-M1 N-of-tokens-in-M2)
   
   (bayes-train M1 M2 [M3 ... Mn] D)

*/ 

CELL * p_bayesTrain(CELL * params)
{
CELL * * category;
CELL * list;
CELL * count;
int * total;
int i, idx, maxIdx = 0;
SYMBOL * ctx;
SYMBOL * sPtr;
SYMBOL * totalPtr;
char * token;

list = params;
while(list != nilCell) list = list->next, maxIdx++;
--maxIdx; /* last is a context not a category */
if(maxIdx < 1) errorProc(ERR_MISSING_ARGUMENT);

category = alloca(maxIdx * sizeof(CELL *));
total = alloca(maxIdx * sizeof(int));
token = alloca(MAX_SYMBOL + 1);

for(idx = 0; idx < maxIdx; idx++)
  {
  params = getListHead(params, &list);
  category[idx] = list;
  }
  
if((ctx = getCreateContext(params, TRUE)) == NULL)
  return(errorProcExt(ERR_SYMBOL_OR_CONTEXT_EXPECTED, params));
            
for(idx = 0; idx < maxIdx; idx++)
  {
  list = category[idx];
  total[idx] = 0;
  while(list != nilCell)    
    {

    switch(list->type)
      {
      case CELL_STRING:
        if(list->aux > MAX_SYMBOL + 1) continue;
        *token = '_';
        memcpy(token + 1, (char *)list->contents, list->aux);
        break;
      case CELL_SYMBOL:
        strncpy(token, ((SYMBOL *)list->contents)->name, MAX_SYMBOL + 1);
        break;
      }
    
    sPtr = translateCreateSymbol(token, CELL_EXPRESSION, ctx, TRUE);
    if(((CELL*)sPtr->contents)->type == CELL_NIL)
      {
      /* create list with maxIdx 0s */
      count = getCell(CELL_EXPRESSION);
      sPtr->contents = (UINT)count;
      count->contents = (UINT)stuffInteger(0);
      count = (CELL *)count->contents;
      for(i = 1; i < maxIdx; i++)
        {
        count->next = stuffInteger(0);
        count = count->next;
        }
      }

    /* increment value at idx */
    count = (CELL *)sPtr->contents;
    if(count->type != CELL_EXPRESSION)
      return(errorProcExt(ERR_LIST_EXPECTED, count));
    count = (CELL *)count->contents;
    for(i = 0; i < idx; i++) 
      count = count->next;
    if(count->type == CELL_LONG)    
      count->contents++;  
#ifndef NEWLISP64
    else if(count->type == CELL_INT64)
      *(INT64 *)&count->aux += 1;
#endif
    else
      return(errorProcExt(ERR_NUMBER_EXPECTED, count));
    total[idx]++;
    list = list->next;
    } /* done category idx */
  }

totalPtr = translateCreateSymbol("total", CELL_EXPRESSION, ctx, TRUE);
if(((CELL *)totalPtr->contents)->type == CELL_NIL)
  {
  count = getCell(CELL_EXPRESSION);
  totalPtr->contents = (UINT)count;
  count->contents = (UINT)stuffInteger(0);
  count = (CELL *)count->contents;
  for(i = 1; i < maxIdx; i++)
    {
    count->next = stuffInteger(0);
    count = count->next;
    }
  }

count = (CELL *)totalPtr->contents;
if(count->type != CELL_EXPRESSION)
    return(errorProcExt(ERR_LIST_EXPECTED, count));
count = (CELL *)count->contents;
for(i = 0; i < maxIdx; i++)
  {
  if(count->type == CELL_LONG)
    count->contents += total[i];
#ifndef NEWLISP64
  else if(count->type == CELL_INT64)
    *(INT64 *)&count->aux += total[i];
#endif
  count = count->next;
  }    

return(copyCell((CELL *)totalPtr->contents));
}

/* 
   (bayes-query L D [trueChainedBayes])
   (bayes-query L D [trueChainedBayes] [trueFloatProbs)
   
   Takes a list of tokens L and a trained dictionary D and returns a list of the 
   combined probabilities of the tokens to be in one category A versus the other 
   categories B. All token in L should occur in D, for tokens which are not in D
   equal probability is asssumed over categories.
   
   Token can be strings or symbols. If token are strings, they are prepended with 
   an underscore when looked up in D. When frequencies in D were learned using 
   bayes-train, the underscore was automatically prepended during learning.

   for 2 categories:
   
                     p(tkn|A) * p(A)
   p(A|tkn) = ---------------------------------
              p(tkn|A) * p(A) + p(tkn|B) * p(B)
              
   p(A|tkn) is the posterior for A which gets prior p(A) for the next token the 
   priors: p(A) and p(B) = 1 are substituted after every token. p(A) = p(not B).

   for N categories:
   
                    p(tkn|Mc) * p(Mc)
   p(Mc|tkn) = -------------------------------    ; Mc is one of N categories
                sum-i-N( p(tkn|Mi) * p(Mi) ) 
              
   
   When in chain Bayes mode p(Mc|tkn) is the posterior for Mc and replaces the 
   prior p(Mc) for the next token. In chain Bayes mode tokens with 0 frequency 
   in one category will effectiviely put the probability of this category to 0. 
   This causes queries resulting in 0 probabilities for all categories to yield 
   NaN values. 
   
   The pure chain Bayes mode is the more sensitive one for changes, when a token 
   count of 0 occurs the resulting probability goes to a complete 0 in this 
   category, while other categories gain higher probabilities. In the Fisher's  
   Chi2 mode the the zero-category is still assigned a probability resulting from 
   other tokens with non-zero counts.
*/

CELL * p_bayesQuery(CELL * params)
{
CELL * tokens;
CELL * tkn;
char * token;
SYMBOL * ctx;
SYMBOL * sPtr;
SYMBOL * totPtr;
CELL * count;
CELL * total;
int i, idx; 
int nTkn = 0;   /* number of token in query */
int maxIdx = 0; /* number of catagories */
int N = 0;      /* total of tokens in all categories */
int chainBayes, probFlag;
double * priorP;
double * postP;
double * p_tkn_and_cat;
double p_tkn_and_all_cat;
double * Pchi2 = NULL;
double * Qchi2 = NULL;
INT countNum, totalNum;

CELL * result = NULL;
CELL * cell = NULL;

params = getListHead(params, &tokens);
params = getContext(params, &ctx);

chainBayes = getFlag(params);
probFlag = getFlag(params->next);

totPtr = translateCreateSymbol("total", CELL_EXPRESSION, ctx, FALSE);
if(totPtr == NULL)  return(nilCell);

total = (CELL *)totPtr->contents;
if(total->type != CELL_EXPRESSION)
    return(errorProcExt(ERR_LIST_EXPECTED, (CELL *)totPtr->contents));

/* 
   Get number of categories maxIdx and total N in all categories.
   If no probabilities are specified get frequencies/counts  
*/
total = (CELL *)total->contents;
while(total != nilCell)
    {
    if(probFlag == FALSE)
        {
        if(total->type == CELL_LONG)
            N += total->contents;
#ifndef NEWLISP64
        else if(total->type == CELL_INT64)
            N += *(INT64 *)&total->aux;
#endif
        }
    total = total->next;
    maxIdx++;
    }

/* allocate memory for priors and posts for each category */ 
priorP = alloca(maxIdx * sizeof(double));
postP = alloca(maxIdx * sizeof(double));
p_tkn_and_cat = alloca(maxIdx * sizeof(double));

/* allocate memory fo Chi-2 probs for each category */
if(!chainBayes)
    {
    Pchi2 = alloca(maxIdx * sizeof(double));
    Qchi2 = alloca(maxIdx * sizeof(double));
    }

/* calculate the prior prob for each category */
total = (CELL *)((CELL *)totPtr->contents)->contents;
for(idx = 0; idx < maxIdx; idx++) 
    {
    if(probFlag == TRUE)
#ifndef NEWLISP64
        priorP[idx] = *(double *)&total->aux; 
#else
        priorP[idx] = (double)total->contents;
#endif
    else
        {
        if(total->type == CELL_LONG)
            priorP[idx] = (double)total->contents / N;
#ifndef NEWLISP64
        else /* INT64 */
            priorP[idx] = (double)*(INT64 *)&total->aux / N;
#endif
        }

#ifdef BAYES_DEBUG
    printf("priorP[%d]=%f probability of category %d\n", idx, priorP[idx], idx);  
#endif                                    

    total = total->next;
    /* initialize Fisher's Chi-2 probs and priors */
    if(!chainBayes) 
        {
        Pchi2[idx] = Qchi2[idx] = 0.0;
        priorP[idx] = 1.0 / maxIdx;
        }
    }

token = alloca(MAX_SYMBOL + 1);
tkn = tokens;
while(tkn != nilCell) tkn = tkn->next, nTkn++;

/* for each token  calculate p(tkn|M) in each category M */
tkn = tokens;
for(i = 0; i < nTkn; i++)
    {
    switch(tkn->type)
      {
      case CELL_STRING:
        if(tkn->aux > MAX_SYMBOL + 1) continue;
        *token = '_';
        memcpy(token + 1, (char *)tkn->contents, tkn->aux);
        break;
      case CELL_SYMBOL:
        strncpy(token, ((SYMBOL *)tkn->contents)->name, MAX_SYMBOL + 1);
        break;
      }

    if((sPtr = lookupSymbol((char *)token, ctx)) == NULL) 
        {
        /* skip the token if it doesn't occur in data */
        tkn = tkn->next;
        continue;
        }

    count = (CELL *)sPtr->contents;
    if(count->type != CELL_EXPRESSION) continue;
  
    count = (CELL *)(CELL *)count->contents;
    total = (CELL *)((CELL *)totPtr->contents)->contents;
     
    /* in each category M[idx] */ 
    p_tkn_and_all_cat = 0.0;
    for(idx = 0; idx < maxIdx; idx++)
        {
        if(probFlag)
#ifndef NEWLISP64
            p_tkn_and_cat[idx] = *(double *)&count->aux * priorP[idx];
#else
            p_tkn_and_cat[idx] = *(double *)&count->contents * priorP[idx];
#endif  
        else /*   p(M) * p(tkn|M) */         
            {
            /* count of token in category idx */
            getInteger(count, (UINT *)&countNum); 
            /* total of all tokens in category idx */
            getInteger(total, (UINT *)&totalNum); 
            p_tkn_and_cat[idx] = priorP[idx] * countNum / totalNum;
            }
        
        /* accumulate probability of token in all cataegories */
        p_tkn_and_all_cat += p_tkn_and_cat[idx];

#ifdef BAYES_DEBUG
        printf("token[%d] p(M[%d]) * p(tkn|M[%d])=%lf * %ld / %ld = %lf\n", i, idx, idx, 
            priorP[idx], countNum, totalNum, (double)p_tkn_and_cat[idx]);
#endif
        count = count->next;
        total = total->next;
        }  
        
    for(idx = 0; idx < maxIdx; idx++)
        {
        if(!chainBayes)
            {
            postP[idx] = p_tkn_and_cat[idx] / p_tkn_and_all_cat;
            
            /* handle probability limits of 0 and using very small value */
            if(postP[idx] == 0.0)
                Qchi2[idx] += log(3e-308) * -2.0;
            else
                Qchi2[idx] += log(postP[idx]) * -2.0;
                       
            if(postP[idx] == 1.0) 
                Pchi2[idx] += log(3e-308) * -2.0; 
            else 
                Pchi2[idx] += log(1.0 - postP[idx]) * -2.0;
            } 
        else 
            {
            postP[idx] = p_tkn_and_cat[idx] / p_tkn_and_all_cat; 
            priorP[idx] = postP[idx]; 
            }
#ifdef BAYES_DEBUG
        printf("postP[%d] = p_tkn_and_cat[%d] / p_tkn_and_all_cat = %lf / %lf = %lf\n", 
          idx, idx, p_tkn_and_cat[idx], p_tkn_and_all_cat, postP[idx]);
#endif
        }
        
    tkn = tkn->next;
    }
    
if(!chainBayes)
    {
    for(idx = 0; idx < maxIdx; idx++)
      {
#ifdef BAYES_DEBUG
        printf("Qchi2[%d] = %f, Pchi2[%d] = %f, nTkn = %d\n", idx, Qchi2[idx], idx, Pchi2[idx], nTkn);
#endif
      /* corrected swapped Pchi2[idx] and Qchi2[idx] in 10.5.5 */
      postP[idx] = (probChi2(Pchi2[idx],  2 * nTkn) - probChi2(Qchi2[idx],  2 * nTkn)  + 1.0) / 2.0;
      }
    }
                                                
    
for(idx = 0; idx < maxIdx; idx++)
    {
    if(idx == 0)
        {
        result = getCell(CELL_EXPRESSION);
        result->contents = (UINT)stuffFloat(*(postP + idx));
        cell = (CELL *)result->contents;
        }
    else
        {
        cell->next = stuffFloat(*(postP + idx));
        cell = cell->next;
        }
    }

return(result);
}

/*
//
// Copyright (C) 1992-2015 Lutz Mueller <lutz@nuevatec.com>
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 2, 1991,
// as published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
*/

/*
 'unify' for Prolog like unification of s-expressions:
  (unify '(f (g A) A) '(f B xyz)) => binds A to xyz and B to (g xyz)

  variable symbols must start with an upper-case letter, variables
  containing nil are considered unbound

  after linear left to right unification each variable symbol is expanded
  by all other variable symbols
*/

#ifdef SUPPORT_UTF8
#include <wctype.h>
#endif

typedef struct
    {
    CELL * left;
    CELL * right;
    void * next;
    } TERMSET;

void pushSet(TERMSET * * root, CELL * left, CELL * right);
CELL * unify(CELL * left, CELL * right);
int unifyGetType(CELL * cell);
void substitute(CELL * expr, CELL * sym, TERMSET * tset);
CELL * subsym(CELL * expr, CELL * sym, CELL * cell);
CELL * finishUnify(CELL * result);
int occurCheck(CELL * symCell, CELL * expr);
void printStack(TERMSET * tset);
void freeTermSet(TERMSET * * tset);

TERMSET * mgu = NULL; /* most general unifier */
TERMSET * ws = NULL;

int bindFlag;

#ifdef DEBUG
int debugFlag;
#endif

CELL * p_unify(CELL * params)
{
CELL * left, * right;
CELL * envHead;
left = evaluateExpression(params);
params = params->next;
right = evaluateExpression(params);

if((params = params->next) != nilCell)
    {
    params = getListHead(params, &envHead);
    while(envHead != nilCell)
        {
        if(envHead->type != CELL_EXPRESSION)
            return(errorProcExt(ERR_LIST_EXPECTED, envHead));
        pushSet(&ws, copyCell((CELL*)envHead->contents),
                    copyCell(((CELL*)envHead->contents)->next));
        envHead = envHead->next;
        }
    }

bindFlag =  getFlag(params);

#ifdef DEBUG
debugFlag =  getFlag(params->next);
if(debugFlag) printStack(ws);
#endif

return(unify(left, right));
}

#define UNIFY_ATOM 0
#define UNIFY_LIST 1
#define UNIFY_VAR 2
#define UNIFY_ANY 3

void pushSet(TERMSET * * root, CELL * left, CELL * right)
{
TERMSET * set;

set = (TERMSET *)callocMemory(sizeof(TERMSET));

set->left = left;
set->right = right;
set->next = *root;
*root = set;
}

void  popSet(TERMSET * * root, CELL * * left, CELL * * right)
{
TERMSET * set;

set = *root;
*root = set->next;

*left = set->left;
*right = set->right;

freeMemory(set);
}


CELL * unify(CELL * left, CELL * right)
{
int leftType, rightType;
CELL * lCell, * rCell;

pushSet(&ws, copyCell(left), copyCell(right));

while(ws != NULL)
    {
    popSet(&ws, &left, &right);

#ifdef DEBUG
    if(debugFlag)
        {
        printf("unify:");
        printCell(left, FALSE, OUT_CONSOLE);
        printf(" ");
        printCell(right, FALSE, OUT_CONSOLE);
        printf("\n");
        }
#endif

    leftType = unifyGetType(left);
    rightType = unifyGetType(right);

    if(leftType == UNIFY_ANY || rightType == UNIFY_ANY)
        {
        deleteList(left);
        deleteList(right);
        continue;
        }

    if( (leftType == UNIFY_ATOM && rightType == UNIFY_ATOM) ||
        (left->contents == right->contents))
        {
        if(compareCells(left, right) == 0)
            {
            deleteList(left);
            deleteList(right);
            continue;
            }
        else
            {
            deleteList(left);
            deleteList(right);
            return(finishUnify(nilCell));
            }
        }

    if(leftType == UNIFY_VAR && !occurCheck(left, right))
        {
        substitute(right, left, mgu); /* expand(right-expr, left-sym) in mgu set */
        substitute(right, left, ws);  /* expand(right-expr, left-sym) in ws set */

#ifdef DEBUG
        if(debugFlag)
            {
            printf("ws stack\n");
            printStack(ws);
            }
#endif
        pushSet(&mgu, left, right);
#ifdef DEBUG
        if(debugFlag)
            {
            printf("mgu stack\n");
            printStack(mgu);
            }
#endif
        continue;
        }

    if(rightType == UNIFY_VAR && !occurCheck(right, left))
        {
        substitute(left, right, mgu); /* expand(left-expr, right-sym) in mgu set */
        substitute(left, right, ws); /* expand(left-expr, right-sym) in ws set */
#ifdef DEBUG
        if(debugFlag)
            {
            printf("ws stack\n");
            printStack(ws);
            }
#endif
        pushSet(&mgu, right, left);
#ifdef DEBUG
        if(debugFlag)
            {
            printf("mgu stack\n");
            printStack(mgu);
            }
#endif
        continue;
        }

    if(leftType == UNIFY_LIST && rightType == UNIFY_LIST)
        {
#ifdef DEBUG
        if(debugFlag)
            {
            printf("lists:");
            printCell(left, FALSE, OUT_CONSOLE);
            printf(" ");
            printCell(right, FALSE, OUT_CONSOLE);
            printf("\n");
            }
#endif
        if(listlen((CELL*)left->contents) != listlen((CELL*)right->contents))
            {
            deleteList(left);
            deleteList(right);
            return(finishUnify(nilCell));
            }
        
        lCell = (CELL*)left->contents;
        rCell = (CELL*)right->contents;
        while(lCell != nilCell)
            {
            pushSet(&ws, copyCell(lCell), copyCell(rCell));
            lCell = lCell->next;
            rCell = rCell->next;
            }
        deleteList(left);
        deleteList(right);
        continue;
        }

    deleteList(left);
    deleteList(right);
    return(finishUnify(nilCell));
    }

return(finishUnify(trueCell));
}

int unifyGetType(CELL * cell)
{
SYMBOL * sPtr;
int wchar;

if(isSymbol(cell->type))
    {
    if(cell->type == CELL_SYMBOL)
        sPtr = (SYMBOL *)cell->contents;
    else
        sPtr = getDynamicSymbol(cell);

    if(*sPtr->name == '_' && *(sPtr->name + 1) == 0)
        return(UNIFY_ANY);
    
#ifndef SUPPORT_UTF8
    wchar = *sPtr->name;
#else
    utf8_wchar(sPtr->name, &wchar);
#endif
    return((wchar > 64 && wchar < 91) ? UNIFY_VAR : UNIFY_ATOM);
    }
else if(isList(cell->type))
    return(UNIFY_LIST);

return(UNIFY_ATOM);
}


int occurCheck(CELL * symCell, CELL * expr)
{
CELL * cell;

if(!isEnvelope(expr->type))
    return(symCell->contents == expr->contents);

cell = (CELL*)expr->contents;

while(cell != nilCell)
    {
    if(symCell->contents == cell->contents) return(1);
    if(isEnvelope(cell->type) && occurCheck(symCell, cell)) return(1);
    cell = cell->next;
    }

return(0);
}

void substitute(CELL * expr, CELL * sym, TERMSET * tset)
{
if(tset == NULL) 
    {
#ifdef DEBUG
    if(debugFlag)
        {
        printf("empty set in substitute %s for ", ((SYMBOL*)sym->contents)->name);
        printCell(expr, FALSE, OUT_CONSOLE);
        printf("\n");
        }
#endif
    return;
    }

while(tset != NULL)
    {
#ifdef DEBUG
    if(debugFlag)
        {
        printf("substitute %s for ", ((SYMBOL*)sym->contents)->name);
        printCell(expr, FALSE, OUT_CONSOLE);
        printf("\n");
        printf("left:");
        printCell(tset->left, FALSE, OUT_CONSOLE);
        }
#endif
    tset->left = subsym(expr, sym, tset->left);
#ifdef DEBUG
    if(debugFlag)
        {
        printf("->");
        printCell(tset->left, FALSE, OUT_CONSOLE);
        printf(" right:");
        printCell(tset->right, FALSE, OUT_CONSOLE);
        }
#endif
    tset->right = subsym(expr, sym, tset->right);
#ifdef DEBUG
    if(debugFlag)
        {
        printf("->");
        printCell(tset->right, FALSE, OUT_CONSOLE);
        printf("\n");
        }
#endif
    tset = tset->next;
    }
}


CELL * subsym(CELL * expr, CELL * sym, CELL * cell)
{
SYMBOL * sPtr;
CELL * sCell;

sPtr = (SYMBOL *)sym->contents;
sCell = (CELL *)sPtr->contents;
sPtr->contents = (UINT)expr;

if(isList(cell->type) || cell->type == CELL_QUOTE)
    {
    expr =  expand(copyCell(cell), sPtr);
    deleteList(cell);
    }
else
    {
    if(cell->contents == (UINT)sPtr)
        {
        expr = copyCell(expr); 
        deleteList(cell);
        }
    else
        expr = cell;
    }

sPtr->contents = (UINT)sCell;
return(expr);
}

CELL * finishUnify(CELL * result)
{
CELL * left, * right;
CELL * list = NULL;
CELL * assoc,  * cell = NULL;
SYMBOL * sPtr;

if(result == nilCell)
    {
    freeTermSet(&mgu);
    freeTermSet(&ws);
    return(nilCell);
    }

/* make an association list of all bound variables */
while(mgu != NULL)
    {
    popSet(&mgu, &left, &right);

    if(!bindFlag)
        {
        assoc = getCell(CELL_EXPRESSION);
        assoc->contents = (UINT)left;
        left->next = right;
        if(list == NULL)
            {
            list = getCell(CELL_EXPRESSION);
            list->contents = (UINT)assoc;
            }
        else
            cell->next = assoc;
        cell = assoc;
        }
    else
        {
        sPtr = (SYMBOL*)left->contents;
        if(isProtected(sPtr->flags))
            return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(sPtr)));
        sPtr->contents = (UINT)right;
        }

    }

freeTermSet(&ws);

if(!list || bindFlag) return(getCell(CELL_EXPRESSION));
return(list);
}       


void freeTermSet(TERMSET * * tset)
{
TERMSET * set;

while(*tset != NULL)
    {
    set = *tset;
    deleteList(set->left);
    deleteList(set->right);
    *tset = set->next;
    freeMemory(set);
    }
}

#ifdef DEBUG
void printStack(TERMSET * tset)
{
while(tset != NULL)
    {
    printCell(tset->left, FALSE, OUT_CONSOLE);
    printf("<->");
    printCell(tset->right, FALSE, OUT_CONSOLE);
    printf("\n");
    tset = tset->next;
    }
}
#endif

/* ---------------------------------- end unify ----------------------------- */

CELL * p_bits(CELL * params) 
{ 
int count = 0;
int i; 
char temp[65];
char result[65];
INT64 num;
CELL * resultList;

params = getInteger64Ext(params, &num, TRUE);

do  {
    temp[count++] = (num % 2) != 0;
    num = num >> 1; 
    } while (num != 0 && count < 64);

if(getFlag(params))
    {
    resultList = getCell(CELL_EXPRESSION);
    for(i = 0; i < count; i++) 
        addList(resultList, (copyCell (temp[i] ? trueCell : nilCell)));
    return(resultList);
    }

for(i = 0; i < count; i++) 
    result[i] = temp[count - 1 - i] + 48;
 
return(stuffStringN(result, count));
}


#define BOOL_EVEN 0
#define BOOL_ODD 1

CELL * isOddEven(CELL * params, int type)
{
INT64 num;

params = evaluateExpression(params);

#ifdef BIGINT
if(params->type == CELL_BIGINT)
    num = *((int *)params->contents + params->aux - 1);
else
#endif
    getInteger64Ext(params, &num, FALSE);

if(type == BOOL_EVEN)
    return((0 == (num & 1)) ? trueCell : nilCell);
else /* BOOL_ODD */
    return((0 == (num & 1)) ? nilCell : trueCell);
}

CELL * p_isOdd(CELL * params)
{ return(isOddEven(params, BOOL_ODD)); }

CELL * p_isEven(CELL * params)
{ return(isOddEven(params, BOOL_EVEN)); }

/* used when passing 32bit floats to library routines */
CELL * p_flt(CELL * params)
{
double dfloatV;
float floatV;
unsigned int number;

getFloat(params, &dfloatV);

floatV = dfloatV;
memcpy(&number, &floatV, 4);

return(stuffInteger64(number));
}


/* ------------------------------ basic statistics ------------------------- */

double * getVector(CELL * data, UINT * N, double * Sum, double * Mean, double * Sd2)
{
CELL * cell;
UINT idx, n;
double * vector = NULL;
double value, sum, ssq, sd2;
CELL * * addr;

n = sum = ssq = 0;
if(data->type == CELL_EXPRESSION)
    {
    if((data = cell = (CELL *)data->contents) == nilCell)
        return(NULL);
    n = 1;
    while(cell->next != nilCell) 
        { cell = cell->next; n++; }
    cell = data;
    vector = callocMemory(sizeof(double) * n);
    for (idx = 0; idx < n; idx++)
        {
        value = getDirectFloat(cell);
        *(vector + idx) = value;
        sum += value;
        ssq += value * value;
        cell = cell->next;
        }
    }

else if(data->type == CELL_ARRAY)
    {
    addr = (CELL * *)data->contents;
    n = (data->aux - 1) / sizeof(CELL *);
    vector = callocMemory(sizeof(double) * n);
    for (idx = 0; idx < n; idx++)
        {
        cell = *(addr + idx);
        value = getDirectFloat(cell);
        *(vector + idx) = value;
        sum += value;
        ssq += value * value;
        }
    }
else return(NULL);
	
sd2 = ssq - sum * sum / n;

*N = n;
*Sum = sum;
*Mean = sum / n;
*Sd2 = sd2;

return(vector);
}


CELL * p_stats(CELL * params)
{
CELL * cell;
UINT N = 0;
double sum, mean, sd2, avdev, sdev, var, skew, curt, s;
double * vector;
int idx;

if((vector = getVector(evaluateExpression(params), &N, &sum, &mean, &sd2)) == NULL)
    return(errorProc(ERR_LIST_OR_ARRAY_EXPECTED));

var = sd2 / (N - 1);    /* pop variance */
sdev = sqrt(var);       /* standard deviation */

skew = curt = avdev = 0.0;
for(idx = 0; idx < N; idx++)
    {
    s = vector[idx] - mean;
    avdev += fabs(s);
    skew += s * s * s;
    curt += s * s * s * s;
    }
avdev /= N;

freeMemory(vector);

if(var != 0.0)
    {
    skew = skew / (N * var * sdev);
    curt = curt / (N * var * var) - 3.0; 
    }
else skew = curt = 0.0;

cell = getCell(CELL_EXPRESSION);
addList(cell, stuffInteger(N));
addList(cell, stuffFloat(mean));
addList(cell, stuffFloat(avdev));
addList(cell, stuffFloat(sdev));
addList(cell, stuffFloat(var));
addList(cell, stuffFloat(skew));
addList(cell, stuffFloat(curt));

return(cell);
}

CELL * p_ttest(CELL * params)
{
UINT Nx, Ny, df;
double sumx, sumy, sumxy;
double meanx, meany;
double sd2x, sd2y;
double varx, vary;
double sdevx, sdevy;
double * X;
double * Y;
double svar, cov, t, tprob;
CELL * cell;
int dependent = FALSE, idx;
double pF = 0.0;

if((X = getVector(evaluateExpression(params), &Nx, &sumx, &meanx, &sd2x)) == NULL)
    return(errorProc(ERR_LIST_OR_ARRAY_EXPECTED));
params = params->next;

cell = evaluateExpression(params);
if(isNumber(cell->type)) /* One sample t-test */
    {
    meany = getDirectFloat(cell);
    sdevx = sqrt(sd2x / (Nx - 1));
    sdevy = sdevx / sqrt(Nx); /* standard error of difference */
    t = (meanx - meany) / ( sdevx / sqrt(Nx) );
    df = Nx - 1;
    goto TTEST_RESULT;
    }

if((Y = getVector(evaluateExpression(params), &Ny, &sumy, &meany, &sd2y)) == NULL)
    return(errorProc(ERR_LIST_OR_ARRAY_EXPECTED));

sumxy = 0.0;
for(idx = 0; idx < Nx; idx++)
    sumxy += X[idx] * Y[idx];

freeMemory(X);
freeMemory(Y);

cell = evaluateExpression(params->next);
if(cell == trueCell)
    {
    dependent = TRUE;
    if(Nx != Ny)
        return(errorProc(ERR_WRONG_DIMENSIONS));
    }
else if(isNumber(cell->type))
    pF = getDirectFloat(cell);

varx = sd2x / (Nx - 1);
vary = sd2y / (Ny - 1);
sdevx = sqrt(varx);
sdevy = sqrt(vary);
cov = sumxy - sumx * sumy / Nx;

/* t for related samples  Nx = Ny */
if(dependent)
    {
    df = Nx - 1;
    cov /= df;
    t = (meanx - meany) / sqrt((varx + vary - 2.0 * cov) / Nx);
    }
/* t for different means in non-related samples */
else
    {
    /* if varx and vary are not equal calc Welch Student's t */
    if(pF != 0.0 && probF(varx/vary, Nx - 1, Ny - 1) < pF) 
        {
        t = (meanx - meany) / sqrt(varx/Nx + vary/Ny);
        df = ((varx/Nx + vary/Ny) * (varx/Nx + vary/Ny)) / 
             (((varx/Nx) * (varx/Nx)) / (Nx - 1) + ((vary/Ny) * (vary/Ny))/(Ny - 1));
        }
    /* assume varx and vary are equal */
    else
        {
        df = Nx + Ny - 2;
        svar = (sd2x + sd2y) / df;
        t = (meanx - meany) / sqrt(svar * (1.0/Nx + 1.0/Ny));
        }
    }

TTEST_RESULT:
tprob = 2.0 * (1.0 - probT(fabs(t), df)); /* two tailed */
cell = getCell(CELL_EXPRESSION);
addList(cell, stuffFloat(meanx));
addList(cell, stuffFloat(meany));
addList(cell, stuffFloat(sdevx));
addList(cell, stuffFloat(sdevy));
addList(cell, stuffFloat(t));
addList(cell, stuffInteger(df));
addList(cell, stuffFloat(tprob));

return(cell);
}


CELL * p_corr(CELL * params)
{
UINT idx, Nx, Ny, df;
double * X;
double * Y;
double sumxy, sumx, sumy, meanx, meany, sd2x, sd2y;
double cov, corr, b0, b1, t, tprob;
CELL * cell;


if((X = getVector(evaluateExpression(params), &Nx, &sumx, &meanx, &sd2x)) == NULL)
    return(errorProc(ERR_LIST_OR_ARRAY_EXPECTED));
if((Y = getVector(evaluateExpression(params->next), &Ny, &sumy, &meany, &sd2y)) == NULL)
    return(errorProc(ERR_LIST_OR_ARRAY_EXPECTED));

if(Nx != Ny)
    return(errorProc(ERR_WRONG_DIMENSIONS));

sumxy = 0.0;
for(idx = 0; idx < Nx; idx++)
    sumxy += X[idx] * Y[idx];

freeMemory(X);
freeMemory(Y);

cov = sumxy - sumx * sumy / Nx;
corr = cov / sqrt(sd2x * sd2y);
b1 = cov / sd2x;
b0 = meany - b1 * meanx;
t = corr * sqrt((Nx - 2) / (1.0 - corr * corr));

/* standard error of corr */
/* rse = sqrt( (1.0 - corr * corr) / (Nx - 2) ); */
/* see http://irthoughts.wordpress.com/2010/06/10/on-spearmans-correlation-coefficients-with-excel/ */
/* rse = corr / t; */

df = Nx - 2;
tprob = 2 * (1.0 - probT(fabs(t), df)); /* two tailed */

cell = getCell(CELL_EXPRESSION);
addList(cell, stuffFloat(corr));
addList(cell, stuffFloat(b0)); /* y = b0 + b1*x */
addList(cell, stuffFloat(b1));
addList(cell, stuffFloat(t));
addList(cell, stuffInteger(df));
addList(cell, stuffFloat(tprob));

return(cell);
}

/* ============================= Big Integer support ============================= */

/* other big int libs:
    GNU GMP https://gmplib.org/
    MPIR http://www.mpir.org/ 
    BSDNT https://github.com/wbhart/bsdnt 
*/

#ifdef BIGINT

/* translate either INT64 or double float to big int */

CELL * p_bigInt(CELL * params)
{
int * numPtr;
int len, size = 0;
CELL * cell;
char * ptr;

cell = evaluateExpression(params);
if(cell->type == CELL_BIGINT)
    return(copyCell(cell));

if(cell->type == CELL_STRING)
    {
    ptr = (char *)cell->contents;
    if(*ptr == '-' || *ptr == '+') ++size, ptr++;
    while(isdigit(*ptr++)) ++size;
    if(size == 0) return(nilCell);
    numPtr = strToBigint((char *)cell->contents, size, &len);
    }
else getBigintSizeDirect(cell, &numPtr, &len);

cell = getCell(CELL_BIGINT);
cell->aux = len + 1;
cell->contents = (UINT)numPtr;

return(cell);
}

/* does not do evaluate expression on cell
   returns NULL when called with cell->type CELL_BIGINT
   else returns numPtr which mast be freed or use by caller
*/
int * getBigintSizeDirect(CELL * cell, int * * numPtr, int * len)
{
int size = 0;
int * num = NULL;

#ifndef NEWLISP64
if(cell->type == CELL_INT64)
    num = intToBigint(*(INT64 *)&cell->aux, &size);
else if(cell->type == CELL_LONG)
    num = intToBigint((INT64)cell->contents, &size);
else if(cell->type == CELL_FLOAT)
    {
#ifdef WINDOWS
    if(isnan(*(double *)&cell->aux) || !_finite(*(double *)&cell->aux)) 
        num = intToBigint(0, &size);
    num = floatToBigint(*(double *)&cell->aux, &size);
#else
    if(isnan(*(double *)&cell->aux)) 
        num = intToBigint(0, &size);
    num = floatToBigint(*(double *)&cell->aux, &size);
#endif
    }
#else /* NEWLISP64 */
if(cell->type == CELL_LONG)
    num = intToBigint((INT64)cell->contents, &size);
else if(cell->type == CELL_FLOAT)
    {
    if(isnan(*(double *)&cell->contents))  
        num = intToBigint(0, &size);
    num = floatToBigint( *(double *)&cell->contents, &size);
    }
#endif
else if(cell->type == CELL_BIGINT)
    {
    *numPtr = (int *)cell->contents;
    *len = cell->aux - 1;
    return(NULL);
    }
else 
    errorProcArgs(ERR_NUMBER_EXPECTED, cell);

*numPtr = num;
*len = size;

return(num);
}


/*  memory for num int array is allocated inside
    return is ptr to the int array and length in *intlen
*/
int * strToBigint(char * str, int len, int * intlen)
{
int n, i, j, cut;
int sign = 1;
char * ptr = str;
int * num;

/* don't look at trailing L */
if(*(str + len - 1) == 'L')
    len--;

/* skip leading sign */
if(*ptr == '-' || *ptr == '+')
    {
    sign = (*ptr == '-') ? -1 : 1;
    ptr++;
    len--;
    }

/* remove leading zeros */
while(len > 1 && *ptr == '0')
    {
    ptr++;
    len--;
    }

n = len / 9 + 1;
cut = len % 9;
if(cut) n++;
else cut = 9;

num = calloc(sizeof(int), n);
num[0] = sign;

for(i = 1; i < n; i++)
    {
    for(j = 0; j < cut; j++)
	num[i] = 10 * num[i] + ptr[j] - 48;
    ptr += cut;
    cut = 9;
    }

*intlen = n - 1;
return(num);
}


/* when offset is specified (48) '+' is suppressed only '-' is put
   when offset = 0, first digit is either -1 or 1  
   call with offset either 0 or 48
   do  ot call with 0 for num
*/
char * bigintToDigits(int * num, int n, int offset, int * slen)
{
int i, j, k = 0, cnt = 0, len = 0;
int q, basediv;
char * digits;
int bigdigit;

if(n == 1 && num[1] == 0)
    {
    digits = calloc(3, 1);
    if(offset == 0)
        {
        digits[0] = '1';
        digits[1] = 0;
        }
    else
        digits[0] = '0';
    return(digits); 
    }

/* cnt of digits in first big digit */
bigdigit = num[1];
basediv = BIGINT_BASE / 10;
while(bigdigit / basediv == 0)
    {
    basediv /= 10;
    cnt++;
    }

cnt = 9 - cnt;
len = 9 * (n - 1) + cnt;

digits = calloc(len + 2, 1);
if(offset != 0)
    {
    if(num[0] == -1) digits[k++] = '-';
    }
else
    digits[k++] = num[0];

for(i = 1; i <=n; i++)
    {
    bigdigit = num[i];
    for(j = 0; j < cnt; j++)
        {
        q = bigdigit / basediv;
        digits[k++] = q + offset;
        bigdigit -= q * basediv;
        basediv /= 10; 
        }
    basediv = BIGINT_BASE / 10;
    cnt = 9;
    }

if(slen) *slen = len;
return(digits);
} 

/* digits come in high to low with leading sign digit (1 or -1)
   nx and ny are the length without sign digit
*/

int * addBigint(int * x, int nx, int * y, int ny, int * sm, int * nsm)
{
int i, carry = 0, digit, n, d, sign;
int * ptr;

if(nx < ny)
    {
    n = nx; nx = ny; ny = n;
    ptr = x; x = y; y = ptr;
    }
d = nx - ny;

if(*x != *y) /* signs are different */
    {
    sign = x[0]; 
    y[0] = sign;
    sm = subBigint(x, nx, y, ny, sm, nsm);
    x[0] = sign; y[0] = sign * -1;
    return(sm);
    }
    
ptr = sm + 1;

/* memset(sm, 0, (nx + 2) * sizeof(int)); */

for(i = ny; i > 0; i--) /* ny is the smaller */
    {
    digit = x[i + d] + y[i] + carry;
    carry = digit / BIGINT_BASE;
    ptr[i + d] = digit % BIGINT_BASE;
    }

for(i = nx - ny; i > 0; i--) /* nx is the bigger */
    {
    digit = x[i] + carry;
    carry = digit / BIGINT_BASE;
    ptr[i] = digit % BIGINT_BASE;
    }
ptr[0] = carry;

if(*ptr == 0)
    {
    for(i = 0; i < nx; i++)
        ptr[i] = ptr[i + 1];
    *nsm = nx;
    }
else
    *nsm = nx + 1;

sm[0] = x[0];

return(sm);
}

    
int * subBigint(int * x, int nx, int * y, int ny, int * sm, int * nsm)
{
INT64 digit;
int * ptr;
int sign, i, j, n, carry = 0;

if(*x != *y)
    {
    sign = x[0];
    y[0] = sign;
    addBigint(x, nx, y, ny, sm, nsm);
    x[0] = sign; y[0] = sign * -1;
    return(sm); 
    }

/* compare and subtract the abs(smaller) from the abs(greater) */
if(cmpAbsBigint(x, nx, y, ny) >= 1)
    sm[0] = x[0];
else
    {
    sm[0] = x[0] * -1;
    n = nx; nx = ny; ny = n;
    ptr = x; x = y; y = ptr; 
    }

for(i = nx, j = ny; i > 0; i--, j--)
    {
    if(j > 0)
        digit = (INT64)x[i] - y[j] - carry;
    else
        digit = (INT64)x[i] - carry;
    if(digit < 0)
        {
        digit = digit + BIGINT_BASE;
        carry = 1;
        }
    else
        carry = 0;

    sm[i] = digit % BIGINT_BASE;
    }

i = 1; n = 0;

/* remove leading zero's */
while (sm[i] == 0 && i <= nx) n++, i++;
if(n == nx)
    {
    *nsm = 1;
    return(sm);
    }

memmove((char *)sm + 1 * sizeof(int), (char *)sm + (n + 1) * sizeof(int), (nx - n) * sizeof(int));
*nsm = (nx - n);

return(sm);
}


int * mulBigint(int * x, int nx, int * y,  int ny, int * p, int * n)
{
int i, j, k, carry;
INT64 digit;

memset(p, 0, (nx + ny + 1) * sizeof(int));
for(i = ny; i > 0 ; i--) /* for each digit in multiplier */
    {
    carry = 0;
    for(j = nx; j > 0; j--)
        {
        digit = (INT64)p[j + i] + (INT64)y[i] * x[j] + carry;
        carry = digit / BIGINT_BASE;
        p[j + i] = digit % BIGINT_BASE;
        }
    digit = (INT64)p[i] + carry;
    p[i] = digit % BIGINT_BASE;
    }

/* remove leading zero's */
k = nx + ny;
while(p[1] == 0 && k > 1)
    {
    for(i = 1; i < k; i++)
        p[i] = p[i + 1];
    k--;
    }

/* set sign  and n */
*p = *x * *y;
*n = k;
return(p);
}


/* q = x/y - caller must _not_ allocate space for q
*/
int * divModBigint(int * x, int nx, int * y, int ny, int rmndr, int * nq)
{
int * r;
int * q;
int * p;
int * ptr;
INT64 digit;
int i, j, k, carry;
int nr, sq, n, pn;
/* turn of certain optimizations using 'volatile' necessary 
   on Linux but will no affect performance on other platforms
*/
volatile double rFloat, yFloat;

if(y[1] == 0 && ny == 1)
    errorProc(ERR_MATH);

if(cmpAbsBigint(x, nx, y, ny) < 0)
    {
    if(rmndr)
        {
        q = malloc((nx + 1) * sizeof(int));
        memcpy((void *)q, (void *)x, (nx + 1) * sizeof(int));
        *nq = nx;
        }
    else
        {
        q = malloc(sizeof(int) * 2);
        q[0] = 1; q[1] = 0;
        *nq = 1;
        }
    return(q);
    }

r = alloca((nx + 1) * sizeof(int));
q = alloca((nx - ny + 1) * sizeof(int));
p = alloca((nx + 1) * sizeof(int));

nr = 0; sq = 0;
r[0] = q[0] = p[0] = 1; 
yFloat = bigintToAbsFloat(y, (ny > 2) ? 2 : ny);
#ifdef DEBUG
printf("yFloat %f\n", yFloat); 
#endif
for(i = 1; i <= nx; i++)    
    {
#ifdef DEBUG
    printf("i = %d\n", i);
#endif
    r[++nr] = x[i];
#ifdef DEBUG
    debug(r, nr, "r after <- x[i]");
#endif
    if(sq == 0 && cmpAbsBigint(r, nr, y, ny) < 0)
        continue;

    n = (ny > 2) ? (nr + 2 - ny) : nr; 
    rFloat = bigintToAbsFloat(r, n);
#ifdef DEBUG
    printf("rFloat %f\n", rFloat); 
#endif

    if(yFloat > rFloat)
        {
#ifdef DEBUG
        printf("yFloat > rFloat\n");
#endif
        q[++sq] = 0;
        goto TRIM_ZEROS;
        }

    if(yFloat == rFloat)
        {
#ifdef DEBUG
        printf("yFloat == rFloat\n");
#endif
        q[++sq] = 1;
        }
    else 
        q[++sq] = rFloat / yFloat + 1.0; /* like ceil() */
#ifdef DEBUG
    debug(q, sq, "q after float div");
#endif

    PRODUCT: 
    carry = 0;
    for(j = ny, pn = ny; j > 0; j--)
        {
        digit = (INT64)y[j] * q[sq] + carry;
        if(digit >= BIGINT_BASE)
            {
            p[j] = digit % BIGINT_BASE;
            carry = digit/BIGINT_BASE;
            }
        else
            p[j] = digit, carry = 0;
        }
    if(carry) /* shift right */
        {
        for(j = ny; j > 0; j--) p[j+1] = p[j];
        p[1] = carry, ++pn;
        }
#ifdef DEBUG
    debug(p, pn, "p product");
#endif
    if(cmpAbsBigint(r, nr, p, pn) < 0) 
        {
        q[sq] = q[sq] - 1; /* q[sq] too big */
#ifdef DEBUG
        printf("decrementing q[sq]\n");
#endif
        goto PRODUCT;
        }

    /* ----- inlined from subBigint ----- */
    for(k = nr, j = pn, carry = 0; k > 0; k--, j--)
        {
        if(j > 0)
            digit = (INT64)r[k] - p[j] - carry;
        else
            digit = (INT64)r[k] - carry;
        if(digit < 0)
            {
            digit += BIGINT_BASE;
            carry = 1;
            }
        else
            carry = 0;

        r[k] = digit % BIGINT_BASE;
        }
    /* trim leading zero's */
TRIM_ZEROS:
    if(r[1] == 0) {
        k = 1; n = 0;
        while (r[k] == 0 && k <= nr) n++, k++;
        if(n == nr) nr = 1;
        else
            {
            nr = (nr - n);
            memmove((char *)r + 1 * sizeof(int), 
                (char *)r + (n + 1) * sizeof(int), nr * sizeof(int));
            }
        }
#ifdef DEBUG
    debug(r, nr, "r after subtraction and trimming");
#endif
    /* ---------------------------------- */
    if(r[1] == 0 && i < nx) nr = 0;
    }

if(rmndr)
    {
    q = malloc((nr + 1) * sizeof(int));
    memcpy((void *)q, (void *) r, (nr + 1) * sizeof(int));
    q[0] = x[0]; /* set sign */
    *nq = nr;
    }
else
    {
    ptr = malloc((sq + 1) * sizeof(int));
    memcpy((void *)ptr, (void *)q, (sq + 1) * sizeof(int));
    q = ptr;
    q[0] = *x * *y;
    *nq = sq;
    }     
return(q);
}


int cmpAbsBigint(int * x, int nx, int * y, int ny)
{
int cmp, i;

cmp = nx - ny;
if(cmp == 0)
    {
    for(i = 1; i <= nx; i++)
        {
        if(x[i] > y[i]) return(1);
        if(x[i] < y[i]) return(-1);
        }
    return(0);
    }

return (cmp > 0) ? 1 : -1;
}

int cmpBigint(int * x, int nx, int * y, int ny)
{
int cmp;

/* make sure 0 == -0 */
if(nx == 1 && ny == 1 && x[1] == 0 && y[1] == 0)
    return(0);

if(*x > *y) return(1);
if(*x < *y) return(-1);

/* signs are both 1,1 or -1, -1 */
cmp = cmpAbsBigint(x, nx, y, ny);

return(cmp * *x);
}

/* biggest  int64  9 223372036 854775807
   smallest int64 -9 223372036 854775808
*/

INT64 bigintToInt64(CELL * cell)
{
INT64 num;
int * numPtr;
int  i;

int maxInt[4] = {1, 9, 223372036, 854775807};
int minInt[4] = {-1, 9, 223372036, 854775808};

numPtr = (int *)(UINT)cell->contents;
if(cmpBigint((int *)(UINT)cell->contents, cell->aux -1, maxInt, 3) == 1 ||
            cmpBigint((int *)(UINT)cell->contents, cell->aux - 1, minInt, 3) == -1)
    errorProc(ERR_NUMBER_OUT_OF_RANGE);

num = numPtr[1];
for(i = 2; i < cell->aux; i++)
    num = num * BIGINT_BASE + numPtr[i];

num = num * numPtr[0];

return(num);
}

int * intToBigint(INT64 num, int * len)
{
int idx = 1;
int * digit;

digit = malloc(4 * sizeof(int));

if(num < 0)
    {
    digit[0] = -1;
    num = -num;
    }
else
    digit[0] = 1;

if(num > BIGINT_BASE2)
    {
    digit[idx] = (int)(num / BIGINT_BASE2);
    num = num - digit[idx] * BIGINT_BASE2;
    idx++;
    }

if(num > BIGINT_BASE)
    {
    digit[idx] =(int)(num / BIGINT_BASE);
    num = num - digit[idx] * BIGINT_BASE;
    idx++;
    }

digit[idx] = num;

*len = idx;

return(digit);
}


/* only used internally by divModBigint() 
   does not check for overflow */
double bigintToAbsFloat(int * numPtr, int n)
{
double value;
int i;

value = numPtr[1];
for(i = 2; i <= n; i++)
    value = value * BIGINT_BASE + numPtr[i];

return(value);
}


double bigintCellToFloat(CELL * cell)
{
double value;
int * numPtr;
int i;

numPtr = (int *)(UINT)cell->contents;
value = numPtr[1];
for(i = 2; i < cell->aux; i++)
    {
    value = value * BIGINT_BASE + numPtr[i];
    if( value > 1.797693134862301e308)
        return(1/0.0);
    }

value *= numPtr[0];

return(value);
}

int * floatToBigint(double fnum, int * len)
{
int decDigits, bigDigits;
int inum, i, sign;
int * numPtr;

#ifdef SOLARIS
        if(isnan(fnum - fnum))
#else
        if(isinf(fnum))
#endif
    errorProcExt2(ERR_CANNOT_CONVERT, stuffFloat(fnum));
        
sign = fnum < 0 ? -1 : 1;
fnum = fnum * sign;

/* account for rounding errors */
if(fnum > 1.797693134862301e308)
    fnum = 1.797693134862301e308;
    
decDigits = log(fnum) / log(10) + 1;
if(decDigits <= 0) 
    {
    numPtr = calloc(2, sizeof(int));
    numPtr[0] = 1;
    *len = 1;
    return(numPtr);
    }

bigDigits = ((decDigits - 1) / 9) + 1;
*len = bigDigits;
numPtr = calloc(bigDigits + 3, sizeof(int)); 
numPtr[0] = sign;
for(i = 1; i <= 3; i++)
    {
    inum = fnum / pow(1000000000, (bigDigits - i));
    numPtr[i] = inum;
    fnum = fnum - (INT64)inum * pow(1000000000, (bigDigits - i));
    }

return(numPtr);
}

int lengthBigint(int * num, int len)
{
int cnt = 0;
int bigdigit, basediv;

if((bigdigit = num[1]) == 0) return(1);

basediv = BIGINT_BASE / 10;
while(bigdigit / basediv == 0)
    basediv /= 10, cnt++;

return(9 * (len - 1) + 9 - cnt);
}
#endif
/* eof */
