/* nl-math.c

    Copyright (C) 2011 Lutz Mueller

    This program is free software: you can redistribute it and/or modify
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
#define OP_ABS 29
#define OP_CEIL 30
#define OP_FLOOR 31
#define OP_NAN 32
#define OP_ERRORFUNC 33
#define OP_SIGNUM 34
#define OP_ISNAN 35
#define OP_ISINF 36


#ifdef WIN_32
int _matherr(struct _exception *e) {return 1;}
#endif

CELL * incDecI(CELL * params, int type)
{
CELL * cell;
INT64 adjust = 1;
INT64 lValue = 0;

cell = evaluateExpression(params);

if(symbolCheck != NULL)
	if(isProtected(symbolCheck->flags))
		return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck)));

if(!isNil(cell)) getInteger64(cell, &lValue);

if(params->next != nilCell)
	getInteger64(params->next, &adjust);

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
	if(isProtected(symbolCheck->flags))
		return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolCheck))); 

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

if(params == nilCell)
	{
	if(op == OP_ADD)
		return(stuffInteger64(0));
	if(op == OP_MULTIPLY)
		return(stuffInteger64(1));
	}

params = getInteger64(params, &result);	

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
	params = getInteger64(params, &number);
	switch(op)
		{
		case OP_ADD:            result += number; break;
		case OP_SUBTRACT:       result -= number; break;
		case OP_MULTIPLY:       result *= number; break;
		case OP_DIVIDE:         
			if(number == 0)	return(errorProc(ERR_MATH));
			result /= number; break;
		case OP_BIT_OR:         result |= number; break;
		case OP_BIT_AND:        result &= number; break;
		case OP_BIT_XOR:        result ^= number; break;
		case OP_SHIFTL:         result <<= number; break;
		case OP_SHIFTR:         result >>= number; break;
		case OP_MODULO:
			if(number == 0)	return(errorProc(ERR_MATH));
			result %= number; break;
		default:
			break;
		}
	}

#ifndef NEWLISP64
return(stuffInteger64(result));
#else
return(stuffInteger(result));
#endif
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
getInteger64(params, &number);
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

int compareInts(CELL * left, CELL * right)
{
INT64 leftnum;
INT64 rightnum;

#ifndef NEWLISP64
if(left->type == CELL_LONG)
	leftnum = (int)left->contents;
else
	leftnum = *(INT64 *)&left->aux;

if(right->type == CELL_LONG)
	rightnum = (int)right->contents;
else
	rightnum = *(INT64 *)&right->aux;
#else
leftnum = (UINT)left->contents;
rightnum = (UINT)right->contents;
#endif

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
	floatNum = (long)param->contents;
else if(param->type == CELL_INT64)
	floatNum = *(INT64 *)&param->aux;
#else
if(param->type == CELL_FLOAT)
	return(*(double *)&param->contents);
else if(param->type == CELL_LONG)
	floatNum = (long)param->contents;
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
CELL * p_abs(CELL * params) { return(functionFloat(params, OP_ABS)); }
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
  case OP_ABS: floatN = (floatN < 0.0) ? -floatN : floatN; break;
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
long digits = 0;
char * fmt;
char * result;


params = getFloat(params, &fNum);
if(params != nilCell)
	getInteger(params, (UINT*)&digits);

if(digits > 0)
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

return(stuffFloat(&fNum)); 
}


CELL * p_rand(CELL * params)
{
long range;  
long n;
CELL * dist, * cell;
long rnum;
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
long len;
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

getInteger64(params, &seedNum);

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
long cnt = 0;
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
	case CELL_MACRO:
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
	case CELL_LONG:
	default:
		if((long)left->contents > (long)right->contents) return(1);
		if((long)left->contents < (long)right->contents) return(-1);
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
double getFloatFromCell(CELL *);
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
		data[i] = getFloatFromCell(list);
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
	data[i] = getFloatFromCell(cell);
	data[i+1] = getFloatFromCell(cell->next);
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
	list->contents = (UINT)stuffFloat(&num1);
	cell = (CELL *)list->contents;
	cell->next = stuffFloat(&num2);
	return(list);
	}


double getFloatFromCell(CELL * cell)
	{
	double number;
#ifndef NEWLISP64
	if(cell->type == CELL_FLOAT)
		memcpy((void *)&number, (void *)&cell->aux, sizeof(double));
	else if(cell->type == CELL_LONG)
		number = (int)cell->contents;
	else number = (double)*(INT64 *)&cell->aux;
#else
	if(cell->type == CELL_FLOAT)
		number = *(double *)&cell->contents;
	else
		number = (long)cell->contents;
#endif
	return(number);
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
	return(scale * (randnum - 6144)/1024 + offset);
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
	return(stuffFloat(&randnum));
	}

dist = getCell(CELL_EXPRESSION);
randnum = getRandom(offset, scale, type);
dist->contents = (UINT)stuffFloat(&randnum);
cell = (CELL*)dist->contents;
for(i = 1; i < n; i++)
	{
	randnum = getRandom(offset, scale, type);
	cell->next = stuffFloat(&randnum);
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
size_t length, i, j;
CELL * * vector;
int repetition = 0;
long rnum;
double scale;

getListHead(params, &list);

if((length = listlen(list)) <= 1) 
  {
  cell = getCell(CELL_EXPRESSION);
  cell ->contents = (UINT)copyList(list);
  return(cell);
  }
  
repetition = getFlag(params->next);
 
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

/* check that new sequence is different */
if(!repetition)
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
free(vector);

return(list);
}

/*
   probZ  - probability of normal z value
   critChi2 - Compute critical chi-square value for p and df
   critZ - Compute critical Z-value from p
*/

double probChi2(double chi2, int df);
double critChi2(double p, int df);
double probZ(double z);
double critZ(double p);
double gammaln(double xx);
double betai(double a, double b, double x);
double gammap(double a, double x);
double betacf(double a, double b, double x);
static double gser(double a, double x, double gln);
double gcf(double a, double x, double gln);

CELL * p_probabilityZ(CELL * params)
{
double z;
double p;

getFloat(params, &z);

p = probZ(z);

return(stuffFloat((double *)&p));
}


double probChi2(double chi2, int df)
{
return(1.0 - gammap(df/2.0, chi2/2.0));
}

CELL * p_probabilityChi2(CELL * params)
{
double chi2;
double df;
double q;

params = getFloat(params, &chi2);
getFloat(params, &df);

q = probChi2(chi2, df);

return(stuffFloat((double *)&q));
}


CELL * p_criticalChi2(CELL  * params)
{
double p;
long df;
double chi;

params = getFloat(params, &p);
getInteger(params, (UINT *)&df);

chi = critChi2((1.0 - p), df);

return(stuffFloat((double *)&chi));
}


CELL * p_criticalZ(CELL * params)
{
double p;
double Z;

getFloat(params, &p);
Z = critZ(p);

return(stuffFloat((double *)&Z));
}


#define Z_MAX 6.0  /* Maximum meaningful z value */
#define SQRT2 1.414213562373095

double probZ(double z) 
{
return(0.5 + erf(z/SQRT2) / 2.0);
}

double critChi2(double p, int df)
{
#define CHI_EPSILON 0.000001   /* Accuracy of critchi approximation */
#define CHI_MAX 99999.0        /* Maximum chi-square value */
double minchisq = 0.0;
double maxchisq = CHI_MAX;
double chisqval;
        
if (p <= 0.0) return maxchisq;
else if (p >= 1.0) return 0.0;
        
chisqval = df / sqrt(p);    /* fair first value */
while ((maxchisq - minchisq) > CHI_EPSILON)
	{
	if (gammap(df/2.0, chisqval/2.0) < p) minchisq = chisqval;
	else maxchisq = chisqval;
	chisqval = (maxchisq + minchisq) * 0.5;
	}
return chisqval;
}


double critZ(double p)
{
#define Z_EPSILON 0.000001   /* Accuracy of critchi approximation */
double minZ = 0.0;
double maxZ = Z_MAX;
double Zval;
        
if (p <= 0.0) return maxZ;
else if (p >= 1.0) return 0.0;
        
Zval = 2.0;    /* fair first value */
while ((maxZ - minZ) > Z_EPSILON)
	{
	if (probZ(Zval) < p) minZ = Zval;
	else maxZ = Zval;
	Zval = (maxZ + minZ) * 0.5;
	}
return Zval;
}


/* ----------------------- betai and gammaln fucntions ----------------------*/


static int paramError;

CELL * p_beta(CELL * params)
{
double a, b, beta;

params = getFloat(params, &a);
getFloat(params, &b);

beta = exp(gammaln(a) + gammaln(b) - gammaln(a+b));

return(stuffFloat(&beta));
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
	
return(stuffFloat(&result));
}


CELL * p_gammaln(CELL * params)
{
double x, result;

getFloat(params, &x);

paramError = 0;

result = gammaln(x);

if(paramError)
	return(nilCell);
	
return(stuffFloat(&result));
}


CELL * p_gammai(CELL * params)
{
double a, x, result;

params = getFloat(params, &a);
getFloat(params, &x);

result = gammap(a, x);

#ifdef DEBUG /* try (gammai 10 10), see also gammap() */
printf("in p_gammai() result = %f\n", result);
#endif

return(stuffFloat(&result));
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
long n, k;
double bico, p, binomial;

params = getInteger(params, (UINT *)&n);
params = getInteger(params, (UINT *)&k);
getFloat(params, &p);

bico = exp(gammaln(n + 1.0) - gammaln(k + 1.0) - gammaln(n - k + 1.0));

binomial = bico * pow(p, (double)k) * pow(1.0 - p, (double)(n - k));

return(stuffFloat(&binomial));
}

/* -------------------------------------------------------------------------------- */

CELL * p_series(CELL * params)
{
double fromFlt, factFlt;
ssize_t count, i;
CELL * sequence;
CELL * cell;
CELL * pCell;
CELL * expr;
CELL * cellIdx;
int errNo;
UINT * resultIdxSave;

cell = evaluateExpression(params);
pCell = evaluateExpression(params->next);
params = params->next;
getInteger(params->next, (UINT *)&count);

sequence = getCell(CELL_EXPRESSION);
if(count <= 0) return(sequence);

if(isNumber(pCell->type))
	{
	if(!isNumber(cell->type))
		return(errorProcExt(ERR_NUMBER_EXPECTED, cell));
	fromFlt = getFloatFromCell(cell);
	factFlt = getFloatFromCell(pCell);
	cell = copyCell(cell);
	sequence->contents = (UINT)cell;
	for(i = 1; i < count; i++)
		{
		fromFlt *= factFlt;
		cell->next = stuffFloat(&fromFlt);
		cell = cell->next;
		}
	}
else /* assumes lambda or primitive */
	{
	cellIdx = initIteratorIndex();
	addList(sequence, copyCell(cell));
	resultIdxSave = resultStackIdx;
	for(i = 1; i < count; i++)
		{
		cell = copyCell(cell);
		cleanupResults(resultIdxSave);	
		expr = makeCell(CELL_EXPRESSION, (UINT)copyCell(pCell));
   		((CELL *)expr->contents)->next = cell;
		pushResult(expr);
		if(!(cell = evaluateExpressionSafe(expr, &errNo)))
			{
			deleteList(sequence);
			longjmp(errorJump, errNo);
			}
		addList(sequence, copyCell(cell));
		if(cellIdx->type == CELL_LONG) cellIdx->contents += 1;
		}
	recoverIteratorIndex(cellIdx);
	}

return(sequence);
}

/* ------------------------------- prime numbers ---------------------------- */

/*
* adapted for newLISP from the following code:
*
* factor.c -- print prime factorization of a number
* Ray Gardner -- 1985 -- public domain
* Modified Feb. 1989 by Thad Smith > public domain
*
* This version takes numbers up to the limits of double precision.
*/


CELL * pushFactor (INT64 d, int k, INT64 * prevFact, CELL * factList)
{
if (! *prevFact)
	{
	factList->contents = (UINT)stuffInteger64(d);
	factList = (CELL*)factList->contents;
	k--;
	}

while(k--)
	{
	factList->next = stuffInteger64(d);
	factList = factList->next;
	}

(*prevFact)++;

return(factList);
}


CELL * p_factor (CELL * params)
{
INT64 d, n;
long k;
INT64 prevFact = 0;
CELL * factList;
CELL * next;

getInteger64(params, &n);

d = n + 1;     /* test for roundoff error */

if ( (n + 3 != d + 2)  || (n < 2) )
      return(nilCell);
      
next = factList = getCell(CELL_EXPRESSION);

if ( n > 2 )
      {
      d = 2;
      for ( k = 0; (n % d) == 0; k++ )
              n /= d;
      if ( k )  next = pushFactor(d, k, &prevFact, next);
      
      for ( d = 3; d * d <= n; d += 2 )
            {
            for ( k = 0; (n % d) == 0; k++ )
            	n /= d;
	    	if ( k ) next = pushFactor(d, k, &prevFact, next);
            }
      }

if ( n > 1 )
      {
      if ( ! prevFact ) 
      	factList->contents = (UINT)stuffInteger64(n);
      else  next = pushFactor(n, 1, &prevFact, next);
      }
      
return(factList);
}

CELL * p_gcd(CELL * params)
{ 
INT64 m, n;
INT64 t, r;

params = getInteger64(params, &m);
if(m < 0) m = -m;
n = m;

while(params != nilCell)
	{
	params = getInteger64(params, &n);
	if(n < 0) n = -n;

	ITERATE_GCD:
	if (m < n) { t = m; m = n; n = t; }
	if(n == 0) {n = m; continue; } /* for (gcd x 0) => x */
	r = m % n;
	if (r == 0) { m = n; continue; }
	m = n; n = r;
	goto ITERATE_GCD;
	}

return(stuffInteger64(n));
} 


/* ------------------------------- financial functions ---------------------- */


CELL * p_pmt(CELL * params)
{
long nper;
double rate, pv;
double fv = 0.0;
double pmt = 0.0;
double inc;
long type = 0;

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
  
return stuffFloat(&pmt);
}


CELL * p_pv(CELL * params)
{
long nper;
double rate, pmt, pv;
double fv = 0.0;
double inc;
long type = 0;

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
  
return stuffFloat(&pv);
}


CELL * p_fv(CELL * params)
{
double rate, pmt, pv;
long nper, type;
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
  
return stuffFloat(&fv);
}


CELL * p_nper(CELL * params)
{
double rate, pmt, pv;
double fv = 0.0;
double R, c, nper;
long type = 0;

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


return stuffFloat(&nper);
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
	if(isNumber(list->type))
		fNum = getFloatFromCell(list);
    else 
        fNum = 0.0;

    cashFlow += fNum / pow((1.0 + rate), (double)++count);
    list = list->next;
    }

return stuffFloat(&cashFlow);
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
long number;

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
		amountsVec[i] = getFloatFromCell(amounts);
		amounts = amounts->next;
		}
	else 
		{
		free(amountsVec);
		free(timesVec);
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

free(amountsVec);
free(timesVec);

if(result == IRR_ERROR)
	return(nilCell);

return(stuffFloat(&result));
}

/* ----------------------------------- CRC32 ----------------------- */

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

params = getStringSize(params, &data, &len, TRUE);
return(stuffInteger(update_crc(0xffffffffL, (unsigned char *)data, (int)len) ^ 0xffffffffL));
}

/* Update a running CRC with the bytes buf[0..len-1]--the CRC
   should be initialized to all 1's, and the transmitted value
   is the 1's complement of the final running CRC (see the
   crc() routine below)). 
*/
   
unsigned int update_crc(unsigned int crc, unsigned char *buf, int len)
{
unsigned int crc_table[256];
unsigned int c;
int n, k;
 
/* make crc table */  
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
token = alloca(MAX_STRING + 1);

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
        if(list->aux > MAX_STRING) continue;
        *token = '_';
        memcpy(token + 1, (char *)list->contents, list->aux);
        break;
      case CELL_SYMBOL:
        strncpy(token, ((SYMBOL *)list->contents)->name, MAX_STRING);
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
/* double sumS = 1.0; */
long countNum, totalNum;

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
    /* initialize Fisher's Chi-2 probs */
    if(!chainBayes) Pchi2[idx] = Qchi2[idx] = 0.0;
    }

token = alloca(MAX_STRING + 1);
tkn = tokens;
while(tkn != nilCell) tkn = tkn->next, nTkn++;

/* for each token  calculate p(tkn|M) in each category M */
tkn = tokens;
for(i = 0; i < nTkn; i++)
    {
    switch(tkn->type)
      {
      case CELL_STRING:
        if(tkn->aux > MAX_STRING) continue;
        *token = '_';
        memcpy(token + 1, (char *)tkn->contents, tkn->aux);
        break;
      case CELL_SYMBOL:
        strncpy(token, ((SYMBOL *)tkn->contents)->name, MAX_STRING);
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
            
            priorP[idx] = 1.0 / maxIdx; /* will cancel out */
            
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
/*  sumS = 0.0; */
    for(idx = 0; idx < maxIdx; idx++)
      {
      postP[idx] = (probChi2(Qchi2[idx],  2 * nTkn) - probChi2(Pchi2[idx],  2 * nTkn) + 1.0) / 2.0;
/*    sumS += postP[idx]; */
      }
    }
                                                
    
for(idx = 0; idx < maxIdx; idx++)
    {
    /* normalize probs from Fisher's Chi-2 */
	/* taken out for 10.3, leads to misinterpretation of propbablities and NaNs
    if(!chainBayes) 
        postP[idx] /= sumS;
	*/

    if(idx == 0)
        {
        result = getCell(CELL_EXPRESSION);
        result->contents = (UINT)stuffFloat(postP + idx);
        cell = (CELL *)result->contents;
        }
    else
        {
        cell->next = stuffFloat(postP + idx);
        cell = cell->next;
        }
    }

return(result);
}

/*
//
// Copyright (C) 1992-2011 Lutz Mueller <lutz@nuevatec.com>
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

TERMSET * mgu = NULL;
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

free(set);
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
		substitute(right, left, ws); /* expand(right-expr, left-sym) in  ws set */

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
	free(set);
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

CELL * p_bits(CELL * params) 
{ 
int count = 0;
int i; 
char temp[65];
char result[65];
INT64 num;
CELL * resultList;

params = getInteger64(params, &num);

do 	{
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


/* eof */
