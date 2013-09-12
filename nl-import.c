/* nl-import.c --- shared library interface for newLISP

    Copyright (C) 2011 Lutz Mueller

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

#ifndef WIN_32
#ifdef MAC_102
#include "osx-dlfcn.h"
#else
#include <dlfcn.h>
#endif
#endif

UINT cdeclFunction(UINT fAddress, UINT * args, int count);

extern int evalCatchFlag;


#if defined(WIN_32) || defined(CYGWIN)
UINT stdcallFunction(UINT fAddress, UINT * args, int count);
#endif

#ifdef WIN_32

CELL * p_importLib(CELL * params)
{
char * libName;
char * funcName;
char * options = NULL;
HINSTANCE hLibrary;
CELL * pCell;
SYMBOL * symbol;
FARPROC initProc;

params = getString(params, &libName);
params = getString(params, &funcName);
if(params != nilCell)
	getString(params, &options);

/* hLibrary = NULL; */

if( (UINT)(hLibrary = LoadLibrary(libName)) < 32)
	return(errorProcExt2(ERR_IMPORT_LIB_NOT_FOUND, stuffString(libName)));

if(options != NULL && strcmp(options, "cdecl") ==  0)
	pCell = getCell(CELL_IMPORT_CDECL);
else
	pCell = getCell(CELL_IMPORT_DLL);

symbol = translateCreateSymbol(funcName, pCell->type, currentContext, TRUE);
if(isProtected(symbol->flags))
	return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol)));
	
deleteList((CELL *)symbol->contents);
symbol->contents = (UINT)pCell;
pCell->contents = (UINT)GetProcAddress(hLibrary, (LPCSTR)funcName);
pCell->aux = (UINT)symbol->name;

/* put name of imported DLL into DLLs space for loadStartup() */
initProc = GetProcAddress(hLibrary, (LPCSTR)"dllName");
if(initProc != 0) (*initProc)(libName);

if(pCell->contents == 0) 
	return(errorProcExt2(ERR_IMPORT_FUNC_NOT_FOUND, stuffString(funcName)));

return(copyCell(pCell));
}

#else  /* UNIX and compatible operating systems */

CELL * p_importLib(CELL * params)
{
char * libName;
char * funcName;
void * hLibrary;
CELL * pCell;
SYMBOL * symbol;
char * error;
#ifdef CYGWIN
int type = CELL_IMPORT_DLL;
char * options = NULL;
#else
int type = CELL_IMPORT_CDECL;
#endif


params = getString(params, &libName);
params = getString(params, &funcName);
#ifdef CYGWIN
if(params != nilCell)
	{
	getString(params, &options);
	if(strcmp(options, "stdcall") ==  0)
		type = CELL_IMPORT_CDECL;
	}
#endif

hLibrary = 0;                

#ifdef TRU64
if((hLibrary = dlopen(libName, RTLD_LAZY)) == 0)
#else
if((hLibrary = dlopen(libName, RTLD_GLOBAL|RTLD_LAZY)) == 0)
#endif
	return(errorProcExt2(ERR_IMPORT_LIB_NOT_FOUND, stuffString((char *)dlerror())));

pCell = getCell(type);
symbol = translateCreateSymbol(funcName, type, currentContext, TRUE);
if(isProtected(symbol->flags))
	return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol)));
	
deleteList((CELL *)symbol->contents);
symbol->contents = (UINT)pCell;

pCell->contents = (UINT)dlsym(hLibrary, funcName);

if((error = (char *)dlerror()) != NULL)
	return(errorProcExt2(ERR_IMPORT_FUNC_NOT_FOUND, stuffString(error)));

pCell->aux = (UINT)symbol->name;

return(copyCell(pCell));
}
#endif


CELL * executeLibfunction(CELL * pCell, CELL * params)
{
CELL * arg;
UINT args[14];
int count;

count = 0;
while(params->type != CELL_NIL && count < 14)
	{
	arg = evaluateExpression(params);
	switch(arg->type)
		{
		case CELL_LONG:
		case CELL_STRING:
		case CELL_PRIMITIVE:
			args[count++] = arg->contents;
			break;
#ifndef NEWLISP64
		/* change 64-bit to 32-bit */
		case CELL_INT64: 
			args[count++] = *(INT64 *)&arg->aux;
			break;
#endif
		case CELL_FLOAT:
#ifndef NEWLISP64
			args[count++] = arg->aux;
#endif
			args[count++] = arg->contents;
			break;
		default:
			args[count++] = (UINT)arg;
			break;
		}
	params = (CELL *)params->next;
	}

#if defined(WIN_32) || defined(CYGWIN)
if(pCell->type == CELL_IMPORT_DLL)
	return(stuffInteger(stdcallFunction(pCell->contents, args, count)));
else
#endif
return(stuffInteger(cdeclFunction(pCell->contents, args, count)));
}


UINT cdeclFunction(UINT fAddress, UINT * args, int count)
{
UINT (*function)();

function = (UINT (*)())fAddress;

switch(count)
	{
	case 0:
            return (*function)();

	case 1:
            return  (*function)(args[0]);

	case 2:
            return  (*function)(args[0], args[1]);

	case 3:
			/* printf("args[0] %llx, args[1] %llx, args[2] %llx, args[1]-args[2] %llx\n ",
					args[0], args[1], args[2], args[1] - args[2]); */

            return  (*function)(args[0], args[1], args[2]);
	case 4:

            return  (*function)(args[0], args[1], args[2], args[3]);

	case 5:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4]);
	case 6:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4], args[5]);
	case 7:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4], args[5], args[6]);
	case 8:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4], args[5], args[6], args[7]);

	case 9:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4], args[5], args[6], args[7], args[8]);

	case 10:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4], args[5], args[6], args[7], args[8], args[9]);
	case 11:
            return  (*function)(args[0], args[1], args[2], args[3],
				args[4], args[5], args[6], args[7],
				args[8], args[9], args[10]);
	case 12:
            return  (*function)(args[0], args[1], args[2], args[3],
				args[4], args[5], args[6], args[7],
				args[8], args[9], args[10], args[11]);

	case 13:
            return  (*function)(args[0], args[1], args[2], args[3],
				args[4], args[5], args[6], args[7],
				args[8], args[9], args[10], args[11],
				args[12]);
	case 14:
            return  (*function)(args[0], args[1], args[2], args[3],
				args[4], args[5], args[6], args[7],
				args[8], args[9], args[10], args[11],
				args[12], args[13]);
	default:
	    break;
	}

return(0);
}


#if defined(WIN_32) || defined(CYGWIN)
UINT stdcallFunction(UINT fAddress, UINT * args, int count)
{
UINT _stdcall (*function)();

function = (UINT _stdcall (*)())fAddress;

switch(count)
	{
	case 0:
            return (*function)();

	case 1:
            return  (*function)(args[0]);

	case 2:
            return  (*function)(args[0], args[1]);

	case 3:
            return  (*function)(args[0], args[1], args[2]);

	case 4:
            return  (*function)(args[0], args[1], args[2], args[3]);

	case 5:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4]);
	case 6:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4], args[5]);
	case 7:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4], args[5], args[6]);
	case 8:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4], args[5], args[6], args[7]);

	case 9:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4], args[5], args[6], args[7], args[8]);

	case 10:
            return  (*function)(args[0], args[1], args[2], args[3],
				     args[4], args[5], args[6], args[7], args[8], args[9]);
	case 11:
            return  (*function)(args[0], args[1], args[2], args[3],
				args[4], args[5], args[6], args[7],
				args[8], args[9], args[10]);
	case 12:
            return  (*function)(args[0], args[1], args[2], args[3],
				args[4], args[5], args[6], args[7],
				args[8], args[9], args[10], args[11]);

	case 13:
            return  (*function)(args[0], args[1], args[2], args[3],
				args[4], args[5], args[6], args[7],
				args[8], args[9], args[10], args[11],
				args[12]);
	case 14:
            return  (*function)(args[0], args[1], args[2], args[3],
				args[4], args[5], args[6], args[7],
				args[8], args[9], args[10], args[11],
				args[12], args[13]);
	default:
	    break;
	}

return(0);
}
#endif


/* used when passing 32bit floats to library routines */
CELL * p_flt(CELL * params)
{
double dfloatV;
float floatV;
unsigned int number;

getFloat(params, &dfloatV);

floatV = dfloatV;
memcpy(&number, &floatV, 4);

return(stuffInteger(number));
}


/* 16 callback functions for up to 8 parameters */

long template(long n, long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8);

long callback0(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(0, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback1(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(1, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback2(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(2, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback3(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(3, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback4(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(4, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback5(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(5, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback6(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(6, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback7(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(7, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback8(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(8, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback9(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(9, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback10(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(10, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback11(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(11, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback12(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(12, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback13(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(13, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback14(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(14, p1, p2, p3, p4, p5, p6, p7, p8);}
long callback15(long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
	{return template(15, p1, p2, p3, p4, p5, p6, p7, p8);}

typedef struct {
	SYMBOL * sym;
	UINT func;
	} LIBCALLBACK;

LIBCALLBACK callback[] = {
	{ NULL, (UINT)callback0 },
	{ NULL, (UINT)callback1 },
	{ NULL, (UINT)callback2 },
	{ NULL, (UINT)callback3 },
	{ NULL, (UINT)callback4 },
	{ NULL, (UINT)callback5 },
	{ NULL, (UINT)callback6 },
	{ NULL, (UINT)callback7 },
	{ NULL, (UINT)callback8 },
	{ NULL, (UINT)callback9 },
	{ NULL, (UINT)callback10 },
	{ NULL, (UINT)callback11 },
	{ NULL, (UINT)callback12 },
	{ NULL, (UINT)callback13 },
	{ NULL, (UINT)callback14 },
	{ NULL, (UINT)callback15 },
};


long template(long n, long p1, long p2, long p3, long p4, long p5, long p6, long p7, long p8) 
{
CELL * args;
CELL * cell;
long result;
jmp_buf errorJumpSave;

memcpy(errorJumpSave, errorJump, sizeof(errorJump));
if(setjmp(errorJump))
	{
	reset();
	initStacks();
	result = -1;
	goto FINISH_CALLBACK;
	}

args = stuffIntegerList(8, p1, p2, p3, p4, p5, p6, p7, p8);
executeSymbol(callback[n].sym, (CELL *)args->contents, &cell);
result = cell->contents;
deleteList(cell);
args->contents = (UINT)nilCell;
deleteList(args);

FINISH_CALLBACK:
memcpy(errorJump, errorJumpSave, sizeof(errorJump));
return(result);
}


CELL * p_callback(CELL * params)
{
SYMBOL * sPtr;
UINT n;

params = getInteger(params, &n);
if(n > 15) return(errorProc(ERR_NUMBER_OUT_OF_RANGE));

getSymbol(params, &sPtr);
callback[n].sym = sPtr;

return(stuffInteger(callback[n].func));
}

/* end of file */
