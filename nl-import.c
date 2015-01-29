/* nl-import.c --- shared library interface for newLISP

    Copyright (C) 2015 Lutz Mueller

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

#ifndef WINDOWS
#include <dlfcn.h>
#endif

#ifdef MAC_OSX
#include <sys/mman.h>
#endif

UINT cdeclFunction(UINT fAddress, UINT * args, int count);
extern int evalCatchFlag;

#ifdef FFI
CELL * ffiPreparation(CELL * pCell, CELL * params, int type);
CELL * ffiTypeToCell(ffi_type *type, void * src);
CELL * unpackFFI(ffi_type * ffi, char * data);
void ffi_trampoline(ffi_cif *cif, void *ret, void **args, void *symbol);
#endif


#if defined(WINDOWS) || defined(CYGWIN)
UINT stdcallFunction(UINT fAddress, UINT * args, int count);
#endif

#ifdef WINDOWS

CELL * p_importLib(CELL * params)
{
char * libName;
char * funcName;
char * options = NULL;
HINSTANCE hLibrary;
CELL * pCell;
SYMBOL * symbol;
FARPROC initProc;
int type = CELL_IMPORT_DLL;

params = getString(params, &libName);
params = getString(params, &funcName);
if(params != nilCell)
    {
    if(params->next == nilCell)
        params = getString(params, &options);
#ifdef FFI
    else
        type = CELL_IMPORT_FFI;
#endif
    }

if( (UINT)(hLibrary = LoadLibrary(libName)) < 32)
    return(errorProcExt2(ERR_IMPORT_LIB_NOT_FOUND, stuffString(libName)));

if(options != NULL && strcmp(options, "cdecl") ==  0)
	type = CELL_IMPORT_CDECL;

symbol = translateCreateSymbol(funcName, type, currentContext, TRUE);
if(isFFIsymbol(symbol->flags)) /* don't redefine return current def */
        return (copyCell((CELL *)symbol->contents));

if(isProtected(symbol->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol)));

pCell = getCell(type);

deleteList((CELL *)symbol->contents);
symbol->contents = (UINT)pCell;
if((pCell->contents = (UINT)GetProcAddress(hLibrary, (LPCSTR)funcName)) == 0)
    return(errorProcExt2(ERR_IMPORT_FUNC_NOT_FOUND, stuffString(funcName)));

/* put name of imported DLL into DLLs space for loadStartup() */
initProc = GetProcAddress(hLibrary, (LPCSTR)"dllName");
if(initProc != 0) (*initProc)(libName);

#ifdef FFI
symbol->flags |= SYMBOL_FFI | SYMBOL_PROTECTED;
if(pCell->type == CELL_IMPORT_FFI)
    {
    pCell->aux = (UINT)calloc(sizeof(FFIMPORT), 1);
    ((FFIMPORT *)pCell->aux)->name = symbol->name;
    return(copyCell(ffiPreparation(pCell, params, FFI_FUNCTION)));
    }
#endif

pCell->aux = (UINT)symbol->name;

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
char * options = NULL;
int type = CELL_IMPORT_DLL;
#else
int type = CELL_IMPORT_CDECL;
#endif


params = getString(params, &libName);
if(params != nilCell)
    params = getString(params, &funcName);
else funcName = NULL;

#ifdef CYGWIN
if(params != nilCell)
    { 
    if(params->next == nilCell)   
        {
        params = getString(params, &options);
        if(strcmp(options, "cdecl") ==  0)
            type = CELL_IMPORT_CDECL;
        }
#ifdef FFI
    else type = CELL_IMPORT_FFI;
#endif
    }
#else
if(params->next != nilCell)
    type = CELL_IMPORT_FFI;
#endif

hLibrary = 0;                

#ifdef TRU64
if((hLibrary = dlopen(libName, RTLD_LAZY)) == 0)
#else
if((hLibrary = dlopen(libName, RTLD_GLOBAL|RTLD_LAZY)) == 0)
#endif
    return(errorProcExt2(ERR_IMPORT_LIB_NOT_FOUND, stuffString((char *)dlerror())));

if(funcName == NULL)
    return(trueCell);

symbol = translateCreateSymbol(funcName, type, currentContext, TRUE);
if(isFFIsymbol(symbol->flags)) /* don't redefine */
        return (copyCell((CELL *)symbol->contents));

if(isProtected(symbol->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol)));
    
pCell = getCell(type);
deleteList((CELL *)symbol->contents);
symbol->contents = (UINT)pCell;

dlerror(); /* clear potential error */
pCell->contents = (UINT)dlsym(hLibrary, funcName);

if((error = (char *)dlerror()) != NULL)
    return(errorProcExt2(ERR_IMPORT_FUNC_NOT_FOUND, stuffString(error)));

#ifdef FFI
symbol->flags |= SYMBOL_FFI | SYMBOL_PROTECTED;
if(pCell->type == CELL_IMPORT_FFI)
    {
    pCell->aux = (UINT)calloc(sizeof(FFIMPORT), 1);
    ((FFIMPORT *)pCell->aux)->name = symbol->name;
    return(copyCell(ffiPreparation(pCell, params, FFI_FUNCTION)));
    }
#endif

pCell->aux = (UINT)symbol->name;
return(copyCell(pCell));
}
#endif


CELL * executeLibfunction(CELL * pCell, CELL * params)
{
CELL * arg;
UINT args[14];
int count;

#ifdef FFI
if(pCell->type == CELL_IMPORT_FFI)
    if(((FFIMPORT *)pCell->aux)->type != 0)
    return executeLibFFI(pCell, params);
#endif

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

#if defined(WINDOWS) || defined(CYGWIN)
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


#if defined(WINDOWS) || defined(CYGWIN)
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


/* 16 callback functions for up to 8 parameters */

INT template(INT n, INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8);

INT callback0(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(0, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback1(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(1, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback2(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(2, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback3(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(3, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback4(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(4, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback5(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(5, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback6(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(6, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback7(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(7, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback8(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(8, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback9(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(9, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback10(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(10, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback11(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(11, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback12(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(12, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback13(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(13, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback14(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(14, p1, p2, p3, p4, p5, p6, p7, p8);}
INT callback15(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
    {return template(15, p1, p2, p3, p4, p5, p6, p7, p8);}

typedef INT (*lib_callback_t)(INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8);

typedef struct {
    SYMBOL * sym;
    lib_callback_t func;
    } LIBCALLBACK;

LIBCALLBACK callback[] = {
    { NULL, callback0 },
    { NULL, callback1 },
    { NULL, callback2 },
    { NULL, callback3 },
    { NULL, callback4 },
    { NULL, callback5 },
    { NULL, callback6 },
    { NULL, callback7 },
    { NULL, callback8 },
    { NULL, callback9 },
    { NULL, callback10 },
    { NULL, callback11 },
    { NULL, callback12 },
    { NULL, callback13 },
    { NULL, callback14 },
    { NULL, callback15 },
};


INT template(INT n, INT p1, INT p2, INT p3, INT p4, INT p5, INT p6, INT p7, INT p8) 
{
CELL * args;
CELL * cell;
INT result;
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

#ifndef NEWLISP64
if(cell->type == CELL_INT64)
    result = *(INT64 *)&cell->aux;
else 
#endif
    result = (INT)cell->contents;

args->contents = (UINT)nilCell;

deleteList(args);
/* before 10.4.4 this was pushResult(cell) but caused resultStack overflow
   in 10.4.4 changed to deleteList(cell), but now return value on
   simple callback not available anymore, use callback with libffi instead
   available on -DFFI compiled versions. See also:
       qa-specific-tests/qa-simplecallback
*/
deleteList(cell); 

FINISH_CALLBACK:
memcpy(errorJump, errorJumpSave, sizeof(errorJump));
return(result);
}


CELL * p_callback(CELL * params)
{
CELL * cell;
SYMBOL * sPtr;
UINT n;


#ifdef FFI
SYMBOL * symbol;
CELL * ffiCell;
FFIMPORT *ffi;
char * cb_name;
int result, len;

#ifdef MAC_OSX
ffi_closure *closure;
#endif

#endif /* FFI */

cell = evaluateExpression(params);
if(cell->type == CELL_SYMBOL)
	sPtr = (SYMBOL *)cell->contents;
else
	goto CALLBACK_SIMPLE;

#ifdef FFI
len = strlen(sPtr->name);
cb_name = calloc(sizeof(char) * (len + 6), 1);
/*
strncpy(cb_name, "$ffi-", 6);
strncat(cb_name, sPtr->name, len);
*/
memcpy(cb_name, "$ffi-", 5);
memcpy(cb_name + 5, sPtr->name, len + 1);

symbol = translateCreateSymbol(cb_name, CELL_NIL, mainContext, TRUE);
if(isFFIsymbol(symbol->flags)) /* already defined */
    {
    ffiCell = (CELL *)symbol->contents;
    ffi = (FFIMPORT *) ffiCell->aux;
    return(stuffInteger((UINT) ffi->code));
    }

if(isProtected(symbol->flags))
    return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol)));

symbol->flags |= SYMBOL_FFI | SYMBOL_PROTECTED;

if(params->next == nilCell)
    return(errorProcExt(ERR_FFI_PREP_FAILED,nilCell));

ffiCell = getCell(CELL_IMPORT_FFI);
ffiCell->aux = (UINT)calloc(sizeof(FFIMPORT), 1);
((FFIMPORT *)ffiCell->aux)->name = cb_name;
ffiCell = ffiPreparation(ffiCell,params->next,FFI_CLOSURE);

if(ffiCell != nilCell)
    {
    ffi = (FFIMPORT *) ffiCell->aux;
    ffi->data = calloc(sizeof(ffi_closure_data), 1);
#ifndef MAC_OSX
    ffi->clos = ffi_closure_alloc(sizeof(ffi_closure),&ffi->code);
    if(!ffi->clos)
        {
        free(ffi->data);
        return(errorProcExt(ERR_FFI_PREP_FAILED,nilCell));
        }
    result = ffi_prep_closure_loc(ffi->clos, &ffi->cif, ffi_trampoline, ffi->data, ffi->code);
    if(result != FFI_OK)
        {
        free(ffi->data);
        return(errorProcExt(ERR_FFI_PREP_FAILED, stuffSymbol(sPtr)));
        }
#else /* MAC_OSX */
    if((closure = mmap(NULL, sizeof(ffi_closure), PROT_READ | PROT_WRITE,
             MAP_ANON | MAP_PRIVATE, -1, 0)) == (void*)-1)
        {
        free(ffi->data);
        return(errorProcExt(ERR_FFI_PREP_FAILED, stuffSymbol(sPtr)));
        }
    ffi->clos = closure;
    if((result = ffi_prep_closure(closure, &ffi->cif, ffi_trampoline, ffi->data)) != FFI_OK)
        {
        free(ffi->data);
        munmap(closure, sizeof(closure));
        return(errorProcExt(ERR_FFI_PREP_FAILED, stuffSymbol(sPtr)));
        }
    if(mprotect(closure, sizeof(closure), PROT_READ | PROT_EXEC) == -1)
        {
        free(ffi->data);
        munmap(closure, sizeof(closure));
        return(errorProcExt(ERR_FFI_PREP_FAILED, stuffSymbol(sPtr)));
        }
    /* ffi->code = ffi_trampoline; */
    ffi->code = closure;
#endif
    ffi->data->symbol = sPtr;
    ffi->data->code = ffi->code;

    deleteList((CELL *)symbol->contents);
    symbol->contents = (UINT)ffiCell;

    return(stuffInteger((UINT) ffi->code));
    }
else
    return(errorProcExt(ERR_FFI_PREP_FAILED, stuffString(sPtr->name)));
#endif /* FFI */

CALLBACK_SIMPLE:
getIntegerExt(cell, &n, FALSE);

if(n > 15) return(errorProc(ERR_NUMBER_OUT_OF_RANGE));

getSymbol(params->next, &sPtr);
callback[n].sym = sPtr;

return(stuffInteger((UINT)callback[n].func));
}

/* ========================= FFFI using ffilib ========================== */

/* because of the non-standard cell in FFI symbol->contents, it cannot be
   memory managed in an efficient way. This is why FFI functions and structs
   can only be defined once. A repeated definitions will return nil and leave
   the symbol's original definition untouched. 

   Thanks to Stefan Sonnenberg for doing most of the coding in this section.
*/


#ifdef FFI

typedef struct 
    {
    char * name;
    ffi_type * type;
    int size;
    } FFITYPE;

/* of the native C-types only types, which are the same 
   on LP64, LLP64 and ILP32, are supported
*/

/* custom ffi_type_charpointer see initFFI() */
ffi_type ffi_type_charpointer = {0, 0, 0, NULL};

FFITYPE ffi_types[] =
    {
    {"void",        &ffi_type_void},
    {"char",        &ffi_type_sint8},
    {"byte",        &ffi_type_uint8},
    {"unsigned short int",  &ffi_type_uint16},
    {"short int",   &ffi_type_sint16},
    {"unsigned int",&ffi_type_uint32},
    {"int",         &ffi_type_sint32},
#ifdef NEWLISP64
    {"long",        &ffi_type_sint64},
#else
    {"long",        &ffi_type_sint32},
#endif
    {"long long",   &ffi_type_sint64},
    {"float",       &ffi_type_float},
    {"double",      &ffi_type_double},
    {"char*",       &ffi_type_charpointer}, /* zero terminated string buffers with textual info */
    {"void*",       &ffi_type_pointer}, /* string or address with binary info, address return */
    {NULL,          NULL}
    };

void initFFI(void)
{
memcpy(&ffi_type_charpointer, &ffi_type_pointer, sizeof(ffi_type));
}

/* creates ffi usable struct information */
CELL * p_struct(CELL * params)
{
CELL * ffiCell;
SYMBOL * symbol;
FFIMPORT * ffi;
int i;

    params = getSymbol(params, &symbol);
    if(isFFIsymbol(symbol->flags)) /* don't redefine */
        return(stuffSymbol(symbol));

    if(isProtected(symbol->flags))
        return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol)));

    if(params == nilCell)
        return(errorProc(ERR_MISSING_ARGUMENT));

    symbol->flags |= SYMBOL_FFI | SYMBOL_PROTECTED;

    ffiCell = getCell(CELL_IMPORT_FFI);
    ffiCell->aux = (UINT)calloc(sizeof(FFIMPORT), 1);
    ffi = (FFIMPORT *) ffiCell->aux;
    ffi->name = symbol->name;
    ffiCell = ffiPreparation(ffiCell, params, FFI_STRUCT);
    ffi->cstruct = calloc(sizeof(ffi_type),1);
    
    ffi->cstruct->elements = calloc(sizeof(ffi_type *) * (ffi->cif.nargs + 1), 1);

    for(i = 0; i < ffi->cif.nargs; i++)
        ffi->cstruct->elements[i] = ffi->cif.arg_types[i];
	/* last elements[nargs] must be left 0, NULL */
	/* size and alignment are already 0, to let ffi know, it has to calculate */

    ffi->cstruct->type = FFI_TYPE_STRUCT; 

    if(ffi_prep_cif(&ffi->cif, FFI_DEFAULT_ABI, 0, ffi->cstruct, 0) != FFI_OK)
        return(errorProc(ERR_FFI_PREP_FAILED));

    deleteList((CELL *)symbol->contents);
    symbol->contents = (UINT)ffiCell;

    return(stuffSymbol(symbol));

}

CELL * packFFIstruct(CELL * cell, CELL * params)
{
char chrV;
unsigned char byteV;
short int shortV;
unsigned short int uint16V;  /* 16 bit */
unsigned int uint32V;         /* 32 bit */
int int32V;                 /* 32 bit */
unsigned long long  uint64V; /* 64 bit */
float floatV;
double doubleV;
ffi_type ** elements;
ffi_type *elem;
size_t offset = 0;
unsigned short pad;
char * data;
int listFlag = 0;
FFIMPORT * ffi;

ffi = (FFIMPORT *) cell->aux;
if(ffi->cstruct && ffi->cstruct->type != FFI_TYPE_STRUCT)
    return(errorProc(ERR_FFI_STRUCT_EXPECTED));

elements = (ffi_type **) ffi->cstruct->elements;

data = allocMemory(ffi->cstruct->size + 1);
memset(data, 0, ffi->cstruct->size + 1);

/* check if data come in as a list like in traditional pack */
    
/* computing offsets and copy data */
elements = (ffi_type **) ffi->cstruct->elements;
while( (elem = *elements++) != NULL)
    {
    if(params->type == CELL_NIL) break;
    if(listFlag)
        cell = params;
    else
        cell = evaluateExpression(params);
    /* accept data in a list */
    if(isList(cell->type))
        {
        params = cell = (CELL *)cell->contents;
        listFlag = 1;
        }

    /* aligned ? */
    pad = offset % elem->alignment;
    /* no, add size of alignment boundary - padding bytes */
    if (pad != 0) offset += elem->alignment - pad;

#ifndef NEWLISP64
    if(cell->type == CELL_FLOAT || cell->type == CELL_INT64)
        uint64V = *(INT64 *)&cell->aux;
    else /* CELL_LONG and CELL_STRING */
        uint64V = cell->contents;
#else
    uint64V = cell->contents;
#endif

    if( elem == &ffi_type_sint8 )
        {
        chrV = (char)uint64V;
        memcpy(data + offset, &chrV, sizeof(char));
        }
    else if( elem == &ffi_type_uint8)
        {
        byteV = (char)uint64V;
        memcpy(data + offset, &byteV, sizeof(unsigned char));
        }
    else if( elem == &ffi_type_uint16)
        {
        uint16V = (unsigned short int)uint64V;
        memcpy(data + offset, &uint16V, sizeof(unsigned short int));
        }
    else if( elem == &ffi_type_sint16)
        {
        shortV = (short int)uint64V;
        memcpy(data + offset, &shortV, sizeof(short int));
        }
    else if( elem == &ffi_type_uint32)
        {
        uint32V = (unsigned int)uint64V;
        memcpy(data + offset, &uint32V, sizeof(unsigned int));
        }
    else if(elem == &ffi_type_sint32)
        {
        int32V = (int)uint64V;
        memcpy(data + offset, &int32V, sizeof(int));
        }
    else if(elem == &ffi_type_sint64)
        memcpy(data + offset, &uint64V, sizeof(long long));
    else if(elem == &ffi_type_float)
        {
        floatV = (float) *(double *)&cell->aux;
        memcpy(data + offset, &floatV, sizeof(float));
        }
    else if(elem == &ffi_type_double)
        {
        doubleV = getDirectFloat(cell);
        memcpy(data+offset, &doubleV, sizeof(double));
        }
    else if(elem == &ffi_type_pointer || elem == &ffi_type_charpointer)
        memcpy(data + offset, (void *)&cell->contents, sizeof(void *));
    else /* just copy what's there, must be struct */
        memcpy(data + offset, (void *)cell->contents, elem->size);

    /* grow offset by size of last element */
    offset += elem->size;
    /* go to next param and repeat */
    params = params->next;
    }

cell = makeStringCell(data, ffi->cstruct->size);

return(cell);
}

CELL * unpackFFIstruct(CELL * cell, char * data)
{
FFIMPORT * ffi;

ffi = (FFIMPORT *) cell->aux;
if(ffi->cstruct && ffi->cstruct->type != FFI_TYPE_STRUCT)
    return(errorProc(ERR_FFI_STRUCT_EXPECTED));

/* This is redundant? works with or without it on OSX, Windows, Linux. 
if(ffi->cstruct->size != 0)
    if(ffi_prep_cif(&ffi->cif, FFI_DEFAULT_ABI, 0, ffi->cstruct,0) != FFI_OK)
        return(errorProc(ERR_FFI_PREP_FAILED));
*/

return(unpackFFI(ffi->cstruct, data));
}


CELL * unpackFFI(ffi_type * ffiPtr, char * data)
{
ffi_type * elem;
ffi_type ** elements;
CELL * cell;
size_t offset = 0;
unsigned short pad;

cell = getCell(CELL_EXPRESSION);
elements = ffiPtr->elements;
while( (elem = *elements++) != NULL )
    {
    pad = offset % elem->alignment;
    if (pad != 0) offset += elem->alignment - pad;
    addList(cell, (CELL *) ffiTypeToCell(elem, data + offset));
    offset += elem->size;
    }

return(cell);
}
 

ffi_type * getFFIType(char * type)
{
int i = 0;
char * name;
SYMBOL * sPtr;
CELL * sType;
FFIMPORT * tmp;

while((name = ffi_types[i].name))
    {
    if(strcmp(name, type) == 0)
        return(ffi_types[i].type);
    ++i;
    }

/* ok - no primitive type found: try to lookup struct definition */
sPtr = lookupSymbol(type,currentContext);
if(sPtr)
    {
    sType = (CELL *)sPtr->contents;
    tmp = (FFIMPORT *)sType->aux;
    return( (ffi_type *)tmp->cstruct );
    }

errorProcExt2(ERR_FFI_INVALID_TYPE, stuffString(type) );

return(NULL);
}
    
#define MAX_TYPE_ARGS 32

CELL * ffiPreparation(CELL * pCell, CELL * params, int type)
    {
    CELL * arg;
    FFIMPORT * ffi;
    ffi_type **atypes;
    unsigned int nargs;
    ffi_type *rtype;

    if(pCell->type != CELL_IMPORT_FFI)
        errorProcExt(ERR_INVALID_PARAMETER, pCell);

    ffi = (FFIMPORT *)pCell->aux;
    ffi->func = FFI_FN(pCell->contents);
    ffi->type = type;

    atypes = calloc(sizeof(ffi_type) * MAX_TYPE_ARGS, 1);

    rtype = &ffi_type_void;

    /* don't consume first argument when building structs */
    if(type != FFI_STRUCT)
        {
        arg = evaluateExpression(params);
        if(arg->type == CELL_STRING)
            rtype = getFFIType((char *) arg->contents);
        else
            errorProcExt(ERR_STRING_EXPECTED, arg);
        params = params->next;
        }

    nargs=0;
    while((arg = evaluateExpression(params)) != nilCell)
        {
        if(arg->type == CELL_STRING)
            {
            /* skip void arguments */
            if(strcmp((char *)arg->contents,"void") != 0)
                {
                atypes[nargs] = getFFIType((char *) arg->contents);
                nargs++;
                }
            }
        else
            errorProcExt(ERR_INVALID_PARAMETER,pCell);
        params = params->next;

        if(nargs == MAX_TYPE_ARGS)
            break; 
        } 

    ffi->cif.nargs = nargs;
    ffi->cif.arg_types = atypes;
    if(ffi_prep_cif(&ffi->cif, FFI_DEFAULT_ABI, nargs, rtype, atypes) != FFI_OK)
        errorProcExt(ERR_FFI_PREP_FAILED, pCell);

    return(pCell);
}



CELL * executeLibFFI(CELL * pCell, CELL * params)
{
    FFIMPORT * ffi;
    int c = 0;
    ffi_type * ffiType;
    INT64 value64;
    UINT value;
    void **avalues;
    CELL * cell;
    void * result;
    double valueDouble;

    ffi = (FFIMPORT *)pCell->aux;
    if(ffi->type == FFI_STRUCT)
        return(pCell);

    avalues = alloca(sizeof(void *) * 16);

    while(params != nilCell)
        {
        if(c >= ffi->cif.nargs) /* too much args */
            errorProc(ERR_NUM_ARGS);

        ffiType = ffi->cif.arg_types[c];
        if(ffiType !=  &ffi_type_void)
            avalues[c] = alloca(ffiType->size);
        /* printf("c: %d size:%ld\n", c, ffiType->size); */

        if(ffiType->type == FFI_TYPE_STRUCT)
            {
            cell = evaluateExpression(params);
            if(cell->type == CELL_STRING)
                memcpy(avalues[c],(void*) cell->contents,ffiType->size);
            else
                {
                getIntegerExt(cell, (UINT *) &value, FALSE);
                memcpy(avalues[c], (void **) value, ffiType->size);
                }
            params = params->next;
            }
        else if(ffiType ==  &ffi_type_pointer) /*  "void*" */
            {
            cell = evaluateExpression(params);
            if(cell->type == CELL_STRING)
                *(UINT *)avalues[c] = cell->contents;
            else
                getIntegerExt(cell, avalues[c], FALSE);
            params = params->next;
            }
        else if(ffiType ==  &ffi_type_charpointer) /*  "char*" */
            {
            params = getString(params, avalues[c]);
            }
        else if(ffiType == &ffi_type_float)  /* float 64-bit */
            {
            params = getFloat(params, &valueDouble);
            *(float *)avalues[c] = valueDouble;
            }
        else if(ffiType == &ffi_type_double)  /* double 64-bit */
            params = getFloat(params, avalues[c]);
        else if(ffiType == &ffi_type_sint64 
               || ffiType == &ffi_type_uint64) /* int 64-bit unsiged 64-bit in newLISP */
            {
            params = getInteger64Ext(params, &value64, TRUE);
            avalues[c] = alloca(sizeof(INT64));
            *(INT64 *)avalues[c] = value64;
            }
        else
            { 
            params = getInteger(params, &value);
            if(ffiType == &ffi_type_sint8) /* char 8-bit */
                *(char *)avalues[c] = value;
            else if(ffiType == &ffi_type_uint8) /* unsigned char 8-bit */
                *(unsigned char *)avalues[c] = value;
            else if(ffiType == &ffi_type_sint16) /* short int 16-bit */
                *(short int *)avalues[c] = value;
            else if(ffiType == &ffi_type_uint16) /* unsigned short int 16-bit */
                *(unsigned short int *)avalues[c] = value;
            else if(ffiType == &ffi_type_sint32) /* int 32-bit */
                *(int *)avalues[c] = value;
            else if(ffiType == &ffi_type_uint32) /* unsigned int 32-bit */
                *(unsigned int *)avalues[c] = value;
            }
        c++;
        } 
    if(c < ffi->cif.nargs) /* not enough args */
        errorProc(ERR_NUM_ARGS); 

    result = (ffi_type *) alloca(sizeof(ffi->cif.rtype->size));

    ffi_call(&ffi->cif, FFI_FN(pCell->contents), result, avalues);

    return ffiTypeToCell(ffi->cif.rtype, result);
}


CELL * ffiTypeToCell(ffi_type *type, void * result)
    {
    double valueDouble;

    /* returning a structure */
    if(type->type == FFI_TYPE_STRUCT) 
        return(unpackFFI(type, result));
/*
        return stuffStringN(result, type->size);
*/

    /* for C displayable strings use return type "char*"
       for destructuring pointers to binary info
       use "void*" as return type */
    else if(type == &ffi_type_charpointer)
        return stuffString(*(char **)result);
    else if(type == &ffi_type_pointer)
        return stuffInteger(*((UINT  *)result));
    else if(type == &ffi_type_double)
        return stuffFloat(*(double *)result);
    else if(type == &ffi_type_float)
        {
        valueDouble = *(float *)result;
        return stuffFloat(valueDouble);
        }
    else if(type == &ffi_type_sint8)
        return stuffInteger(*((char *)result));
    else if(type == &ffi_type_uint8)
        return stuffInteger(*((unsigned char *)result));
    else if(type == &ffi_type_sint16)
        return stuffInteger(*((short int *)result));
    else if(type == &ffi_type_uint16)
        return stuffInteger(*((unsigned short int *)result));
    else if(type == &ffi_type_sint32)
        return stuffInteger(*((int *)result));
    else if(type == &ffi_type_uint32)
        return stuffInteger64(*((unsigned int *)result));
    else if(type == &ffi_type_sint64 || type == &ffi_type_uint64)
        return stuffInteger64(*((INT64 *)result));

    return nilCell;
    }

/* This function gets called by external C functions/libraries */

void ffi_trampoline(ffi_cif *cif, void *ret, void **args, void *data)
{

    SYMBOL *symbol;    
    CELL *arg = NULL;
    CELL *next;
    CELL *result;
    int c;
    INT64 value64;
    UINT value;

    if(!cif)
        return;

    if(ret)
        memset(ret, 0, cif->rtype->size);

    symbol = (SYMBOL *) ((ffi_closure_data*) data)->symbol;    
    if(!symbol)
        return;
    
    if(cif->nargs)
		{
        arg = next = ffiTypeToCell(cif->arg_types[0], args[0]);
		for(c = 1;c < cif->nargs; c++)
			next = next->next =  ffiTypeToCell(cif->arg_types[c], args[c]);
		}

    executeSymbol(symbol,(CELL *) arg, &result);
    
    if((cif->rtype ==  &ffi_type_pointer) || (cif->rtype->type == FFI_TYPE_STRUCT) ) /* string or void pointer, or struct */
        *(UINT *) ret = (UINT) result->contents;
    else if(cif->rtype == &ffi_type_float)  /* float 64-bit */
        *(float *)ret = (float) getDirectFloat(result);
    else if(cif->rtype == &ffi_type_double)  /* double 64-bit */
        *(double *)ret = getDirectFloat(result);
    /* int 64-bit in newLISP */
    else if(cif->rtype == &ffi_type_sint64 || cif->rtype == &ffi_type_uint64) 
        {
        getInteger64Ext(result, &value64, TRUE);
        *(INT64 *)ret = value64;
        }
    else if(cif->rtype == &ffi_type_void)
        goto TRAMPOLINE_FINISH;
    else 
        {
        getIntegerExt(result, &value, FALSE);
        if(cif->rtype == &ffi_type_sint8) /* char 8-bit */
            *(char *)ret = (char) value;
        else if(cif->rtype == &ffi_type_uint8) /* unsigned char 8-bit */
            *(unsigned char *)ret = (unsigned char) value;
        else if(cif->rtype == &ffi_type_sint16) /* short int 16-bit */
            *(short int *)ret = (short int) value;
        else if(cif->rtype == &ffi_type_uint16) /* unsigned short int 16-bit */
            *(unsigned short int *)ret = (unsigned short int) value;
        else if(cif->rtype == &ffi_type_sint32) /* int 32-bit */
            *(int *)ret = (int) value;
        else if(cif->rtype == &ffi_type_uint32) /* unsigned int 32-bit */
            *(unsigned int *)ret = (unsigned int) value;
        else
            *(double *)ret = sqrt(-1.0); /* return NaN; never happens */
        }

    TRAMPOLINE_FINISH:
    deleteList(result); 
    return;
    }
#endif /* FFI */

/* end of file */
