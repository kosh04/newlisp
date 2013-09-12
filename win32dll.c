/* win32dll.c - make the newlisp.exe usable as a DLL 

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
#include <winsock2.h>

#define EXPORT __declspec(dllexport) __stdcall

extern char linkOffset[];
extern void loadStartup(char *name);
extern LPSTR getLibname(void);
extern int evalSilent;
extern int opsys;
extern SYMBOL * mainArgsSymbol;


int dllInitialized = 0;
WSADATA WSAData;

char libName[MAX_LINE] = "newlisp.dll";

void initializeMain(void)
{
char name[MAX_LINE + 1];
char * initFile;

WSAStartup(MAKEWORD(2,2), &WSAData);

opsys += 64;

#ifdef SUPPORT_UTF8
opsys += 128;
#endif

#ifdef FFI
opsys += 1024;
initFFI();
#endif

initLocale();
initStacks();
initialize();
mainArgsSymbol->contents = (UINT)getCell(CELL_EXPRESSION);

initFile = getenv("NEWLISPLIB_INIT");

if(initFile)
	{
	strncpy(name, initFile, MAX_LINE);
	loadFile(name, 0, 0, mainContext);
	}

if(strncmp(linkOffset, "@@@@@@@@", 8)) /* contains linked source */
	{
	GetModuleFileName(GetModuleHandle(libName), name, MAX_LINE);
	loadFile(name, *(UINT*)linkOffset, 1, mainContext);
	}

dllInitialized = 1;
reset();
}

/* ------------ initialize DLL (gets called by Windows) ----------------- */

extern STREAM errorStream;

STREAM libStrStream = {0, NULL, NULL, 0, 0};

int CALLBACK LibMain(HANDLE hModule, WORD wDataSeg, WORD cbHeapSize, LPSTR lpszCmdLine)
{
return 1;
}


/* called automatically from nl-import when DLL is loaded from newLISP */

int EXPORT dllName(LPSTR name)
{
strncpy(libName, name, MAX_LINE);
return(1);
}

/* import and call this to create a console for stdio - debugging I/O  */

int EXPORT debugConsole() {

   IOchannel = stdin;

   if(!AllocConsole())
		return(0);

   if(freopen("CONOUT$","w+t",stdout) == NULL) return(0);
   if(freopen("CONIN$","r+t",stdin) == NULL) return(0);  

return(1);
}

/* ---- imported and called from a client using newlisp.dll ---- */

LPSTR EXPORT newlispEvalStr(LPSTR cmd)
{
if(!dllInitialized) initializeMain();

if(setjmp(errorJump)) 
	{
	reset();
	initStacks();
	if(errorReg) 
		{
		executeSymbol(errorEvent, NULL, NULL);
		return((LPSTR)libStrStream.buffer);
		}
	else
	return((LPSTR)errorStream.buffer);
	}

openStrStream(&libStrStream, MAX_STRING, 1);
executeCommandLine(cmd, (UINT)&libStrStream, NULL);

if(evalSilent) evalSilent = 0;

return((LPSTR)libStrStream.buffer);
}


LPSTR EXPORT dllEvalStr(LPSTR cmd)
{
return(newlispEvalStr(cmd));
}


/* callbacks from newlisp library into caller 

currently only tested with newLISP as caller:

(import "newlisp.dylib" "newlispEvalStr")
(import "newlisp.dylib" "newlispCallback")
(define (callme p1 p2 p3) (println "p1 => " p1 " p2 => " p2 " p3 => " p3) "hello world")
(newlispCallback "callme" (callback 0 'callme) "cdecl")
(get-string (newlispEvalStr {(get-string (callme 123 456 789))})) ; for string return
;(get-string (newlispEvalStr {(callme 123 456 789)})) ; for number return

*/

long EXPORT newlispCallback(char * funcName, long funcAddr, char * callType)
{
CELL * pCell;
SYMBOL * symbol;

if(!dllInitialized) initializeMain();

if(callType != NULL && strcmp(callType, "cdecl") ==  0)
	pCell = getCell(CELL_IMPORT_CDECL);
else
	pCell = getCell(CELL_IMPORT_DLL);

symbol = translateCreateSymbol(funcName, pCell->type, currentContext, TRUE);

if(isProtected(symbol->flags))
    {
	errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol));
    return(-1);
    }

deleteList((CELL *)symbol->contents);
symbol->contents = (UINT)pCell;
pCell->contents = (UINT)funcAddr;

pCell->aux = (UINT)symbol->name;

return(funcAddr);
}


/* ------------ called from Windows when unloading DLL ------------------ */

int EXPORT WEP (int bSystemExit)
{
return(1);
}

/* eof */
