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

WSAStartup(MAKEWORD(1,1), &WSAData);

opsys += 64;

#ifdef SUPPORT_UTF8
opsys += 128;
#endif

initLocale();
initialize();
mainArgsSymbol->contents = (UINT)getCell(CELL_EXPRESSION);
initStacks();

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



/* ------------ called from Windows when unloading DLL ------------------ */

int EXPORT WEP (int bSystemExit)
{
return(1);
}

/* eof */
