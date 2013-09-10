/* win32dll.c - make the newlisp.exe usable as a DLL 

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
#include "protos.h"

#ifdef WIN_32
#define EXPORT __declspec(dllexport) __stdcall
#define DLLCALL EXPORT
#endif

#include <winsock2.h>

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

WSAStartup(MAKEWORD(1,1), &WSAData);

opsys += 64;

#ifdef SUPPORT_UTF8
opsys += 128;
#endif
#ifdef IPV6
opsys += 512
#endif

initLocale();
initialize();
mainArgsSymbol->contents = (UINT)getCell(CELL_EXPRESSION);
initStacks();

GetModuleFileName(GetModuleHandle(libName), name, MAX_LINE);

loadStartup(name);

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

int DLLCALL dllName(LPSTR name)
{
strncpy(libName, name, MAX_LINE);
return(1);
}


/* ---- imported and called from a client using newlisp.dll ---- */

LPSTR DLLCALL newlispEvalStr(LPSTR cmd)
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


LPSTR DLLCALL dllEvalStr(LPSTR cmd)
{
return(newlispEvalStr(cmd));
}



/* ------------ called from Windows when unloading DLL ------------------ */

int DLLCALL WEP (int bSystemExit)
{
return(1);
}

/* eof */
