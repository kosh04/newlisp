/* unix-lib.c - make the newlisp shared newlisp library

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

extern void setupAllSignals(void);
extern void loadStartup(char * name);
extern int evalSilent;
extern int opsys;
extern SYMBOL * mainArgsSymbol;
extern char linkOffset[];

int libInitialized = 0;

void initializeMain(void)
{
char name[MAX_LINE + 1];
char * initFile;

opsys += 64;

#ifdef SUPPORT_UTF8
opsys += 128;
#endif
#ifdef NEWLISP64
opsys += 256;
#endif

initLocale();
initialize();
mainArgsSymbol->contents = (UINT)getCell(CELL_EXPRESSION);
setupAllSignals();
initStacks();
initDefaultInAddr(); 

initFile = getenv("NEWLISPLIB_INIT");
if(initFile)
	{
	strncpy(name, initFile, MAX_LINE);
	loadFile(name, 0, 0, mainContext);
	}

if(strncmp(linkOffset, "@@@@@@@@", 8)) /* contains linked source */
	loadFile("newlisp.so", *(UINT*)linkOffset, 1, mainContext);

libInitialized = 1;
reset();
}


extern STREAM errorStream;
STREAM libStrStream = {0, NULL, NULL, 0, 0};


/* ---- imported and called from a client using newlisp.so ---- */

char * newlispEvalStr(char * cmd)
{
if(!libInitialized) initializeMain();

if(setjmp(errorJump)) 
	{
	setupAllSignals(); 

	reset();
	initStacks();

	if(errorReg) 
		{
		executeSymbol(errorEvent, NULL, NULL);
		return(libStrStream.buffer);
		}
	else
	return(errorStream.buffer);
	}

openStrStream(&libStrStream, MAX_STRING, 1);
executeCommandLine(cmd, (UINT)&libStrStream, NULL);

if(evalSilent) evalSilent = 0;

return(libStrStream.buffer);
}

/* eof */
