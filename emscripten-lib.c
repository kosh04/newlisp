/* emscripten-lib.c - make the newlisp for Emscripten

    Copyright (C) 2014 Lutz Mueller

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


#define LIBNAME "newlisp-js-lib.js"

extern int bigEndian;
extern char preLoad[];
CELL * sysEvalString(char * str, SYMBOL * context, CELL * proc, int mode);
extern void setupAllSignals(void);
extern int evalSilent;
extern int opsys;
extern SYMBOL * mainArgsSymbol;
extern char linkOffset[];
extern FILE * IOchannel;

int libInitialized = 0;

void initializeMain(void)
{
opsys += 64;

#ifdef SUPPORT_UTF8
opsys += 128;
#endif

#ifdef NEWLISP64
opsys += 256;
#endif

bigEndian = (*((char *)&bigEndian) == 0);

IOchannel = stdin;

initLocale();
initStacks();
initialize();
mainArgsSymbol->contents = (UINT)makeCell(CELL_EXPRESSION, (UINT)stuffString(LIBNAME));
sysEvalString(preLoad, mainContext, nilCell, EVAL_STRING);

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
    /* setupAllSignals(); taken out for LIBRARY in 10.5.7 */

    reset();
    initStacks();

    if(errorReg) 
        {
        executeSymbol(errorEvent, NULL, NULL);
        return(libStrStream.buffer);
        }
    else
        return(libStrStream.buffer);
    }

openStrStream(&libStrStream, MAX_STRING, 1);

executeCommandLine(cmd, (UINT)&libStrStream, NULL);

if(evalSilent) evalSilent = 0;

return(libStrStream.buffer);
}

/* for callbacks 'eval-string-js' is used see p_evalStringJS in newlisp.c */

/* eof */
