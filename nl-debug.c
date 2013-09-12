/* nl-debug.c --- debugging functions for newLISP

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

#ifdef WIN_32
#define fgets win32_fgets
#endif

extern FILE * IOchannel;
extern int evalSilent;

int matchString(char * buffer,  char * string, int * length);
int stringComp(char * buffer, char * string);
int printFunction(CELL * cell);
void getDebuggerInput(char * msg);

extern SYMBOL * currentFunc;
extern SYMBOL * timerEvent;
extern SYMBOL * cilkEvent;
extern SYMBOL * symHandler[];
extern int currentSignal;

int currentLevel = 0;
int debugStackIdx = 0;
UINT * debugStack;

char preStr[8] = "#";
char postStr[8] = "#";
char headerStr[16] = "\n-----\n\n";
char footerStr[32] = " s|tep n|ext c|ont q|uit > ";

#define pushDebugStack(A) (*(debugStack + debugStackIdx++) = (UINT)(A))
#define popDebugStack() (*(debugStack + --debugStackIdx))

#define DEBUG_ENTRY "->"
#define DEBUG_EXIT "<-"

void openTrace(void)	
	{
	if(traceFlag) return;
	traceFlag = TRUE;
	currentFunc = nilSymbol;
	debugStackIdx = 0;
	debugStack = (UINT *)allocMemory(MAX_CPU_STACK * 2 * sizeof(UINT));
	}

void closeTrace(void)
	{
	traceFlag = 0;
	if(debugStack) free(debugStack);
	debugStack = NULL;
	}


CELL * p_debug(CELL * params)
{
CELL * result;

openTrace();
result = copyCell(evaluateExpression(params));
closeTrace();

return(result);
}


CELL * p_trace(CELL * params)
{
if(params != nilCell)
	{
	if(getFlag(params))
		openTrace();
	else
		closeTrace();
	}

return((traceFlag == 0 ? nilCell : trueCell));
}

CELL * p_traceHighlight(CELL * params)
{
char * pre, * post, * header, * footer;

params = getString(params, &pre);
params = getString(params, &post);

strncpy(preStr, pre, 7);
strncpy(postStr, post, 7);

if(params != nilCell)
	{
	params = getString(params, &header);
	strncpy(headerStr, header, 15);
	}

if(params != nilCell)
	{
	getString(params, &footer);
	strncpy(footerStr, footer, 31);
	}


return(trueCell);
}


void traceEntry(CELL * cell, CELL * pCell, CELL * args)
{
int defaultFuncFlag = FALSE;

if(traceFlag & (TRACE_IN_ENTRY | TRACE_IN_EXIT | TRACE_DEBUG_NEXT)) return;
traceFlag |= TRACE_IN_ENTRY;

if(traceFlag & TRACE_SIGNAL)
	{
	traceFlag &= ~TRACE_SIGNAL;
	executeSymbol(symHandler[currentSignal - 1], stuffInteger(currentSignal), NULL);
	traceFlag &= ~TRACE_IN_ENTRY;
	return;
	}

if(traceFlag & TRACE_SIGINT)
	{
	traceFlag &= ~TRACE_SIGINT;
	longjmp(errorJump, ERR_USER_RESET);
	}
	
if(traceFlag & TRACE_TIMER)
	{
	traceFlag &= ~TRACE_TIMER;
	executeSymbol(timerEvent, NULL, NULL);
	traceFlag &= ~TRACE_IN_ENTRY;
	return;
	}

if(debugStackIdx > 1)
	{
	if(printFunction(cell))
		getDebuggerInput(DEBUG_ENTRY);
	if(!traceFlag) return;
	}

if(traceFlag & TRACE_DEBUG_NEXT)
	{
	traceFlag &= ~TRACE_IN_ENTRY;
	return;
	}

if(pCell->type == CELL_CONTEXT)
	{
	defaultFuncFlag = TRUE;
	currentFunc = translateCreateSymbol(
			((SYMBOL*)pCell->contents)->name,
			CELL_NIL,
			(SYMBOL*)pCell->contents,
			TRUE);

	pCell = (CELL *)currentFunc->contents;
	}

if((pCell->type == CELL_LAMBDA || pCell->type == CELL_MACRO)
	&& args->type == CELL_SYMBOL)
	{
	if(debugStackIdx == 0) /* startup */
		traceFlag &= ~TRACE_DEBUG_NEXT;
	
	if(!defaultFuncFlag)
		currentFunc = (SYMBOL *)args->contents;	
	pushDebugStack(recursionCount);
	pushDebugStack(currentFunc);
	}

traceFlag &= ~TRACE_IN_ENTRY;
}


void traceExit(CELL * result, CELL * cell, CELL * pCell, CELL * args)
{
if(traceFlag & (TRACE_IN_ENTRY | TRACE_IN_EXIT | TRACE_SIGNAL | TRACE_SIGINT | TRACE_TIMER)) return;
traceFlag |= TRACE_IN_EXIT;

if(traceFlag & TRACE_DEBUG_NEXT)
	{
	if(currentLevel >= recursionCount)
		traceFlag &= ~TRACE_DEBUG_NEXT;
	else 
		{
		traceFlag &= ~TRACE_IN_EXIT;
		return;
		}
	}

if( (pCell->type == CELL_LAMBDA || pCell->type == CELL_MACRO)
		&& args->type == CELL_SYMBOL)
	{
	if((UINT)recursionCount == *(debugStack + debugStackIdx - 2) )
		{
		debugStackIdx -= 2;
		if(debugStackIdx > 0)
			currentFunc = (SYMBOL *)*(debugStack + debugStackIdx - 1);
		if(debugStackIdx == 0)
			traceFlag &= ~TRACE_DEBUG_NEXT;
		}
	}

if(printFunction(cell))
	{
	varPrintf(OUT_CONSOLE, "\nRESULT: ");
	printCell(result, TRUE, OUT_CONSOLE);
	varPrintf(OUT_CONSOLE, "\n");

	if(debugStackIdx > 0)
		{
		getDebuggerInput(DEBUG_EXIT);
		if(!traceFlag) return;
		}
	}

if(traceFlag & TRACE_DEBUG_NEXT)
	currentLevel = recursionCount;

traceFlag &= ~TRACE_IN_EXIT;
}


void getDebuggerInput(char * msg)
{
char command[MAX_LINE];
char * context;
jmp_buf errorJumpSave;
UINT * resultStackIdxSave;
SYMBOL * contextSave;

while(TRUE)
	{

	if(currentContext != mainContext)
		context = currentContext->name;
	else context = "";


	if(!evalSilent)
		varPrintf(OUT_CONSOLE, "\n[%s %d %s]%s", msg, recursionCount, context, footerStr);
	else evalSilent = FALSE;

	if(fgets(command, MAX_LINE - 1, IOchannel) == NULL)
		fatalError(ERR_IO_ERROR, 0, 0);

	if( (strcmp(command, "n\n") == 0) || (strcmp(command, "n\r\n") == 0))
		{
		traceFlag |= TRACE_DEBUG_NEXT;
		currentLevel = recursionCount;
		break;
		}
	else if( (strcmp(command, "s\n") == 0) || (strcmp(command, "s\r\n") == 0))
		{
		traceFlag &= ~TRACE_DEBUG_NEXT;
		break;
		}
	else if( (strcmp(command, "q\n") == 0) || (strcmp(command, "q\r\n") == 0))
		{
		closeTrace();
		longjmp(errorJump, ERR_USER_RESET);
		}
	else if( (strcmp(command, "c\n") == 0) || (strcmp(command, "c\r\n") == 0))
		{
		closeTrace();
		break;
		}

	resultStackIdxSave = resultStackIdx;
	memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
	contextSave = currentContext;
	currentContext = currentFunc->context;
	if(setjmp(errorJump))
		{
		cleanupResults(resultStackIdxSave);
		goto DEBUG_EVAL_END;
		}

	executeCommandLine(command, OUT_CONSOLE, NULL);	

	DEBUG_EVAL_END:
	currentContext = contextSave;
	memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
	}
}


int printFunction(CELL * cell)
{
char * funcStr;
char * expStr;
int start, length;
STREAM strStream = {0, NULL, NULL, 0, 0};

if(currentFunc == nilSymbol) return FALSE;

openStrStream(&strStream, MAX_STRING, 0);
printSymbol(currentFunc, (UINT)&strStream);
length = strlen(strStream.buffer);
funcStr = allocMemory(length + 1);
memcpy(funcStr, strStream.buffer, length + 1);
*(funcStr + length) = 0;

openStrStream(&strStream, MAX_STRING, 0);
printCell((CELL *)cell, TRUE, (UINT)&strStream);
expStr = strStream.buffer;
start = matchString(funcStr, expStr, &length);
closeStrStream(&strStream);

if(start == -1 || length == 0)
	{
	free(funcStr);
	return FALSE;
	}

varPrintf(OUT_CONSOLE, headerStr);

expStr = allocMemory(length + 1);
strncpy(expStr, funcStr + start, length);
*(expStr + length) = 0;
*(funcStr + start) = 0;

varPrintf(OUT_CONSOLE, "%s%s%s%s%s", 
	funcStr, preStr, expStr, postStr, funcStr + start + length);

free(funcStr);

return TRUE;
}


/* search for a string skipping white space
*/
int matchString(char * buffer,  char * string, int * length)
{
int position, flag;

/* strip leading white space */
while(*string <= ' ' && *string > 0) string++;

position = flag = 0;

while(*buffer)
	{
	if(*buffer > ' ')
		if((*length = stringComp(buffer, string)) > 0)
			{
			flag = TRUE;
			break;
			}
	position++;
	buffer++;
	}

if(!flag) return(-1);
return(position);
}


/* compare strings skipping white space
*/
int stringComp(char * buffer, char * string)
{
char * start;

start = buffer;
while(*string && *buffer)
	{
	if(*string > ' ')
		{
		if(*buffer > ' ')
			{
			if(*string != *buffer) return(0);
			string++;
			buffer++;
			}
		else buffer++;
		}
	else string++;
	}
	
if(*string == 0)
	return(buffer - start);
	
return(0);
}


/* eof */


