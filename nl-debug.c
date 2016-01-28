/* nl-debug.c --- debugging functions, and functions to trap signals and timers 

    Copyright (C) 2016 Lutz Mueller

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

#ifdef WINDOWS
#define fgets win_fgets
#endif

extern FILE * IOchannel;
extern int evalSilent;

int matchString(char * buffer,  char * string, int * length);
int stringComp(char * buffer, char * string);
int debugPrintFunction(CELL * cell);
void getDebuggerInput(char * msg);

extern SYMBOL * currentFunc;
extern SYMBOL * timerEvent;
extern SYMBOL * cilkEvent;
extern SYMBOL * symHandler[];
extern int currentSignal;

int currentLevel = 0;
int debugStackIdx = 0;
UINT * debugStack;
UINT tracePrintDevice;

char debugPreStr[8] = "#";
char debugPostStr[8] = "#";
CELL * debugPrintCell = NULL;

char headerStr[16] = "\n-----\n\n";
char footerStr[32] = " s|tep n|ext c|ont q|uit > ";

#define pushDebugStack(A) (*(debugStack + debugStackIdx++) = (UINT)(A))
#define popDebugStack() (*(debugStack + --debugStackIdx))

#define DEBUG_ENTRY "->"
#define DEBUG_EXIT "<-"

void openTrace(void)    
    {
    if(traceFlag) return;
    traceFlag = TRACE_TRUE;
    currentFunc = nilSymbol;
    debugStackIdx = 0;
    debugStack = (UINT *)allocMemory(MAX_CPU_STACK * 2 * sizeof(UINT));
    }

void closeTrace(void)
    {
    if(traceFlag & TRACE_PRINT_EVAL) 
        close(tracePrintDevice);
    traceFlag = 0;
    if(debugStack) free(debugStack);
    debugStack = NULL;
    }

#ifdef DEBUGGER
CELL * p_debug(CELL * params)
{
CELL * result;

openTrace();
traceFlag |= TRACE_IN_DEBUG;
result = copyCell(evaluateExpression(params));
closeTrace();

return(result);
}
#endif


CELL * p_trace(CELL * params)
{
if(params != nilCell)
    {
    params = evaluateExpression(params);
    if(isNumber(params->type))
        {
        traceFlag |= TRACE_PRINT_EVAL;
        getIntegerExt(params, &tracePrintDevice, FALSE);
        return(stuffInteger(tracePrintDevice));
        }
    if(!isNil(params))
        {
        openTrace();
        traceFlag |= TRACE_IN_DEBUG;
        }
    else
        closeTrace();
    }

if(traceFlag & TRACE_IN_DEBUG) return(trueCell);
if(traceFlag & TRACE_PRINT_EVAL) return(stuffInteger(tracePrintDevice));
return(nilCell);
}

#ifdef DEBUGGER
CELL * p_traceHighlight(CELL * params)
{
char * pre, * post, * header, * footer;

params = getString(params, &pre);
params = getString(params, &post);

strncpy(debugPreStr, pre, 8);
strncpy(debugPostStr, post, 8);
*(debugPreStr + 7) = 0;
*(debugPostStr + 7) = 0;

if(params != nilCell)
    {
    params = getString(params, &header);
    strncpy(headerStr, header, 16);
    }

if(params != nilCell)
    {
    getString(params, &footer);
    strncpy(footerStr, footer, 32);
    }

*(headerStr + 15) = 0;
*(footerStr + 31) = 0;

return(trueCell);
}
#endif /* DEBUGGER */

void tracePrint(char * label, CELL * expr)
{
UINT printDeviceSave = printDevice;

printDevice = tracePrintDevice;
varPrintf(OUT_DEVICE, "%d %s: ", recursionCount, label);
if(expr) printCell(expr, TRUE, OUT_DEVICE);
varPrintf(OUT_DEVICE, "%s", "\n");
printDevice = printDeviceSave;
}

void traceEntry(CELL * cell, CELL * pCell, CELL * args)
{
if(traceFlag & (TRACE_IN_ENTRY | TRACE_IN_EXIT | TRACE_DEBUG_NEXT)) return;
traceFlag |= TRACE_IN_ENTRY;

#ifdef DEBUGGER
int defaultFuncFlag = FALSE;
#endif

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

if(traceFlag & TRACE_PRINT_EVAL)
    {
    if(cell->type == CELL_EXPRESSION) tracePrint("entry", cell);
    traceFlag &= ~TRACE_IN_ENTRY;
    return;
    }

#ifdef DEBUGGER
if(debugStackIdx > 1)
    {
    if(debugPrintFunction(cell))
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

if((pCell->type == CELL_LAMBDA || pCell->type == CELL_FEXPR)
    && args->type == CELL_SYMBOL)
    {
    if(debugStackIdx == 0) /* startup */
        traceFlag &= ~TRACE_DEBUG_NEXT;
    
    if(!defaultFuncFlag)
        currentFunc = (SYMBOL *)args->contents; 
    pushDebugStack(recursionCount);
    pushDebugStack(currentFunc);
    }
#endif /* no_DEBUG */

traceFlag &= ~TRACE_IN_ENTRY;
}


void traceExit(CELL * result, CELL * cell, CELL * pCell, CELL * args)
{
if(traceFlag & (TRACE_IN_ENTRY | TRACE_IN_EXIT | TRACE_SIGNAL | TRACE_SIGINT | TRACE_TIMER)) return;

if(traceFlag == TRACE_PRINT_EVAL)
    {
    tracePrint("exit", result);
    return;
    }

traceFlag |= TRACE_IN_EXIT;

#ifdef DEBUGGER
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

if( (pCell->type == CELL_LAMBDA || pCell->type == CELL_FEXPR)
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

if(debugPrintFunction(cell))
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
#endif /* DEBUGGER */

traceFlag &= ~TRACE_IN_EXIT;
}

#ifdef DEBUGGER
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

    /* client and server could have different line-termination */
    if(*(command + 1) == '\n' || *(command + 1) == '\r')
        {
        if(*command == 'n')
            {
            traceFlag |= TRACE_DEBUG_NEXT;
            currentLevel = recursionCount;
            break;
            }
        if(*command == 's')
            {
            traceFlag &= ~TRACE_DEBUG_NEXT;
            break;
            }
        if(*command == 'q')
            {
            closeTrace();
            longjmp(errorJump, ERR_USER_RESET);
            }
        if(*command == 'c')
            {
            closeTrace();
            break;
            }
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


int debugPrintFunction(CELL * cell)
{
int preLen, pos = 0;
char * strPos;

STREAM strStream = {NULL, NULL, 0, 0, 0};

if(currentFunc == nilSymbol) return FALSE;

debugPrintCell = cell;
openStrStream(&strStream, MAX_STRING, 0);
printSymbol(currentFunc, (UINT)&strStream);
debugPrintCell = NULL;

strPos = strstr(strStream.buffer, debugPreStr);
if(strPos != NULL)
    {
    preLen = strlen(debugPreStr);
    while(*(strPos + preLen + pos) <= ' ' && *(strPos + preLen + pos) != 0) ++pos;
    if(pos) /* if there is white space */
        {
        /* swap whitespace and debugPreStr */
        strncpy(strPos, strPos + preLen, pos);
        strncpy(strPos + pos, debugPreStr, preLen);
        }
    varPrintf(OUT_CONSOLE, "%s", headerStr);
    varPrintf(OUT_CONSOLE, "%s", strStream.buffer);
    }

closeStrStream(&strStream);
return (strPos != NULL);
}

#endif /* DEBUGGER */
/* eof */


