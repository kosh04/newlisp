/* 
   win-util.c, utitity routines for native windows port of newLISP 

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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#define WIN32_LEAN_AND_MEAN     /* exclude rarely-used windows headers */
#include <windows.h>
#include <io.h>
#include <process.h>


/* from newlisp.c */
int init_argv(char * ptr, char *argv[]);
void * callocMemory(size_t nbytes);

WINBASEAPI DWORD WINAPI GetProcessId(HANDLE);

/*
typedef struct _PROCESS_INFORMATION {
    HANDLE hProcess; 
    HANDLE hThread; 
    DWORD dwProcessId; 
    DWORD dwThreadId; 
} PROCESS_INFORMATION; 
*/


/* kill for MinGW
   by Shigeru Kobayashi for v10.4.5
   to be used by p_destroyProcess()
   see also changes in plainProcess() and winPipedProcess() below
*/

int kill(pid_t pid, int sig)
{
int ret;
HANDLE h;

h = OpenProcess(PROCESS_TERMINATE, FALSE, pid);
if (h == NULL) return -1;

ret = TerminateProcess(h, 0) ? 0 : -1;
CloseHandle(h);
return ret;
}

/* ---------------------------- pipes -------------------------------------- */


UINT plainProcess(char * command, size_t len)
{
char * cPtr;
char * argv[16];
HANDLE hProc;
DWORD pid;

cPtr = callocMemory(len + 1);
memcpy(cPtr, command, len + 1);

init_argv(cPtr, argv);

hProc = (HANDLE)_spawnvp(P_NOWAIT, argv[0], (const char * const *)argv); 

free(cPtr);
if(hProc == INVALID_HANDLE_VALUE) return(0);

pid = GetProcessId(hProc);
CloseHandle(hProc);

return(pid);
}


UINT winPipedProcess(char * cmd, int inpipe, int outpipe, int option)
{
STARTUPINFO si = { 0 };
PROCESS_INFORMATION process;
int result;
HANDLE fin, fout; 
UINT pid;

if(inpipe == -1 && outpipe == -1)
    {
    memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    si.wShowWindow = option; 
    memset(&process, 0, sizeof(process));
    }
else
    {
    /* GetStartupInfo(&si);  */
    si.cb = sizeof(si);
    si.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
    si.wShowWindow = option; /* SW_SHOW, SW_HIDE  get additional user option in Win32 versions */

    fin  = (HANDLE)_get_osfhandle(inpipe);
    fout = (HANDLE)_get_osfhandle(outpipe);

    si.hStdInput  = (inpipe)  ? fin  : GetStdHandle(STD_INPUT_HANDLE);
    si.hStdOutput = (outpipe) ? fout : GetStdHandle(STD_OUTPUT_HANDLE);
    si.hStdError  = (outpipe) ? fout : GetStdHandle(STD_OUTPUT_HANDLE);
    }

if((result = CreateProcess(NULL,cmd,NULL,NULL,TRUE,DETACHED_PROCESS,NULL,NULL,&si, &process)) == 0)
    return(0); 

pid = GetProcessId(process.hProcess);
CloseHandle(process.hProcess);

return(pid);
}


int winPipe(UINT * inpipe, UINT * outpipe)
{
SECURITY_ATTRIBUTES sa = { 0 };
HANDLE pipe_r, pipe_w;

sa.nLength = sizeof(sa);
sa.bInheritHandle = TRUE;
sa.lpSecurityDescriptor = NULL;

if(!CreatePipe(&pipe_r, &pipe_w, &sa, 0))
    return(0);

*inpipe = _open_osfhandle((intptr_t) pipe_r, 0);
*outpipe = _open_osfhandle((intptr_t) pipe_w, 0);


return(1);
}


/* ---------------------------------- semaphores ---------------------------- */

UINT winCreateSemaphore(void)
{
SECURITY_ATTRIBUTES security = { 0 };
HANDLE hSemaphore;

security.nLength = sizeof(security);
security.bInheritHandle = TRUE;
security.lpSecurityDescriptor = NULL; /* default of caller */

hSemaphore = CreateSemaphore(&security, 0, 65536, NULL);

return((UINT)hSemaphore);
} 

UINT winWaitSemaphore(UINT hSemaphore)
{
DWORD dwWaitResult; 

dwWaitResult = WaitForSingleObject((HANDLE)hSemaphore, INFINITE);

if(dwWaitResult == WAIT_FAILED)
    return(FALSE);

return(TRUE);
}

UINT winSignalSemaphore(UINT hSemaphore, int count)
{
return(ReleaseSemaphore((HANDLE)hSemaphore, count, NULL));
}

int winDeleteSemaphore(UINT hSemaphore) 
{
return (CloseHandle((HANDLE)hSemaphore));
}

/*
See: http://www.codeguru.com/Cpp/W-P/win32/article.php/c1423/
QuerySemaphore() call
int winGetSemaphoreCount(UINT hSemaphore) {}
*/

/* ---------------------------- shared memory interface -------------------- */

UINT winSharedMemory(int size)
{
SECURITY_ATTRIBUTES sa = { 0 };
HANDLE hMemory;

sa.nLength = sizeof(sa);
sa.bInheritHandle = TRUE;
sa.lpSecurityDescriptor = NULL; /* default of caller */

hMemory = CreateFileMapping(INVALID_HANDLE_VALUE, &sa, PAGE_READWRITE, 0, size, NULL);

return((UINT)hMemory);
}

UINT * winMapView(UINT hMemory, int size)
{
return((UINT *)MapViewOfFile((HANDLE)hMemory, FILE_MAP_WRITE, 0, 0, size));
}

/* ---------------------------- timer -------------------------------------- */

extern SYMBOL * timerEvent;
extern int milliSecTime(void);
extern CELL * getCreateSymbol(CELL * params, SYMBOL * * symbol, char * name);

/* 
UINT_PTR SetTimer(HWND hWnd, UINT_PTR nIDEvent, UINT uElapse, TIMERPROC lpTimerFunc);
BOOL KillTimer(HWND hWnd, UINT_PTR uIDEvent);
*/

void timerFunc(void * dummy);
int timerDuration = 0;
int rollover = 0;
int start;

CELL * p_timerEvent(CELL * params)
{
CELL * symCell;
double seconds;
int now;

if(params != nilCell) 
  {
  params = getCreateSymbol(params, &timerEvent, "$timer");

  if(params != nilCell)
    {
    getFloat(params, &seconds);
    timerDuration = 1000 * seconds;
    if(timerDuration > 0)
        {
        start = milliSecTime();
        _beginthread(timerFunc, 0, 0);
        seconds = timerDuration/1000.0;
        }
    }
  else
    {
    /* return the elapsed time */
    seconds = ((now = milliSecTime()) < start ? 
            86400000 - start + now :
            now - start)/1000.0;  
    }
  return(stuffFloat(seconds));    
  }
  
symCell = getCell(CELL_SYMBOL);
symCell->contents = (UINT)timerEvent;
return(symCell);
}


void timerFunc(void * dummy)
{
if(timerDuration == 0) return;

/* take care of midnight rollover */
if((rollover = start + timerDuration - 86400000) > 0)
    {
    while(milliSecTime() > start) mySleep(10); /* wait for rollover */
    while(milliSecTime() < rollover) mySleep(10);
    }
else
    while( milliSecTime() < (start + timerDuration) ) mySleep(10);

if(recursionCount)
  traceFlag |= TRACE_TIMER;
else /* if idle */
  executeSymbol(timerEvent, NULL, NULL);
}


extern STREAM errorStream;


/* eof */
