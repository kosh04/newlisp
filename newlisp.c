/* newlisp.c --- enrty point and main functions for newLISP


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
#include "pcre.h"
#include "protos.h"
#include "primes.h"

#ifdef WIN_32
#include <winsock2.h>
#else
#include <sys/socket.h>
#endif

#ifdef READLINE
#include <readline/readline.h>
/* take following line out on Slackware Linux */
#include <readline/history.h>
#endif

#ifdef SUPPORT_UTF8
#include <wctype.h>
#endif

#define freeMemory free

#define INIT_FILE "init.lsp"

#ifdef WIN_32
#define fprintf win32_fprintf
#define fgets win32_fgets
#define fclose win32_fclose
#endif

/* used only in loadStartup() */
#ifndef _BSD
#define strlcpy strncpy
#define strlcat strncat
#endif

#ifdef LIBRARY
extern STREAM libStrStream;
#endif

#ifdef LINUX
int opsys = 1;
#endif

#ifdef _BSD
int opsys = 2;
#endif

#ifdef MAC_OSX
int opsys = 3;
#endif

#ifdef SOLARIS
int opsys = 4;
#endif

#ifdef WIN_32
int opsys = 6;
#endif

#ifdef OS2 
int opsys = 7; 
#endif

#ifdef CYGWIN
int opsys = 8;
#endif

#ifdef TRU64
int opsys = 9; 
#endif

#ifdef AIX
int opsys = 10;
#endif

int version = 10300;

char copyright[]=
"\nnewLISP v.10.3.0 Copyright (c) 2011 Lutz Mueller. All rights reserved.\n\n%s\n\n";

#ifndef NEWLISP64
#ifdef SUPPORT_UTF8
char banner[]=
"newLISP v.10.3.0 on %s IPv4/6 UTF-8%s\n\n";
#else
char banner[]=
"newLISP v.10.3.0 on %s IPv4/6%s\n\n";
#endif
#else
#ifdef SUPPORT_UTF8
char banner[]=
"newLISP v.10.3.0 64-bit on %s IPv4/6 UTF-8%s\n\n";
#else
char banner[]=
"newLISP v.10.3.0 64-bit on %s IPv4/6%s\n\n";
#endif 
#endif

char banner2[]=
", execute 'newlisp -h' for more info.";

char linkOffset[] = "@@@@@@@@";
char preLoad[] = 
	"(define Tree:Tree)"
	"(define (Class:Class) (cons (context) (args)))"
	"(set (global 'module) (fn (m) (load (append (env {NEWLISPDIR}) {/modules/} m))))";
void printHelpText(void);
#ifdef READLINE
char ** newlisp_completion (char * text, int start, int end);
#endif
/* --------------------- globals -------------------------------------- */

/* interactive command line */

int isTTY = FALSE;
int demonMode = 0;

int noPromptMode = 0;
int forcePromptMode = 0;
int httpMode = 0;

#ifdef WIN_32
int IOchannelIsSocketStream = 0;
#endif
FILE * IOchannel;
char * IOdomain = NULL;
int IOport = 0;
int connectionTimeout = 0;

int logTraffic = 0;
#define LOG_LESS 1
#define LOG_MORE 2

/* initialization */
int MAX_CPU_STACK = 0x800;
int MAX_ENV_STACK;
int MAX_RESULT_STACK;
#define MAX_OBJECT_STACK 64
#ifndef NEWLISP64
long MAX_CELL_COUNT = 0x10000000;
#else
long MAX_CELL_COUNT = 0x800000000000000LL;
#endif

CELL * firstFreeCell = NULL;

CELL * nilCell;
CELL * trueCell;
CELL * lastCellCopied;
SYMBOL * nilSymbol;
SYMBOL * trueSymbol;
SYMBOL * starSymbol;
SYMBOL * plusSymbol;
SYMBOL * questionSymbol;
SYMBOL * atSymbol;
SYMBOL * currentFunc;
SYMBOL * argsSymbol;
SYMBOL * mainArgsSymbol;
SYMBOL * dolistIdxSymbol;
SYMBOL * itSymbol;

SYMBOL * sysSymbol[MAX_REGEX_EXP];

SYMBOL * currentContext = NULL;
SYMBOL * mainContext = NULL;
SYMBOL * errorEvent;
SYMBOL * timerEvent;
SYMBOL * promptEvent;
SYMBOL * commandEvent;
SYMBOL * transferEvent;
SYMBOL * readerEvent;

SYMBOL * symHandler[32];
int currentSignal = 0;

SYMBOL * symbolCheck = NULL;
CELL * stringCell = NULL;
void * stringIndexPtr = NULL;

jmp_buf errorJump;

char lc_decimal_point;

/* error and exception handling */

#define EXCEPTION_THROW -1
#define EXCEPTION_CONTINUATION -2
int errorReg = 0;
CELL * throwResult;

/* buffers for read-line and error reporting */
STREAM readLineStream;
STREAM errorStream;

/* compiler */

size_t cellCount = 0;
size_t symbolCount = 0;

int parStackCounter = 0;

/* expression evaluation */

static CELL * (*evalFunc)(CELL *) = NULL;

UINT * envStack = NULL;
UINT * envStackIdx;
UINT * envStackTop;
UINT * resultStack = NULL;
UINT * resultStackIdx;
UINT * resultStackTop;
UINT * lambdaStack = NULL;
UINT * lambdaStackIdx;

/* internal dummy to carry FOOP object */
SYMBOL objSymbol = {SYMBOL_GLOBAL | SYMBOL_BUILTIN, 
	0, "container of (self)", 0, NULL, NULL, NULL, NULL};
CELL * objCell;

int evalSilent = 0;

extern PRIMITIVE primitive[];

int traceFlag = 0;
int evalCatchFlag = 0;
int recursionCount = 0;

int prettyPrintPars = 0;
int prettyPrintCurrent = 0;
int prettyPrintFlags = 0;
int prettyPrintLength = 0;
char * prettyPrintTab = " ";
char * prettyPrintFloat = "%1.10g";
#define MAX_PRETTY_PRINT_LENGTH 80
UINT prettyPrintMaxLength =  MAX_PRETTY_PRINT_LENGTH;
int stringOutputRaw = TRUE;

#define pushLambda(A) (*(lambdaStackIdx++) = (UINT)(A))
#define popLambda() ((CELL *)*(--lambdaStackIdx))

int pushResultFlag = TRUE;

char startupDir[PATH_MAX]; /* start up directory, if defined via -w */
char logFile[PATH_MAX]; /* logFile, is define with -l, -L */

/* memory management in nl-filesys.c */

int pagesize;

/* ============================== MAIN ================================ */

/*
void setupSignalHandler(int sig, void (* handler)(int))
{
static struct sigaction sig_act;
sig_act.sa_handler = handler;
sigemptyset(&sig_act.sa_mask);
sig_act.sa_flags = SA_RESTART | SA_NOCLDSTOP;
if(sigaction(sig, &sig_act, 0) != 0)
	printf("Error setting signal:%d handler\n", sig);
}
*/

void setupSignalHandler(int sig, void (* handler)(int))
{
if(signal(sig, handler) == SIG_ERR)
	printf("Error setting signal:%d handler\n", sig);
}

#if defined(SOLARIS) || defined(TRU64) || defined(AIX)
void sigpipe_handler(int sig)
{
setupSignalHandler(SIGPIPE, sigpipe_handler);
}

void sigchld_handler(int sig)
{
waitpid(-1, (int *)0, WNOHANG);
}

void ctrlC_handler(int sig) 
{
char chr; 

setupSignalHandler(SIGINT, ctrlC_handler);

traceFlag |= TRACE_SIGINT;

printErrorMessage(ERR_SIGINT, NULL, 0);
printf("%s", "(c)ontinue, e(x)it, (r)eset:");
fflush(NULL);
chr = getchar();
if(chr == 'x') exit(1);
if(chr == 'c') traceFlag &= ~TRACE_SIGINT;
}


void sigalrm_handler(int sig)
{
setupSignalHandler(sig, sigalrm_handler);
/* check if not sitting idle */
if(recursionCount)
  traceFlag |= TRACE_TIMER;
else /* if idle */
  executeSymbol(timerEvent, NULL, NULL);
}

#endif /* SOLARIS, TRUE64, AIX */


void setupAllSignals(void)
{
#if defined(SOLARIS) || defined(TRU64) || defined(AIX)
setupSignalHandler(SIGINT, ctrlC_handler);
#else
setupSignalHandler(SIGINT, signal_handler);
#endif

#ifndef WIN_32

#if defined(SOLARIS) || defined(TRU64) || defined(AIX)
setupSignalHandler(SIGALRM, sigalrm_handler);
setupSignalHandler(SIGVTALRM, sigalrm_handler);
setupSignalHandler(SIGPROF, sigalrm_handler);
setupSignalHandler(SIGPIPE, sigpipe_handler);
setupSignalHandler(SIGCHLD, sigchld_handler);
#else
setupSignalHandler(SIGALRM, signal_handler);
setupSignalHandler(SIGVTALRM, signal_handler);
setupSignalHandler(SIGPROF, signal_handler);
setupSignalHandler(SIGPIPE, signal_handler);
setupSignalHandler(SIGCHLD, signal_handler);
#endif

#endif
}

void signal_handler(int sig)
{
#ifndef WIN_32
char chr; 
#endif

if(sig > 32 || sig < 1) return;

#if defined(SOLARIS) || defined(TRU64) || defined(AIX)
switch(sig)
  {
  case SIGALRM:
  case SIGVTALRM:
  case SIGPROF:
    setupSignalHandler(sig, sigalrm_handler);
    break;
  case SIGPIPE:
    setupSignalHandler(SIGPIPE, sigpipe_handler);
    break;
  case SIGCHLD:
    setupSignalHandler(SIGCHLD, sigchld_handler);
    break;
  }
#else
setupSignalHandler(sig, signal_handler);
#endif

if(symHandler[sig - 1] != nilSymbol)
    {
    if(recursionCount)
        {
        currentSignal = sig;
        traceFlag |= TRACE_SIGNAL;
        return;
        }
    else
        {
        executeSymbol(symHandler[sig-1], stuffInteger(sig), NULL);
        return;
        }
    }

switch(sig)
    {
    case SIGINT:
        printErrorMessage(ERR_SIGINT, NULL, 0);

#ifdef WIN_32
        traceFlag |= TRACE_SIGINT;
#else
        printf("%s", "\n(c)ontinue, (d)ebug, e(x)it, (r)eset:");
        fflush(NULL);
        chr = getchar();
        if(chr == 'x') exit(1);
        if(chr == 'd') 
            {
            traceFlag &= ~TRACE_SIGINT;
            openTrace();
            }
        if(chr == 'r') traceFlag |= TRACE_SIGINT;
        break;
    case SIGPIPE:
        break;
    case SIGALRM:
    case SIGVTALRM:
    case SIGPROF:
        /* check if not sitting idle */
        if(recursionCount)
            traceFlag |= TRACE_TIMER;
        else /* if idle */
            executeSymbol(timerEvent, NULL, NULL);
        break;
    case SIGCHLD:
        waitpid(-1, (int *)0, WNOHANG);
#endif
        break;
    default:
        return;
    }	
}
 

void loadStartup(char * name)
{
char initFile[MAX_LINE];
char * envPtr;
#ifdef WIN_32
#ifndef LIBRARY
char EXEName[MAX_LINE];

GetModuleFileName(NULL, EXEName, MAX_LINE);
name = EXEName;
#endif
#endif

/* normal newLISP start up */
if(strncmp(linkOffset, "@@@@", 4) == 0)
	{
	if(getenv("HOME"))
		strncpy(initFile, getenv("HOME"), MAX_LINE - 16);
	else if(getenv("USERPROFILE"))
		strncpy(initFile, getenv("USERPROFILE"), MAX_LINE - 16);
	else if(getenv("DOCUMENT_ROOT"))
		strncpy(initFile, getenv("DOCUMENT_ROOT"), MAX_LINE - 16);

    /* for non BSDs strlcat and strlcpy are redefined as strncat and strncpy */
	strlcat(initFile, "/.", 3);
	strlcat(initFile, INIT_FILE, 9);
	if(loadFile(initFile, 0, 0, mainContext) == NULL)
		{
		envPtr = getenv("NEWLISPDIR");
	  	if(envPtr)
	  		{
			strlcpy(initFile, envPtr, MAX_LINE - 16);
	   		strlcat(initFile, "/", 2);
			strlcat(initFile, INIT_FILE, 9);
			loadFile(initFile, 0, 0, mainContext);		
			}
		}
	}
/* load encrypted part at offset no init.lsp or .init.lsp is loaded */
else
	loadFile(name, *(UINT*)linkOffset, 1, mainContext);
}


#ifdef _BSD
struct lconv    *localeconv(void);
char            *setlocale(int, const char *);  
#endif

void initLocale(void)
{
struct lconv * lc;
char * locale;

#ifndef SUPPORT_UTF8
locale = setlocale(LC_ALL, "C");
#else
locale = setlocale(LC_ALL, "");
#endif

if (locale != NULL)
  stringOutputRaw = (strcmp(locale, "C") == 0);

lc = localeconv();
lc_decimal_point = *lc->decimal_point;
}

/* set NEWLISPDIR only if not set already */
void initNewlispDir(void)
{
#ifdef WIN_32
char * varValue;
char * newlispDir;

if(getenv("NEWLISPDIR") == NULL)
	{
	newlispDir = alloca(MAX_PATH);
	varValue = getenv("PROGRAMFILES");
	if(varValue != NULL)
		{
		strncpy(newlispDir, varValue, MAX_PATH);
		strncat(newlispDir, "/newlisp", 8);
		setenv("NEWLISPDIR", newlispDir, TRUE);
		}
	else setenv("NEWLISPDIR", "newlisp", TRUE);
	}
#else
int result;
if(getenv("NEWLISPDIR") == NULL)
	result = setenv("NEWLISPDIR", NEWLISPDIR, TRUE);
#endif
}

#ifndef  LIBRARY
char * getArg(char * * arg, int argc, int * index)
{
if(strlen(arg[*index]) > 2)
	return(arg[*index] + 2);

if(*index >= (argc - 1))
	{
	printf("missing parameter for %s\n", arg[*index]);
	exit(-1);
	}

*index = *index + 1;

return(arg[*index]);
}

#ifndef WIN_32
char ** MainArgs;
#endif 

CELL * getMainArgs(char * mainArgs[])
{
CELL * argList;
#ifndef LIBRARY
int idx = 0;
#endif

#ifndef WIN_32
MainArgs = mainArgs;
#endif

argList = getCell(CELL_EXPRESSION);

#ifndef LIBRARY
while(mainArgs[idx] != NULL)
	addList(argList, stuffString(mainArgs[idx++]));
#endif 

return(argList);
}

char * getCommandLine(int batchMode);
    
int main(int argc, char * argv[])
{
char command[MAX_COMMAND_LINE];
STREAM cmdStream = {0, NULL, NULL, 0, 0};
char * cmd;
int idx;


#ifdef WIN_32
WSADATA WSAData;
if(WSAStartup(MAKEWORD(2,2), &WSAData) != 0)
	{
	printf("Winsocket initialization failed\n");
    exit(-1);
	}
pagesize = 4096;
#endif

#ifdef SUPPORT_UTF8
opsys += 128;
#endif
#ifdef NEWLISP64
opsys += 256;
#endif

#ifndef WIN_32
#ifndef OS2
pagesize = getpagesize();
#endif
tzset();
#endif

#ifdef OS2
/* Reset the floating point coprocessor */
_fpreset();
#endif

initLocale();
initNewlispDir();
IOchannel = stdin;

initialize();
initStacks();
initDefaultInAddr(); 

mainArgsSymbol->contents = (UINT)getMainArgs(argv);

if((errorReg = setjmp(errorJump)) != 0) 
    {
    if(errorReg && (errorEvent != nilSymbol) ) 
        executeSymbol(errorEvent, NULL, NULL);
    else exit(-1);
    goto AFTER_ERROR_ENTRY;
    }

setupAllSignals();

sysEvalString(preLoad, mainContext, nilCell, EVAL_STRING);

/* loading of init.lsp can be suppressed with -n as first option
   but is never when program is link.lsp'd */
if(argc < 2 || strncmp(argv[1], "-n", 2))
	loadStartup(argv[0]);

errno = 0;

if(realpath(".", startupDir) == NULL)
	fatalError(ERR_IO_ERROR, 0, 0);

for(idx = 1; idx < argc; idx++)
	{
	if(strncmp(argv[idx], "-c", 2) == 0)
		noPromptMode = TRUE;

	if(strncmp(argv[idx], "-C", 2) == 0)
		forcePromptMode = TRUE;

	if(strncmp(argv[idx], "-http", 5) == 0)
		{
		noPromptMode = TRUE;
		httpMode = TRUE;
		}

	if(strncmp(argv[idx], "-s", 2) == 0)
		{
		MAX_CPU_STACK = atoi(getArg(argv, argc, &idx));

		if(MAX_CPU_STACK < 1024) MAX_CPU_STACK = 1024;
		initStacks();
		continue;
		}

	if(strncmp(argv[idx], "-p", 2) == 0 || strncmp(argv[idx], "-d", 2) == 0  )
		{
		if(strncmp(argv[idx], "-d", 2) == 0)
			demonMode = TRUE;

		IOdomain = getArg(argv, argc, &idx);
		IOport = atoi(IOdomain);

		setupServer(0);
		continue;
		}

	if(strncmp(argv[idx], "-t", 2) == 0)
		{
		connectionTimeout = atoi(getArg(argv, argc, &idx));
		continue;
		}

	if(strncmp(argv[idx], "-e", 2) == 0)
		{
		executeCommandLine(getArg(argv, argc, &idx), OUT_CONSOLE, &cmdStream);
		exit(0);
		}		

	if(strncmp(argv[idx], "-l", 2) == 0 || strncmp(argv[idx], "-L", 2) == 0)
		{
		logTraffic = (strncmp(argv[idx], "-L", 2) == 0) ? LOG_MORE : LOG_LESS;
		if(realpath(getArg(argv, argc, &idx), logFile) == NULL) /* log file must exist */
			fatalError(ERR_IO_ERROR, 0, 0);
		continue;
		}

	if(strncmp(argv[idx], "-m", 2) == 0)
		{
#ifndef NEWLISP64
		MAX_CELL_COUNT =  abs(0x0010000 * atoi(getArg(argv, argc, &idx)));
#else
		MAX_CELL_COUNT =  abs(0x0008000 * atoi(getArg(argv, argc, &idx)));
#endif
		continue;
		}

	if(strncmp(argv[idx], "-w", 2) == 0)
		{
		if(realpath(getArg(argv, argc, &idx), startupDir) == NULL 
										|| chdir(startupDir) < 0)
			fatalError(ERR_WORKING_DIR, 0, 0);
		continue;
		}	

	if(strcmp(argv[idx], "-6") == 0)
		{
		ADDR_FAMILY = AF_INET6;
		initDefaultInAddr(); 
		}

	if(strcmp(argv[idx], "-v") == 0)
		{
		varPrintf(OUT_CONSOLE, banner, OSTYPE, ".");
		exit(0);
		}

	if(strcmp(argv[idx], "-h") == 0)
		{
		printHelpText();
		exit(0);
		}
	
	loadFile(argv[idx], 0, 0, mainContext);
	}

AFTER_ERROR_ENTRY:

if(isatty(fileno(IOchannel)))
	{
	isTTY = TRUE;
	if(!noPromptMode) 	
		varPrintf(OUT_CONSOLE, banner, OSTYPE, banner2);
	}
else
	{
#ifdef WIN_32
	if(!IOchannelIsSocketStream) 
#endif
		setbuf(IOchannel,0);
	if(forcePromptMode)
		varPrintf(OUT_CONSOLE, banner, OSTYPE,  banner2);
	}

/* ======================= main entry on reset ====================== */


errorReg = setjmp(errorJump);

setupAllSignals();
reset();
initStacks();

if(errorReg && !isNil((CELL*)errorEvent->contents) ) 
	executeSymbol(errorEvent, NULL, NULL);


#ifdef READLINE
rl_readline_name = "newlisp";
rl_attempted_completion_function = (CPPFunction *)newlisp_completion;
#endif

while(TRUE)
	{
	cleanupResults(resultStack);
	if(isTTY)  
		{
		cmd = getCommandLine(FALSE);
		executeCommandLine(cmd, OUT_CONSOLE, &cmdStream);
		free(cmd);
		continue;
		}

	if(IOchannel != stdin || forcePromptMode) 
		varPrintf(OUT_CONSOLE, prompt());

	/* demon mode timeout if nothing read after accepting connection */
	if(connectionTimeout && IOchannel && demonMode)
		{
#ifdef WIN_32
		if(IOchannelIsSocketStream)
		  if(wait_ready(getSocket(IOchannel), connectionTimeout, 0) == 0)
#else
		if(wait_ready(fileno(IOchannel), connectionTimeout, 0) == 0)
#endif
			{
			fclose(IOchannel);
			setupServer(1);
			continue;
			}
		}

	if(IOchannel == NULL || fgets(command, MAX_COMMAND_LINE - 1, IOchannel) == NULL)
		{
		if(!demonMode)  exit(1);
		if(IOchannel != NULL) fclose(IOchannel);
		setupServer(1);
		continue;
		}

	executeCommandLine(command, OUT_CONSOLE, &cmdStream);
	}

#ifndef WIN_32
return 0;
#endif
}
#endif

#ifdef READLINE
char * command_generator(char * text, int state)
{
static int list_index, len, clen;
char * name;
     
if (!state)
	{
	list_index = 0;
	len = strlen (text);
	}
     
while((name = primitive[list_index].name))
	{
	list_index++;
     
	if (strncmp (name, text, len) == 0)
		{
		clen = strlen(name) + 1;
		return(strncpy(malloc(clen), name, clen));
		}
	}
     
return ((char *)NULL);
}

#ifdef _BSD
extern char **completion_matches PARAMS((char *, rl_compentry_func_t *));
#else
char ** completion_matches(const char * text, CPFunction commands);
#endif

char ** newlisp_completion (char * text, int start, int end)
{
return(completion_matches(text, (CPFunction *)command_generator));
}
#endif /* READLINE */


char * getCommandLine(int batchMode)
{
char * cmd;

#ifdef READLINE
int errnoSave = errno;
if((cmd = readline(batchMode ? "" : prompt())) == NULL) exit(0);
errno = errnoSave; /* reset errno, set by readline() */
if(strlen(cmd) > 0) 
	add_history(cmd);
#else
if(!batchMode) varPrintf(OUT_CONSOLE, prompt());
cmd = calloc(MAX_COMMAND_LINE, 1);
if(fgets(cmd, MAX_COMMAND_LINE - 1, IOchannel) == NULL) exit(1);
#endif	

return(cmd);
}


void printHelpText(void)
{
varPrintf(OUT_CONSOLE, copyright, 
	"usage: newlisp [file | url ...] [options ...] [file | url ...]\n\noptions:");
varPrintf(OUT_CONSOLE, "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n",
	" -n no init.lsp (must be first)",
	" -h this help",
	" -v version",
	" -s <stacksize>",
	" -m <max-mem-MB> cell memory",
	" -e <quoted lisp expression>",
	" -l <path-file> log connections",
	" -L <path-file> log all",
	" -w <working dir>",
	" -c no prompts, HTTP",
	" -C force prompts",
	" -t <usec-server-timeout>",
	" -p <port-no>",
	" -d <port-no> demon mode",
	" -http only",
	" -6 IPv6 mode",
	"\nmore info at http://newlisp.org");
}


void setupServer(int reconnect)
{
if((IOchannel  = serverFD(IOport,  IOdomain, reconnect)) == NULL)
	{
	printf("newLISP server setup on %s failed.\n", IOdomain);
	exit(1);
	}
#ifdef WIN_32
else	IOchannelIsSocketStream = TRUE; 

if(!IOchannelIsSocketStream)
#endif
	setbuf(IOchannel,0);

if(!reconnect && !noPromptMode)
	varPrintf(OUT_CONSOLE, banner, OSTYPE, ".");
}


char * prompt(void)
{
char * context;
CELL * result;
static char string[64];


if(evalSilent || noPromptMode) 
	{
	evalSilent = 0;
	return("");
	}

if(promptEvent != nilSymbol)
	{
	if(executeSymbol(promptEvent, stuffSymbol(currentContext), &result) == CELL_STRING)
		{
		strncpy(string, (char *)result->contents, 64);
		string[63] = 0;
		deleteList(result);
		return(string);
		}
	deleteList(result);
	}
	
if(currentContext != mainContext)
	context = currentContext->name;
else context = "";

if(traceFlag & TRACE_SIGINT) 
	{
	traceFlag &= ~TRACE_SIGINT;
	longjmp(errorJump, errorReg);
	}
	
if(traceFlag)
	snprintf(string, 63, "%s %d> ", context, recursionCount);
else
	snprintf(string, 63, "%s> ", context);

return(string);
}


void reset()
{
recoverEnvironment(envStack);

while(resultStackIdx > resultStack)
	deleteList(popResult());

envStackIdx = envStack;
lambdaStackIdx = lambdaStack;

freeCellBlocks();

if(printDevice) close((int)printDevice);
printDevice = recursionCount  = traceFlag = prettyPrintFlags = 0;
evalFunc = NULL;
xmlTags = NULL; /* force recreation */
pushResultFlag = TRUE;
currentContext = mainContext;
itSymbol->contents = (UINT)nilCell;
}


void recoverEnvironment(UINT * index)
{
SYMBOL * symbol;
CELL * cell;

while(envStackIdx > index)
	{
	symbol = (SYMBOL *)popEnvironment();
	cell = (CELL*)popEnvironment();
	if(cell != (CELL*)symbol->contents)
		{
		deleteList((CELL*)symbol->contents);
		symbol->contents = (UINT)cell;
		}
	}
}


char * processCommandEvent(char * command);

void executeCommandLine(char * command, UINT outDevice, STREAM * cmdStream)
{
STREAM stream;
char buff[MAX_COMMAND_LINE];
char * cmd;
int batchMode = 0;

if(memcmp(command, "[cmd]", 5) == 0)
	batchMode = 2;
else if(isTTY && (*command == '\n' || *command == '\r' || *command == 0))
	batchMode = 1;

#ifndef LIBRARY
if(!batchMode && commandEvent != nilSymbol)
	command = processCommandEvent(command);
#endif

if(!isTTY && (*command == '\n' || *command == '\r' || *command == 0)) return;

if(!batchMode) 
	{
	if(logTraffic == LOG_MORE) 
		writeLog(command, 0);
	if(strncmp(command, "GET /", 5) == 0) 
		executeHTTPrequest(command + 5, HTTP_GET);
	else if(strncmp(command, "HEAD /", 6) == 0)
		executeHTTPrequest(command + 6, HTTP_HEAD);
	else if(strncmp(command, "PUT /", 5) == 0)
		executeHTTPrequest(command + 5, HTTP_PUT);
	else if(strncmp(command, "POST /", 6) == 0)
		executeHTTPrequest(command + 6, HTTP_POST);
	else if(strncmp(command, "DELETE /", 8) == 0)
		executeHTTPrequest(command + 8, HTTP_DELETE);
	else if(!httpMode) goto EXEC_COMMANDLINE;
	return;
	}

EXEC_COMMANDLINE:
if(noPromptMode == FALSE && *command == '!' && *(command + 1) != ' ' && strlen(command) > 1)
	{
	if(system(command + 1)) return; /* avoid stupid compiler warning */
	return;
	}
	
if(cmdStream != NULL && batchMode)
	{
	openStrStream(cmdStream, 1024, TRUE);
	for(;;)
		{
		if(isTTY) 
			{
			cmd = getCommandLine(TRUE);
			strncpy(buff, cmd, MAX_COMMAND_LINE -1);
#ifdef READLINE
			strlcat(buff, "\n", 1);
#endif
			free(cmd);
			}
		else
			if(fgets(buff, MAX_COMMAND_LINE - 1, IOchannel) == NULL) break;
		if( (memcmp(buff, "[/cmd]", 6) == 0 && batchMode == 2) || 
				(batchMode == 1 && (*buff == '\n' || *buff == '\r' || *buff == 0)))
			{
			if(logTraffic) 
				{
				writeLog(cmdStream->buffer, 0);
				writeLog(buff, 0);
				}
			makeStreamFromString(&stream, cmdStream->buffer);
			evaluateStream(&stream, outDevice, 0);
			return;
			}
		writeStreamStr(cmdStream, buff, 0);
		}
	closeStrStream(cmdStream);
	if(!demonMode)  exit(1);
	if(IOchannel != NULL) fclose(IOchannel);
	setupServer(1);
	return;
	}

if(logTraffic) writeLog(command, TRUE);
prettyPrintLength = 0;

makeStreamFromString(&stream, command);
evaluateStream(&stream, outDevice, 0);
}

char * processCommandEvent(char * command)
{
CELL * result;

if(executeSymbol(commandEvent, stuffString(command), &result) == CELL_STRING)
	{
	pushResult(result);
	command = (char *)result->contents;
	}

return(command);
}

/*
void printResultStack()
{
printf("result stack:\n");
while(resultStackIdx > resultStack)
	{
	--resultStackIdx;
	printCell(*resultStackIdx, TRUE, OUT_CONSOLE);
	printf("\n");
	}
printf("\n");
}
*/

/* used for loadFile() and and executeCommandLine() */
CELL * evaluateStream(STREAM * stream, UINT outDevice, int flag)
{
CELL * program;
CELL * eval = nilCell;
UINT * resultIdxSave;
int result = TRUE;
CELL * xlate;

resultIdxSave = resultStackIdx;
while(result)
	{
	program = getCell(CELL_QUOTE);
	result = compileExpression(stream, program);
	if(readerEvent != nilSymbol && result)
		{
		executeSymbol(readerEvent, program, &xlate);
		program = makeCell(CELL_QUOTE, (UINT)xlate);
		}
	pushResult(program);
	if(result)
		{
		if(flag && eval != nilCell) deleteList(eval);
		eval = evaluateExpression((CELL *)program->contents);
		if(outDevice != 0 && !evalSilent) 
			{
			printCell(eval, TRUE, outDevice);
			varPrintf(outDevice, "\n");
			if(logTraffic == LOG_MORE)
				{
				printCell(eval, TRUE, OUT_LOG);
				writeLog("", TRUE);
				}
			}
		if(flag) eval = copyCell(eval);
		}
	cleanupResults(resultIdxSave);
	}

if(flag) return(eval);
return(NULL);
}


int executeSymbol(SYMBOL * symbol, CELL * params, CELL * * result)
{
CELL * program;
CELL * cell;
UINT * resultIdxSave;

resultIdxSave = resultStackIdx;
if(symbol == nilSymbol || symbol == NULL)   return(0);
pushResult(program = getCell(CELL_EXPRESSION));
cell = makeCell(CELL_SYMBOL, (UINT)symbol);
program->contents = (UINT)cell;
if(params != NULL) cell->next = params;

if(result == NULL)
	{
	evaluateExpression(program);
	cleanupResults(resultIdxSave);
	return(0);
	}

*result = copyCell(evaluateExpression(program));
cleanupResults(resultIdxSave);

return((*result)->type);
}


SYMBOL *  makeStringSymbol(char * name, char * str, int flags)
{
SYMBOL * symbol;

symbol = translateCreateSymbol(name, CELL_STRING, mainContext, TRUE);
symbol->contents = (UINT)stuffString(str);
symbol->flags = flags;

return(symbol);
}

/* -------------------------- initialization -------------------- */

void initialize()
{
int i;
SYMBOL * symbol;
CELL * pCell;
char  symName[8];

/* build true and false cells */
nilCell = getCell(CELL_NIL);
nilCell->aux = nilCell->contents = (UINT)nilCell;
nilCell->next =  nilCell;

trueCell = getCell(CELL_TRUE);
trueCell->contents = (UINT)trueCell;

/* make first symbol, which is context MAIN */
currentContext = createRootContext("MAIN");

/* build symbols for primitives */
for(i = 0; primitive[i].name != NULL; i++)
	{
	pCell = getCell(CELL_PRIMITIVE);
	symbol = translateCreateSymbol(
		primitive[i].name, CELL_PRIMITIVE, mainContext, TRUE);
	symbol->contents = (UINT)pCell;
	symbol->flags = primitive[i].flags | SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
	pCell->contents = (UINT)primitive[i].function;
	pCell->aux = (UINT)symbol->name;
	}

/* build nil, true, * and ? symbols and others  */
nilSymbol = translateCreateSymbol("nil", CELL_NIL, mainContext, TRUE);
nilSymbol->contents = (UINT)nilCell;
trueSymbol = translateCreateSymbol("true", CELL_TRUE, mainContext, TRUE);
trueSymbol->contents = (UINT)trueCell;
starSymbol = translateCreateSymbol("*", CELL_PRIMITIVE, mainContext, TRUE);
plusSymbol = translateCreateSymbol("+", CELL_PRIMITIVE, mainContext, TRUE);
questionSymbol = translateCreateSymbol("?", CELL_NIL, mainContext, TRUE);
atSymbol = translateCreateSymbol("@", CELL_NIL, mainContext, TRUE);
argsSymbol = translateCreateSymbol("$args", CELL_NIL, mainContext, TRUE);
mainArgsSymbol = translateCreateSymbol("$main-args", CELL_NIL, mainContext, TRUE);
dolistIdxSymbol = translateCreateSymbol("$idx", CELL_NIL, mainContext, TRUE);
itSymbol = translateCreateSymbol("$it", CELL_NIL, mainContext, TRUE);

for(i = 0; i < MAX_REGEX_EXP; i++)
	{
	snprintf(symName, 8, "$%d", i);
	sysSymbol[i] = translateCreateSymbol(symName, CELL_NIL, mainContext, TRUE);
	sysSymbol[i]->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN;
	}

currentFunc = errorEvent = timerEvent = promptEvent = commandEvent = transferEvent = readerEvent = nilSymbol;

trueSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
nilSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
questionSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
atSymbol->flags |=  SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
argsSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
mainArgsSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
dolistIdxSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
itSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
argsSymbol->contents = (UINT)getCell(CELL_EXPRESSION);
objSymbol.contents = (UINT)nilCell;
objSymbol.context = mainContext;
objCell = nilCell;

makeStringSymbol("ostype", OSTYPE, SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED);

/* init signal handlers */
for(i = 0; i < 32; i++)
  symHandler[i] = nilSymbol;

/* init readLineStream */
openStrStream(&readLineStream, 16, 0);
openStrStream(&errorStream, MAX_LINE, 0);
}

void initStacks()
{
MAX_ENV_STACK = (MAX_CPU_STACK * 8 * 2);
MAX_RESULT_STACK = (MAX_CPU_STACK * 2);
if(envStack != NULL) freeMemory(envStack);
if(resultStack != NULL) freeMemory(resultStack);
if(lambdaStack != NULL) freeMemory(lambdaStack);
envStackIdx = envStack = (UINT *)allocMemory((MAX_ENV_STACK + 16) * sizeof(UINT));
envStackTop = envStack + MAX_ENV_STACK;
resultStackIdx = resultStack = (UINT *)allocMemory((MAX_RESULT_STACK + 16) * sizeof(UINT));
resultStackTop = resultStack + MAX_RESULT_STACK;
lambdaStackIdx = lambdaStack = (UINT *)allocMemory((MAX_RESULT_STACK + 16) * sizeof(UINT));
}

	
/* ------------------------- evaluate s-expression --------------------- */


CELL * evaluateExpression(CELL * cell)
{
CELL * result;
UINT * resultIdxSave = resultStackIdx;
CELL * args = NULL;
CELL * pCell = NULL;
SYMBOL * newContext = NULL;
SYMBOL * sPtr = NULL;

symbolCheck = NULL;
stringCell = NULL;

switch(cell->type)
	{
	case CELL_NIL:
	case CELL_TRUE:
	/* case CELL_INT: only used with COMPARE_TYPE_MASK */
	case CELL_LONG:
	case CELL_INT64:
	case CELL_FLOAT:
	case CELL_STRING:
	case CELL_PRIMITIVE:
	case CELL_IMPORT_CDECL:
	case CELL_IMPORT_DLL:
	case CELL_LAMBDA:
	case CELL_MACRO:
	case CELL_ARRAY:
		return(cell);

	case CELL_SYMBOL:
	case CELL_CONTEXT:
		symbolCheck = (SYMBOL *)cell->contents;
		return((CELL *)symbolCheck->contents);

	case CELL_QUOTE:
		return((CELL *)cell->contents);

	case CELL_EXPRESSION:
		args = (CELL *)cell->contents;
		if(++recursionCount > (int)MAX_CPU_STACK)
			fatalError(ERR_OUT_OF_CALL_STACK, args, 0);
		
		if(args->type == CELL_SYMBOL) /* precheck for speedup */
			{
			sPtr = (SYMBOL *)args->contents;
			newContext = sPtr->context;
			pCell =  (CELL*)sPtr->contents;
			}
		else if(args->type == CELL_DYN_SYMBOL)
			{
			sPtr = getDynamicSymbol(args);
			newContext = sPtr->context;
			pCell = (CELL *)sPtr->contents;
			}
		else
			{ 
			pCell = evaluateExpression(args);
			newContext = currentContext;
			}

		if(traceFlag) traceEntry(cell, pCell, args);

		/* pCell is evaluated op element */
		if(pCell->type == CELL_PRIMITIVE)
			{
			evalFunc = (CELL *(*)(CELL*))pCell->contents;
			result = evalFunc(args->next);
			evalFunc = NULL;
			break;
			}
		
		if(pCell->type == CELL_LAMBDA)
			{ 
			pushLambda(args);
			result = evaluateLambda((CELL *)pCell->contents, args->next, newContext); 
			--lambdaStackIdx; 
			break; 
			}
		
		if(pCell->type == CELL_MACRO)
			{ 
			pushLambda(args);
			result = evaluateLambdaMacro((CELL *)pCell->contents, args->next, newContext);
			--lambdaStackIdx; 
			break;
			}

		if(pCell->type == CELL_IMPORT_CDECL
#if defined(WIN_32) || defined(CYGWIN)
		   || pCell->type == CELL_IMPORT_DLL
#endif
			)
			{
			result = executeLibfunction(pCell, args->next);  
 			break;
			}

			/* check for 'default' functor
			* allow function call with context name, i.e: (ctx)
			* assumes that a ctx:ctx contains a function
			*/
		if(pCell->type == CELL_CONTEXT)
			{
			newContext = (SYMBOL *)pCell->contents;
			sPtr= translateCreateSymbol(newContext->name, CELL_NIL, newContext, TRUE);
			pCell = (CELL *)sPtr->contents;


			if(isNil(pCell))
				{
				result = evaluateNamespaceHash(args->next, newContext);
				break;
				}

			else if(pCell->type == CELL_PRIMITIVE)
				{
				evalFunc = (CELL *(*)(CELL*))pCell->contents;
				result = evalFunc(args->next);
				evalFunc = NULL;
				break;
				}

			else if(pCell->type == CELL_LAMBDA)
				{
				pushLambda(args);
				result = evaluateLambda((CELL *)pCell->contents, args->next, newContext); 
				--lambdaStackIdx; 
				break; 
				}

			else if(pCell->type  == CELL_MACRO)
				{
				result = evaluateLambdaMacro((CELL *)pCell->contents, args->next, newContext); 
				break; 
				}

			}
			

		/* allow 'implicit indexing' if pCell is a list, array, string or number:
                   (pCell idx1 idx2 ...) 
		*/
                
		if(args->next != nilCell)
			{
			if(pCell->type == CELL_EXPRESSION)
				{
				if(!sPtr) sPtr = symbolCheck;
				result = implicitIndexList(pCell, args->next);
				symbolCheck = sPtr;
				pushResultFlag = FALSE;
				}

			else if(pCell->type == CELL_ARRAY)
				{
				if(!sPtr) sPtr = symbolCheck;
				result = implicitIndexArray(pCell, args->next);
				symbolCheck = sPtr;
				pushResultFlag = FALSE;
				}	
				
			else if(pCell->type == CELL_STRING)
				{
				if(sPtr || (sPtr = symbolCheck))
					{
					/* implicitIndexString() always returns copy */
					result = implicitIndexString(pCell, args->next);
					pushResult(result);
					symbolCheck = sPtr;
					--recursionCount;
					return(result);
					}
				else
					result = implicitIndexString(pCell, args->next); 
				}
                                             
			else if(isNumber(pCell->type))
				result = implicitNrestSlice(pCell, args->next);
	
			else result = errorProcExt(ERR_INVALID_FUNCTION, cell);                              
			}
		else 
			result = errorProcExt(ERR_INVALID_FUNCTION, cell);
		break;

	case CELL_DYN_SYMBOL:
		symbolCheck = getDynamicSymbol(cell);
		return((CELL *)symbolCheck->contents);
		
	default:
		result = nilCell;
	}

if(pushResultFlag) 
	{
	if(resultStackIdx > resultStackTop)
		fatalError(ERR_OUT_OF_CALL_STACK, pCell, 0);

	while(resultStackIdx > resultIdxSave)
		deleteList(popResult());

	pushResult(result);
	}
else 
	pushResultFlag = TRUE;

if(traceFlag) traceExit(result, cell, pCell, args);
--recursionCount;
return(result);
}


CELL *  evaluateExpressionSafe(CELL * cell, int * errNo)
{
jmp_buf errorJumpSave;
CELL * result;

memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
if((*errNo = setjmp(errorJump)) != 0)
	{
	memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
	return(NULL);
	}

result = evaluateExpression(cell);
memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
return(result);
}


CELL * evaluateNamespaceHash(CELL * args, SYMBOL * newContext)
{	
SYMBOL * sPtr;
CELL * pCell;

pCell = evaluateExpression(args);
if(pCell->type == CELL_STRING)
	{
	/* sent contents */
	if(args->next != nilCell)
		{
		sPtr = makeSafeSymbol(pCell, newContext, TRUE);
		
		itSymbol->contents = sPtr->contents;
		itSymbol->contents = (UINT)copyCell(evaluateExpression(args->next));
		deleteList((CELL *)sPtr->contents);
		sPtr->contents = itSymbol->contents;
		itSymbol->contents = (UINT)nilCell;
		
		if(isNil((CELL *)sPtr->contents))
			{
			deleteFreeSymbol(sPtr, FALSE);
			return(nilCell);
			}

		symbolCheck = sPtr;
		pushResultFlag = FALSE;
		return((CELL *)sPtr->contents);
		}
	/* get contents */
	else 
		{
		sPtr = makeSafeSymbol(pCell, newContext, FALSE);
		if(sPtr == NULL)
			return(nilCell);
		else
			{
			symbolCheck = sPtr;
			pushResultFlag = FALSE;
			return((CELL *)sPtr->contents);
			}
		}
	}
/* create Tree from association list */
else if(pCell->type == CELL_EXPRESSION)
	{
	args = (CELL *)pCell->contents;
	while(args->type == CELL_EXPRESSION)
		{
		pCell = (CELL *)args->contents;
		if(pCell->type == CELL_STRING)
			{
			sPtr = makeSafeSymbol(pCell, newContext, TRUE);
			deleteList((CELL *)sPtr->contents);
			sPtr->contents = (UINT)copyCell(pCell->next);
			}
		args = args->next;
		}
	return(stuffSymbol(newContext));
	}
/* return association list */
else if(pCell->type == CELL_NIL)
	{
	return(associationsFromTree(newContext));
	}

return(errorProcExt(ERR_INVALID_PARAMETER, pCell));
}


/* a symbol belonging to a dynamic context
   the parent context symbol points to the real context  
   cell->contents -> name str of this symbol
   cell->aux -> symbol var which holds context (dynamic) 
   ((SYMBOL*)cell->aux)->contents -> context cell
 */
SYMBOL * getDynamicSymbol(CELL * cell)
{
CELL * contextCell;

contextCell = (CELL *)((SYMBOL *)cell->aux)->contents;
if(contextCell->type != CELL_CONTEXT)
	fatalError(ERR_CONTEXT_EXPECTED, stuffSymbol((SYMBOL*)cell->aux), TRUE);

return(translateCreateSymbol( 
		(char*)cell->contents, /* name of dyn symbol */
		CELL_NIL,
		(SYMBOL*)contextCell->contents,	/* contextPtr */
		TRUE));
}


void cleanupResults(UINT * from)
{
while(resultStackIdx > from)
	deleteList(popResult());
}

/* -------------------- evaluate lambda function ----------------------- */

CELL * evaluateLambda(CELL * localLst, CELL * arg, SYMBOL * newContext)
{
CELL * local;
CELL * result = nilCell;
CELL * cell;
SYMBOL * symbol;
SYMBOL * contextSave;
UINT * resultIdxSave;
int localCount = 1; /* 1 for $args */

if(envStackIdx > envStackTop)
	return(errorProc(ERR_OUT_OF_ENV_STACK));

if(localLst->type != CELL_EXPRESSION)
	return(errorProcExt(ERR_INVALID_LAMBDA, localLst));

/* evaluate arguments */
if(arg != nilCell)
	{
	/* this symbol precheck does 10% speed improvement on lambdas  */
	if(arg->type == CELL_SYMBOL)
		cell = result = copyCell((CELL*)((SYMBOL *)arg->contents)->contents);
	else
		cell = result = copyCell(evaluateExpression(arg));
       
	while((arg = arg->next) != nilCell)
		{
		if(arg->type == CELL_SYMBOL)
			cell = cell->next = copyCell((CELL*)((SYMBOL *)arg->contents)->contents);
		else
			cell = cell->next = copyCell(evaluateExpression(arg));
		}
	}

/* change to new context */
contextSave = currentContext;
currentContext = newContext;

/* save environment and get parameters */
local = (CELL*)localLst->contents;
for(;;)
	{
	if(local->type == CELL_SYMBOL)
		symbol = (SYMBOL *)local->contents;
	/* get default parameters */
	else if(local->type == CELL_EXPRESSION)
		{
		if(((CELL*)local->contents)->type == CELL_SYMBOL)
			{
			cell = (CELL *)local->contents;
			if(cell->type == CELL_SYMBOL)
				{
				symbol = (SYMBOL *)cell->contents;
				if(result == nilCell)
					result = copyCell(evaluateExpression(cell->next));
				}
			else break;
			}
		else break;
		}
	else break;

	if(isProtected(symbol->flags))
		return(errorProcExt(ERR_SYMBOL_PROTECTED, local));

	/* save symbol environment */
	pushEnvironment(symbol->contents);
	pushEnvironment((UINT)symbol);

	/* fill local symbols */
	if(result == nilCell) result = copyCell(nilCell);
	symbol->contents = (UINT)result;

	cell = result;
	result = result->next;

	/* unlink list */
	cell->next = nilCell;

	local = local->next;
	localCount++;
	}

/* put unassigned args in  protected $args */
pushEnvironment(argsSymbol->contents);
pushEnvironment((UINT)argsSymbol);
argsSymbol->contents = (UINT)makeCell(CELL_EXPRESSION, (UINT)result);

/* get contents for (self), is nil if no ancestor caller is colon : */
objSymbol.contents = (UINT)objCell;

#ifdef FOOP_DEBUG
printf("objCell in lambda:");
printCell(objCell, TRUE, OUT_CONSOLE);
printf(" context:%s\n", currentContext->name);
#endif

/* evaluate body expressions */
resultIdxSave = resultStackIdx;
result = nilCell;
while( (localLst = localLst->next) != nilCell)
	{
	while(resultStackIdx > resultIdxSave)
		deleteList(popResult());
	result = evaluateExpression(localLst);
	}
result = copyCell(result);

/* restore symbols used as locals */
while(localCount--)
	{
	symbol = (SYMBOL *)popEnvironment();
	deleteList((CELL *)symbol->contents);
	symbol->contents = popEnvironment();
	}

currentContext = contextSave;
symbolCheck = NULL;
stringCell = NULL;
return(result);
}


CELL * evaluateLambdaMacro(CELL * localLst, CELL * arg, SYMBOL * newContext)
{
CELL * local;
CELL * result = nilCell;
CELL * cell;
SYMBOL * symbol;
SYMBOL * contextSave;
UINT * resultIdxSave;
int localCount = 1; /* for $args */

if(envStackIdx > envStackTop)
	return(errorProc(ERR_OUT_OF_ENV_STACK));

if(localLst->type != CELL_EXPRESSION)
	return(errorProcExt(ERR_INVALID_MACRO, localLst));

local = (CELL *)localLst->contents;

contextSave = currentContext;
currentContext = newContext;

/* save environment and get parameters */
GET_ARGS:
  {
  if(local->type == CELL_SYMBOL)
  	symbol = (SYMBOL *)local->contents;
  /* get default parameters */
  else if(local->type == CELL_EXPRESSION)
		{
		if(((CELL*)local->contents)->type == CELL_SYMBOL)
			{
			cell = (CELL *)local->contents;
			if(cell->type == CELL_SYMBOL)
				{
				symbol = (SYMBOL *)cell->contents;
				if(arg == nilCell)
					arg = evaluateExpression(cell->next);
				}
			else goto GOT_ARGS;
			}
		else goto GOT_ARGS;
		}
  else goto GOT_ARGS;

  if(isProtected(symbol->flags))
    return(errorProcExt(ERR_SYMBOL_PROTECTED, local));

  pushEnvironment(symbol->contents);
  pushEnvironment((UINT)symbol);
  symbol->contents = (UINT)copyCell(arg);
  local = local->next;
  arg = arg->next;
  localCount++;
  }
goto GET_ARGS;

GOT_ARGS:

/* put unassigned args in $args */
pushEnvironment(argsSymbol->contents);
pushEnvironment((UINT)argsSymbol);
argsSymbol->contents = (UINT)makeCell(CELL_EXPRESSION, (UINT)copyList(arg));

/* evaluate body expressions */
resultIdxSave = resultStackIdx;
while((localLst = localLst->next) != nilCell)
	{
	while(resultStackIdx > resultIdxSave)
		deleteList(popResult());
	result = evaluateExpression(localLst);
	}
result = copyCell(result);

/* restore symbols used as locals */
while(localCount--)
    {
	symbol = (SYMBOL *)popEnvironment();
	deleteList((CELL *)symbol->contents);
	symbol->contents = popEnvironment();
    }

currentContext = contextSave;	
symbolCheck = NULL;
stringCell = NULL;
return(result);
}


/* -------------- list/cell creation/deletion routines ---------------- */


CELL * stuffInteger(UINT contents)
{
CELL * cell;

if(firstFreeCell == NULL) allocBlock();
cell = firstFreeCell;
firstFreeCell = cell->next;
++cellCount;

cell->type = CELL_LONG;
cell->next = nilCell;
cell->aux = (UINT)nilCell;
cell->contents = contents;

return(cell);
}

#ifndef NEWLISP64
CELL * stuffInteger64(INT64 contents)
{
CELL * cell;

if(firstFreeCell == NULL) allocBlock();
cell = firstFreeCell;
firstFreeCell = cell->next;
++cellCount;

cell->type = CELL_INT64;
cell->next = nilCell;

*(INT64 *)&cell->aux = contents;
return(cell);
}
#endif

CELL * stuffIntegerList(int argc, ...)
{
CELL * cell;
CELL * list;
va_list ap;

va_start(ap, argc);

list = makeCell(CELL_EXPRESSION, (UINT)stuffInteger(va_arg(ap, UINT)));
cell = (CELL *)list->contents;

while(--argc)
	{
	cell->next = stuffInteger(va_arg(ap, UINT));
	cell = cell->next;
	}
va_end(ap);

return(list);
}

/* only safe for text content */
CELL * stuffString(char * string)
{
CELL * cell;

cell = getCell(CELL_STRING);
cell->aux = strlen(string) + 1;
cell->contents = (UINT)allocMemory((UINT)cell->aux);

memcpy((void *)cell->contents, string, (UINT)cell->aux);
return(cell);
}

/* safe for binary content */
CELL * stuffStringN(char * string, int len)
{
CELL * cell;

cell = getCell(CELL_STRING);
cell->aux = len + 1;
cell->contents = (UINT)allocMemory((UINT)cell->aux);
memcpy((void *)cell->contents, string, len);
*(char*)(cell->contents + len) = 0;
return(cell);
}

CELL * stuffFloat(double * floatPtr)
{
CELL * cell;

cell = getCell(CELL_FLOAT);
#ifndef NEWLISP64
*(double *)&cell->aux = *floatPtr;
#else
*(double *)&cell->contents = *floatPtr;
#endif
return(cell);
}

CELL * stuffSymbol(SYMBOL * sPtr)
{
CELL * cell;

cell = getCell(CELL_SYMBOL);
cell->contents = (UINT)sPtr;
return(cell);
}

/* appends to a list, the list must have be either optimized
   with list->aux pointing to the last cell, or list->aux must
   contain nilCell and be empty
*/

void addList(CELL * list, CELL * new)
{
if(list->aux == (UINT)nilCell)
	list->contents = (UINT)new;
else
	((CELL *)list->aux)->next = new;
list->aux = (UINT)new;
}

ssize_t convertNegativeOffset(ssize_t offset, CELL * list)
{
int len=0;

while(list != nilCell)
	{
	++len;
	list = list->next;
	}
offset = len + offset;
if(offset < 0) 
	errorProc(ERR_LIST_INDEX_OUTOF_BOUNDS);
return(offset);
}

/* ------------------------ creating and freeing cells ------------------- */

CELL * getCell(int type)
{
CELL * cell;

if(firstFreeCell == NULL) allocBlock();
cell = firstFreeCell;
firstFreeCell = cell->next;
++cellCount;

cell->type = type;
cell->next = nilCell;
cell->aux = (UINT)nilCell;
cell->contents = (UINT)nilCell;

return(cell);
}


CELL * makeCell(int type, UINT contents)
{
CELL * cell;

if(firstFreeCell == NULL) allocBlock();
cell = firstFreeCell;
firstFreeCell = cell->next;
++cellCount;

cell->type = type;
cell->next = nilCell;
cell->aux = (UINT)nilCell;
cell->contents = contents;

return(cell);
}


CELL * makeStringCell(char * contents, size_t size)
{
CELL * cell;

if(firstFreeCell == NULL) allocBlock();
cell = firstFreeCell;
firstFreeCell = cell->next;
++cellCount;

cell->type = CELL_STRING;
cell->next = nilCell;
cell->aux = (UINT)size + 1;
cell->contents = (UINT)contents;

return(cell);
}


CELL * copyCell(CELL * cell)
{
CELL * newCell;
CELL * list;
UINT len;

if(firstFreeCell == NULL) allocBlock();
newCell = firstFreeCell;
firstFreeCell = newCell->next;
++cellCount;

newCell->type = cell->type;
newCell->next = nilCell;
newCell->aux = cell->aux;
newCell->contents = cell->contents;

if(isEnvelope(cell->type))
	{
	if(cell->type == CELL_ARRAY)
		newCell->contents = (UINT)copyArray(cell);
	else /* normal list expression */
		{
		if(cell->contents != (UINT)nilCell) 
			{
			newCell->contents = (UINT)copyCell((CELL *)cell->contents);
			list = (CELL *)newCell->contents;
			cell = (CELL *)cell->contents;
			while((cell = cell->next) != nilCell)
				list = list->next = copyCell(cell);
			newCell->aux = (UINT)list;  /* last element optimization */
			}
		}
	}
else if(cell->type == CELL_STRING)
	{
	newCell->contents = (UINT)allocMemory((UINT)cell->aux);
	memcpy((void *)newCell->contents, (void*)cell->contents, (UINT)cell->aux);
	}
else if(cell->type == CELL_DYN_SYMBOL)
	{
	len = strlen((char *)cell->contents);
	newCell->contents = (UINT)allocMemory(len + 1);
	memcpy((char *)newCell->contents, (char *)cell->contents, len + 1);
	}

return(newCell);
}


/* this routine must be called with the list head
   if copying with envelope call copyCell() instead */
CELL * copyList(CELL * cell)
{
CELL * firstCell;
CELL * newCell;

if(cell == nilCell) 
	{
	lastCellCopied = nilCell;
	return(cell);
	}

firstCell = newCell = copyCell(cell);

while((cell = cell->next) != nilCell)
	newCell = newCell->next = copyCell(cell);
	
lastCellCopied = newCell;
return(firstCell);
}


/* for deleting lists _and_ cells */
void deleteList(CELL * cell)
{
CELL * next;

while(cell != nilCell)
	{
	if(isEnvelope(cell->type))
		{
		if(cell->type == CELL_ARRAY)
			deleteArray(cell);
		else
			deleteList((CELL *)cell->contents);
		}

	else if(cell->type == CELL_STRING || cell->type == CELL_DYN_SYMBOL) 
		freeMemory( (void *)cell->contents);

	next = cell->next;
	
	/* free cell */
	if(cell != trueCell) 
		{
		cell->type = CELL_FREE;
		cell->next = firstFreeCell;
		firstFreeCell = cell;
		--cellCount;
		}
	
	cell = next;
	}
}

/* --------------- cell / memory allocation and deallocation -------------

   allthough (MAC_BLOCK + 1) are allocated only MAX_BLOCK cells
   are used. The last cell only serves as a pointer to the next block
*/

CELL * cellMemory = NULL;/* start of cell memory */
CELL * cellBlock = NULL; /* the last block allocated */

void allocBlock()
{
int i;

if(cellCount > MAX_CELL_COUNT) fatalError(ERR_NOT_ENOUGH_MEMORY, NULL, 0);

if(cellMemory == NULL)
	{
	cellMemory = (CELL *)allocMemory((MAX_BLOCK + 1) * sizeof(CELL));
	cellBlock = cellMemory;
	}
else
	{
	(cellBlock + MAX_BLOCK)->next = 
		(CELL *)allocMemory((MAX_BLOCK + 1) * sizeof(CELL));
	cellBlock = (cellBlock + MAX_BLOCK)->next;
	}

for(i = 0; i < MAX_BLOCK; i++)
	{
	(cellBlock + i)->type = CELL_FREE;
	(cellBlock + i)->next = (cellBlock + i + 1);
	}
(cellBlock + MAX_BLOCK - 1)->next = NULL;
(cellBlock + MAX_BLOCK)->next = NULL;
firstFreeCell = cellBlock;
}


/* Return unused blocks to OS, this is normally only called under error 
   conditions but can also be forced issuing a (reset nil)

   Older versions also did a complete cell mark and sweep. Now all
   error conditons clean out allocated cells and memory before doing
   the longjmp().
*/

void freeCellBlocks()
{
CELL * blockPtr;
CELL * lastBlockPtr = NULL;
CELL * lastFreeCell = NULL;
CELL * prevLastFreeCell;
CELL * prevCellBlock;
int i, freeCount;

cellBlock = blockPtr = cellMemory;
firstFreeCell = NULL;
while(blockPtr != NULL)
	{
	prevLastFreeCell = lastFreeCell;
	prevCellBlock = cellBlock;
	cellBlock = blockPtr;
	for(i = freeCount = 0; i < MAX_BLOCK; i++)
		{
		if(*(UINT *)blockPtr == CELL_FREE)
			{
			if(firstFreeCell == NULL)
				firstFreeCell = lastFreeCell = blockPtr;
			else
				{
				lastFreeCell->next = blockPtr;
				lastFreeCell = blockPtr;
				}
			freeCount++;
			}
		blockPtr++;
		}
	if(freeCount == MAX_BLOCK)
		{
		lastFreeCell = prevLastFreeCell;
		cellBlock = prevCellBlock;
		blockPtr = blockPtr->next;
		freeMemory(lastBlockPtr->next);
		lastBlockPtr->next = blockPtr;
		}
	else 
		{
		lastBlockPtr = blockPtr;
		blockPtr = blockPtr->next;
		}
	}
lastFreeCell->next = NULL;
}


/* OS memory allocation */

void * allocMemory(size_t nbytes)
{
void * ptr;

if( (ptr = (void *)malloc(nbytes)) == NULL)
	fatalError(ERR_NOT_ENOUGH_MEMORY, NULL, 0);

return(ptr);
}

void * callocMemory(size_t nbytes)
{
void * ptr;

if( (ptr = (void *)calloc(nbytes, 1)) == NULL)
	fatalError(ERR_NOT_ENOUGH_MEMORY, NULL, 0);

return(ptr);
}

void * reallocMemory(void * prevPtr, UINT size)
{
void * ptr;

if( (ptr = realloc(prevPtr, size)) == NULL)
	fatalError(ERR_NOT_ENOUGH_MEMORY, NULL, 0);

return(ptr);
}

/* -------------------------- I/O routines ------------------------------ */

UINT printDevice;
void prettyPrint(UINT device);

void varPrintf(UINT device, char * format, ...)
{
char * buffer;
va_list argptr;
 
va_start(argptr,format);

/* defined in nl-filesys.c if not in libc */
vasprintf(&buffer, format, argptr); 

prettyPrintLength += strlen(buffer);
switch(device)
	{
	case OUT_NULL:
		return;

	case OUT_DEVICE:
		if(printDevice != 0)
			{
			if(write(printDevice, buffer, strlen(buffer)) < 0)
				fatalError(ERR_IO_ERROR, 0, 0);
			break;
			}
	case OUT_CONSOLE:
#ifdef LIBRARY
		writeStreamStr(&libStrStream, buffer, 0);
		return;
#else
		if(IOchannel == stdin)
			{
			printf("%s", buffer);
			if(!isTTY) fflush(NULL);
			}
		else if(IOchannel != NULL) 
			fprintf(IOchannel, "%s", buffer);
		break;
#endif
	case OUT_LOG:
		writeLog(buffer, 0);
		break;

	default:
		writeStreamStr((STREAM *)device, buffer, 0);
		break;
	}

freeMemory(buffer);

va_end(argptr);
}


void printCell(CELL * cell, UINT printFlag, UINT device)
{
SYMBOL * sPtr;
SYMBOL * sp;

switch(cell->type)
	{
	case CELL_NIL:
		varPrintf(device, "nil"); break;

	case CELL_TRUE:
		varPrintf(device, "true"); break;
	
	case CELL_LONG:
		varPrintf(device,"%ld", cell->contents); break;

#ifndef NEWLISP64
	case CELL_INT64:
#ifdef TRU64
		varPrintf(device,"%ld", *(INT64 *)&cell->aux); break;
#else
#ifdef WIN_32
		varPrintf(device,"%I64d", *(INT64 *)&cell->aux); break;
#else
		varPrintf(device,"%lld", *(INT64 *)&cell->aux); break;
#endif /* WIN32 */
#endif /* TRU64 */
#endif /* NEWLISP64 */
	case CELL_FLOAT:
#ifndef NEWLISP64
		varPrintf(device, prettyPrintFloat ,*(double *)&cell->aux);
#else
		varPrintf(device, prettyPrintFloat ,*(double *)&cell->contents);
#endif
		break;

	case CELL_STRING:
		if(printFlag)
			printString((char *)cell->contents, device, cell->aux - 1);
		else
			varPrintf(device,"%s",cell->contents);
		break;
	
	case CELL_SYMBOL:
	case CELL_CONTEXT:
		sPtr = (SYMBOL *)cell->contents;
		if(sPtr->context != currentContext  
			/* if not global or global overwritten in current context */
			&& (!(sPtr->flags & SYMBOL_GLOBAL) || (lookupSymbol(sPtr->name, currentContext)))
			&& (symbolType(sPtr) != CELL_CONTEXT || 
				(SYMBOL *)((CELL*)sPtr->contents)->contents != sPtr)) /* context var */
			{
			varPrintf(device,"%s:%s", (char*)((SYMBOL*)sPtr->context)->name, sPtr->name);
			break;
			}
		/* overwriting global in MAIN */
		if(sPtr->context == currentContext
			&& currentContext != mainContext
			&& ((sp = lookupSymbol(sPtr->name, mainContext)) != NULL)
			&& (sp->flags & SYMBOL_GLOBAL) )
			{
			varPrintf(device,"%s:%s", currentContext->name, sPtr->name);
			break;
			}

		varPrintf(device,"%s",sPtr->name);
		break;
	
	case CELL_PRIMITIVE:
	case CELL_IMPORT_CDECL:
#if defined(WIN_32) || defined(CYGWIN)
	case CELL_IMPORT_DLL:
#endif
		varPrintf(device,"%s<%lX>", (char *)cell->aux, cell->contents);
		break;
	
	case CELL_QUOTE:
		varPrintf(device, "'");
		prettyPrintFlags |= PRETTYPRINT_DOUBLE;
		printCell((CELL *)cell->contents, printFlag, device);
		break;
	
	case CELL_EXPRESSION:
	case CELL_LAMBDA:
	case CELL_MACRO:
		printExpression(cell, device);
		break;

	case CELL_DYN_SYMBOL:
		varPrintf(device, "%s:%s", ((SYMBOL*)cell->aux)->name, (char*)cell->contents);
		break;                                                                                                                                                                             
	case CELL_ARRAY:
		printArray(cell, device);
		break;

	default:
		varPrintf(device,"?");
	}

prettyPrintFlags &= ~PRETTYPRINT_DOUBLE;
}


void printString(char * str, UINT  device, int size)
{
char chr;

if(size >= MAX_STRING)
    {
    varPrintf(device, "[text]");
    while(size--) varPrintf(device, "%c", *str++);
    varPrintf(device, "[/text]");
    return;
    }

varPrintf(device,"\"");
while(size--)
	{
	switch(chr = *str++)
		{
		case '\n': varPrintf(device,"\\n"); break;
		case '\r': varPrintf(device,"\\r"); break;
		case '\t': varPrintf(device,"\\t"); break;
		case '\\': varPrintf(device,"\\\\"); break;
		case '"': varPrintf(device,"\\%c",'"'); break;
		default: 
			if((unsigned char)chr < 32 || (stringOutputRaw && (unsigned char)chr > 126))
                            varPrintf(device,"\\%03u", (unsigned char)chr);
                        else
			    varPrintf(device,"%c",chr); break;
		}
	}
varPrintf(device,"\"");
}


void printExpression(CELL * cell, UINT device)
{
CELL * item;
int i, pFlags;

item = (CELL *)cell->contents;


if(prettyPrintPars <= prettyPrintCurrent || 
	prettyPrintLength > prettyPrintMaxLength)
	prettyPrint(device);

if(cell->type == CELL_LAMBDA) 
	{
	varPrintf(device, "(lambda ");
	++prettyPrintPars;
	}
else if(cell->type == CELL_MACRO) 
	{
	varPrintf(device, "(lambda-macro ");
	++prettyPrintPars;
	}
else 
	{
	if(isSymbol(item->type))
		{
		if(item->type == CELL_SYMBOL)
			 pFlags = ((SYMBOL *)item->contents)->flags;
		else
			 pFlags = 0;

		if((pFlags & PRINT_TYPE_MASK) != 0)
			{
			prettyPrint(device);
			varPrintf(device, "(");
			++prettyPrintPars;
			for(i = 0; i < (pFlags & PRINT_TYPE_MASK); i++)
				{
				if(item == nilCell) 
					{prettyPrintFlags |= PRETTYPRINT_DOUBLE; break;}
				printCell(item, TRUE, device);
				item = item->next;
				if(item != nilCell) varPrintf(device," ");
				else prettyPrintFlags |= PRETTYPRINT_DOUBLE;
				}
			prettyPrint(device);
			}
		else 
			{
			varPrintf(device, "(");
			++prettyPrintPars;
			}
		}
	else 
		{
		varPrintf(device, "(");
		++prettyPrintPars;
		}
	}


while(item != nilCell)
	{
	if(prettyPrintLength > prettyPrintMaxLength) prettyPrint(device);
	printCell(item, TRUE, device);
	item = item->next;
	if(item != nilCell) varPrintf(device," ");
	}

varPrintf(device,")");
--prettyPrintPars;
}


void prettyPrint(UINT device)
{
int i;

if(prettyPrintFlags) return;

if(prettyPrintPars > 0) 
	varPrintf(device, LINE_FEED);
/* varPrintf(device, LINE_FEED);  before 7106 */

for(i = 0; i < prettyPrintPars; i++) 
	varPrintf(device, prettyPrintTab);
prettyPrintLength = prettyPrintCurrent = prettyPrintPars;
prettyPrintFlags |= PRETTYPRINT_DOUBLE;
}


void printSymbol(SYMBOL * sPtr, UINT device)
{
CELL * cell;
CELL * list = NULL;
char * setStr;

prettyPrintCurrent = prettyPrintPars = 1;
prettyPrintLength = 0;
prettyPrintFlags &= !PRETTYPRINT_DOUBLE;

if(sPtr->flags & SYMBOL_PROTECTED)
	setStr = "(constant ";
else
	setStr = "(set ";

switch(symbolType(sPtr))
	{
	case CELL_PRIMITIVE:
	case CELL_IMPORT_CDECL:
#if defined(WIN_32) || defined(CYGWIN) 
	case CELL_IMPORT_DLL:
#endif
		break;
	case CELL_SYMBOL:
	case CELL_DYN_SYMBOL:
		varPrintf(device, setStr);
		printSymbolNameExt(device, sPtr);
		varPrintf(device,"'");
		printCell((CELL *)sPtr->contents, TRUE, device);
		varPrintf(device, ")");
		break;
	case CELL_ARRAY:
	case CELL_EXPRESSION:
		varPrintf(device, setStr);
		printSymbolNameExt(device, sPtr);
		cell = (CELL *)sPtr->contents;

		if(symbolType(sPtr) == CELL_ARRAY)
			{
			varPrintf(device, "(array ");
			printArrayDimensions(cell, device);
			varPrintf(device, "(flat ");
			list = cell = arrayList(cell);
			}

		cell = (CELL *)cell->contents;

		varPrintf(device,"'(");
		prettyPrintPars = 2;
		if(cell->type == CELL_EXPRESSION) prettyPrint(device);
		while(cell != nilCell)
			{
			if(prettyPrintLength > prettyPrintMaxLength) 
					prettyPrint(device);
			printCell(cell, TRUE, device);
			cell = cell->next;
			if(cell != nilCell) varPrintf(device, " ");
			}
		varPrintf(device, "))");
		if(symbolType(sPtr) == CELL_ARRAY)
			{
			deleteList(list);
			varPrintf(device ,"))");
			}
		break;
	case CELL_LAMBDA:
	case CELL_MACRO:
		if(isProtected(sPtr->flags))
			{
			varPrintf(device, "%s%s%s", LINE_FEED, LINE_FEED, setStr);
			printSymbolNameExt(device, sPtr);
			printExpression((CELL *)sPtr->contents, device);
			varPrintf(device, ")");
			}
		else if (isGlobal(sPtr->flags))
			{
			printLambda(sPtr, device);
			varPrintf(device, "%s%s", LINE_FEED, LINE_FEED);
			printSymbolNameExt(device, sPtr);
			}
		else printLambda(sPtr, device);
		break;
	default:
		varPrintf(device, setStr);
		printSymbolNameExt(device, sPtr);
		printCell((CELL *)sPtr->contents, TRUE, device);
		varPrintf(device, ")");
		break;
	}

varPrintf(device, "%s%s", LINE_FEED, LINE_FEED);

prettyPrintLength = prettyPrintPars = 0;
}


void printLambda(SYMBOL * sPtr, UINT device)
{
CELL * lambda;
CELL * cell;

lambda = (CELL *)sPtr->contents;
cell = (CELL *)lambda->contents;
if(cell->type == CELL_EXPRESSION)
	cell = (CELL *)cell->contents;

if(!isLegalSymbol(sPtr->name))
        {
        varPrintf(device, "(set (sym ");
        printString(sPtr->name, device, strlen(sPtr->name));
        varPrintf(device, " %s) ", ((SYMBOL*)sPtr->context)->name);
        printExpression((CELL *)sPtr->contents, device);
        varPrintf(device, ")");
        return;
        }
	
if(symbolType(sPtr) == CELL_LAMBDA)
	varPrintf(device, "(define (");
else 
	varPrintf(device, "(define-macro (");
prettyPrintPars += 2;

printSymbolName(device, sPtr);
varPrintf(device, " ");

while(cell != nilCell)
	{
	printCell(cell, TRUE, device);
	cell = cell->next;
	if(cell != nilCell) varPrintf(device, " ");
	}
varPrintf(device, ")");
--prettyPrintPars;
prettyPrint(device);

cell = (CELL *)lambda->contents;
while((cell = cell->next) != nilCell)
	{
	if(prettyPrintLength > prettyPrintMaxLength) prettyPrint(device);
	printCell(cell, TRUE, device);
	if(!(cell->type & ENVELOPE_TYPE_MASK) && cell->next != nilCell) varPrintf(device, " ");
	}

varPrintf(device, ")");
--prettyPrintPars;
}


void printSymbolName(UINT device, SYMBOL * sPtr)
{
SYMBOL * sp;

if(sPtr->context == currentContext)
	{
	if(*sPtr->name == *currentContext->name && strcmp(sPtr->name, currentContext->name) == 0)
		varPrintf(device, "%s:%s", sPtr->name, sPtr->name);

	else if(currentContext != mainContext 
		&& ((sp = lookupSymbol(sPtr->name, mainContext)) != NULL)
		&& (sp->flags &  SYMBOL_GLOBAL) )
		varPrintf(device, "%s:%s", currentContext->name, sPtr->name);
	else
		varPrintf(device,"%s", sPtr->name);
	}
else
	varPrintf(device,"%s:%s", 
		(char *)((SYMBOL*)sPtr->context)->name, sPtr->name);
}


void printSymbolNameExt(UINT device, SYMBOL * sPtr)
{
if(isGlobal(sPtr->flags))
	{
	varPrintf(device, "(global '");
	printSymbolName(device, sPtr);
	if(symbolType(sPtr) == CELL_LAMBDA || symbolType(sPtr) == CELL_MACRO)
		varPrintf(device, ")");
	else varPrintf(device, ") ");
	}
else 
	{
	if(!isLegalSymbol(sPtr->name))
		{
		varPrintf(device, " (sym ");
		printString(sPtr->name, device, strlen(sPtr->name));
		varPrintf(device, " MAIN:%s) ", ((SYMBOL*)sPtr->context)->name);
		}
	else
		{
		varPrintf(device, "'");
		printSymbolName(device, sPtr);
		}
	varPrintf(device, " ");
	}
}


CELL * p_prettyPrint(CELL * params)
{
CELL * result;
char * str;
size_t len;

if(params != nilCell)
	params = getInteger(params, &prettyPrintMaxLength);
if(params != nilCell)
	{
	params = getStringSize(params, &str, &len, TRUE);
	prettyPrintTab = allocMemory(len + 1);
	memcpy(prettyPrintTab, str, len + 1);
	}
if(params != nilCell)
	{
	getStringSize(params, &str, &len, TRUE);
	prettyPrintFloat = allocMemory(len + 1);
	memcpy(prettyPrintFloat, str, len + 1);
	}

result = getCell(CELL_EXPRESSION);
addList(result, stuffInteger(prettyPrintMaxLength));
addList(result, stuffString(prettyPrintTab));
addList(result, stuffString(prettyPrintFloat));

return(result);
}



/* -------------------------- error handling --------------------------- */

char * errorMessage[] =
	{
	"",                             /* 0 */
	"not enough memory",            /* 1 */
	"environment stack overflow",   /* 2 */
	"call or result stack overflow",/* 3 */
	"problem accessing file",       /* 4 */
	"illegal token or expression",  /* 5 */
	"missing parenthesis",          /* 6 */
	"string token too long",        /* 7 */
	"missing argument",             /* 8 */
	"number or string expected",    /* 9 */
	"value expected",               /* 10 */
	"string expected",              /* 11 */
	"symbol expected",              /* 12 */
	"context expected",             /* 13 */
	"symbol or context expected",   /* 14 */
	"list expected",                /* 15 */
	"list or array expected",       /* 15 */
	"list or symbol expected",      /* 17 */
	"list or string expected",      /* 18 */
	"list or number expected",      /* 19 */
	"array expected",               /* 20 */
	"array, list or string expected", /* 21 */
	"lambda expected",              /* 22 */
	"lambda-macro expected",        /* 23 */
	"invalid function",             /* 24 */
	"invalid lambda expression",    /* 25 */
	"invalid macro expression",     /* 26 */
	"invalid let parameter list",   /* 27 */
	"problem saving file",          /* 28 */
	"division by zero",             /* 29 */
	"matrix expected",              /* 30 */ 
	"wrong dimensions",             /* 31 */
	"matrix is singular",           /* 32 */
	"syntax in regular expression", /* 33 */
	"throw without catch",			/* 34 */
	"problem loading library",      /* 35 */
	"import function not found",    /* 36 */
	"symbol is protected",          /* 37 */
	"number out of range",          /* 38 */
	"regular expression",           /* 39 */
	"missing end of text [/text]",  /* 40 */
	"mismatch in number of arguments",  /* 41 */
	"problem in format string",     /* 42 */
	"data type and format don't match", /* 43 */
	"invalid parameter",			/* 44 */
	"invalid parameter: 0.0",	 	/* 45 */
	"invalid parameter: NaN",	 	/* 46 */
	"illegal parameter type",	 	/* 47 */
	"symbol not in MAIN context",	/* 48 */
	"symbol not in current context", /* 49 */
	"target cannot be MAIN",		/* 50 */
	"list index out of bounds",		/* 51 */
	"array index out of bounds",	/* 52 */
	"string index out of bounds",	/* 53 */
	"nesting level to deep",		/* 54 */
	"invalid syntax",               /* 55 */
	"user error",	                /* 56 */
	"user reset -",		 	 		/* 57 */
	"received SIGINT -",		 	/* 58 */
	"function is not reentrant",	/* 59 */
	"not allowed on local symbol",  /* 60 */
	"no reference found",			/* 61 */
	"list is empty",				/* 62 */
	"I/O error",					/* 63 */
	"no working directory found",	/* 64 */
	"invalid PID",					/* 65 */
	"FOOP stack overflow",			/* 66 */
	NULL
	};


void errorMissingPar(STREAM * stream)
{
char str[64]; 
snprintf(str, 40, "...%-40s", ((char *)((stream->ptr - stream->buffer) > 40 ? stream->ptr - 40 : stream->buffer)));
errorProcExt2(ERR_MISSING_PAR, stuffString(str));
}


CELL * errorProcAll(int errorNumber, CELL * expr, int deleteFlag)
{
if(!traceFlag) fatalError(errorNumber, expr, deleteFlag);
printErrorMessage(errorNumber, expr, deleteFlag);
openTrace();
return(nilCell);
}

CELL * errorProc(int errorNumber)
{
return(errorProcAll(errorNumber, NULL, 0));
}

/* extended error info in expr */
CELL * errorProcExt(int errorNumber, CELL * expr)
{
return(errorProcAll(errorNumber, expr, 0));
}

/* extended error info in expr, which has to be discarded after printing */
CELL * errorProcExt2(int errorNumber, CELL * expr)
{
return(errorProcAll(errorNumber, expr, 1));
}

CELL * errorProcArgs(int errorNumber, CELL * expr)
{
if(expr == nilCell) 
	return(errorProcExt(ERR_MISSING_ARGUMENT, NULL));

return(errorProcExt(errorNumber, expr));
}

void fatalError(int errorNumber, CELL * expr, int deleteFlag)
{
printErrorMessage(errorNumber, expr, deleteFlag);
closeTrace();
longjmp(errorJump, errorReg);
}

void printErrorMessage(UINT errorNumber, CELL * expr, int deleteFlag)
{
CELL * lambdaFunc;
UINT * stackIdx = lambdaStackIdx;
SYMBOL * context;
int i;

if(errorNumber == EXCEPTION_THROW)
	errorNumber = ERR_THROW_WO_CATCH;

errorReg = errorNumber;

if(!errorNumber) return;

openStrStream(&errorStream, MAX_STRING, 1);
writeStreamStr(&errorStream, "ERR: ", 5);
writeStreamStr(&errorStream, errorMessage[errorReg], 0);

for(i = 0; primitive[i].name != NULL; i++)
	{
	if(evalFunc == primitive[i].function)
		{
		writeStreamStr(&errorStream, " in function ", 0);
		writeStreamStr(&errorStream, primitive[i].name, 0);
		break;
		}
	}

if(expr != NULL)
	{
	writeStreamStr(&errorStream, " : ", 3);
	printCell(expr, (errorNumber != ERR_USER_ERROR), (UINT)&errorStream);
	if(deleteFlag) deleteList(expr);
	}

while(stackIdx > lambdaStack)
	{
	lambdaFunc = (CELL *)*(--stackIdx);
	if(lambdaFunc->type == CELL_SYMBOL)
		{
		writeStreamStr(&errorStream, LINE_FEED, 0);
		writeStreamStr(&errorStream, "called from user defined function ", 0);
		context = ((SYMBOL *)lambdaFunc->contents)->context;
		if(context != mainContext)
		  {
		  writeStreamStr(&errorStream, context->name, 0);
		  writeStreamStr(&errorStream, ":", 0);
		  }
		writeStreamStr(&errorStream, ((SYMBOL *)lambdaFunc->contents)->name, 0);
		}
	}

if(!(traceFlag & TRACE_SIGINT)) evalFunc = NULL; 
parStackCounter = prettyPrintPars = 0;

if(evalCatchFlag && !(traceFlag & TRACE_SIGINT)) return;

if(errorEvent == nilSymbol)
	{
	if(errorNumber == ERR_SIGINT)
		printf("%s", errorStream.buffer);
	else
		{
		varPrintf(OUT_CONSOLE, "\n%.1024s\n", errorStream.buffer);
		if(logTraffic == LOG_MORE) writeLog(errorStream.buffer, TRUE);
		}
	}
}


/* --------------------------- load source file ------------------------- */


CELL * loadFile(char * fileName, UINT offset, int encryptFlag, SYMBOL * context)
{
CELL * result;
STREAM stream;
int errNo, dataLen;
jmp_buf errorJumpSave;
SYMBOL * contextSave;
char key[16];
#ifdef LOAD_DEBUG
int i;
#endif

contextSave = currentContext;
currentContext = context;
if(encryptFlag)
	{
	dataLen = *((int *) (linkOffset + 4));
	snprintf( key, 15, "%d", dataLen);
	}
else dataLen = MAX_FILE_BUFFER;

if(my_strnicmp(fileName, "http://", 7) == 0)
	{
	result = getPutPostDeleteUrl(fileName, nilCell, HTTP_GET, CONNECT_TIMEOUT);
	pushResult(result);
	if(memcmp((char *)result->contents, "ERR:", 4) == 0)
		return(errorProcExt2(ERR_ACCESSING_FILE, stuffString((char *)result->contents)));
	result = copyCell(sysEvalString((char *)result->contents, context, nilCell, EVAL_STRING));
	currentContext = contextSave;
	return(result);
	}

if(makeStreamFromFile(&stream, fileName, dataLen + 4 * MAX_STRING, offset) == 0) 
	return(NULL);

/* the stream contains the whole file, when encrypt flag was set */
if(encryptFlag)
	encryptPad(stream.buffer, stream.buffer, key, dataLen, strlen(key));

memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
if((errNo = setjmp(errorJump)) != 0)
	{
	closeStrStream(&stream);
	memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
	currentContext = contextSave;
	longjmp(errorJump, errNo);
	}
	
#ifdef LOAD_DEBUG
for(i = 0; i<recursionCount; i++) printf("  ");	
printf("load: %s\n", fileName);
#endif

result = evaluateStream(&stream, 0, TRUE);
currentContext = contextSave;

#ifdef LOAD_DEBUG
for(i = 0; i<recursionCount; i++) printf("  ");	
printf("finish load: %s\n", fileName);
#endif

memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));
closeStrStream(&stream);
return(result);
}

/* -------------------------- parse / compile -----------------------------

   Takes source in a string stream and and envelope cell and compiles
   newLISP source into an internal LISP cell structure tree. The tree
   can be decompiled to source at any time and is processed by the
   evaluateExpression() function.

*/

int compileExpression(STREAM * stream, CELL * cell)
{
char token[MAX_STRING + 4];
double floatNumber;
CELL * newCell;
CELL * contextCell;
SYMBOL * contextPtr;
int listFlag, tklen;
char * lastPtr;

listFlag = TRUE; /* cell is either quote or list envelope */

GETNEXT:
lastPtr = stream->ptr;
switch(getToken(stream, token, &tklen))
	{
	case TKN_ERROR:
		errorProcExt2(ERR_EXPRESSION, stuffStringN(lastPtr, 
			(strlen(lastPtr) < 60) ? strlen(lastPtr) : 60));
		return(0);

	case TKN_EMPTY:
		if(parStackCounter != 0) errorMissingPar(stream);
		return(0);

	case TKN_CHARACTER:
		newCell = stuffInteger((UINT)token[0]);
		break;

	case TKN_HEX:
#ifndef NEWLISP64
		newCell = stuffInteger64((INT64)strtoull(token,NULL,0));
#else
		newCell = stuffInteger(strtoull(token,NULL,0));
#endif
		break;

	case TKN_DECIMAL:
#ifndef NEWLISP64
		newCell = stuffInteger64(strtoll(token,NULL,0));
#else
		newCell = stuffInteger(strtoll(token,NULL,0));
#endif
		break;

	case TKN_FLOAT:
		floatNumber = (double)atof(token);
		newCell = stuffFloat(&floatNumber);
		break;

	case TKN_STRING:
		newCell = stuffStringN(token, tklen);
		break;

	case TKN_SYMBOL:
		if(strcmp(token, "lambda") == 0 || strcmp(token, "fn") == 0)
			{
			if(cell->type != CELL_EXPRESSION)
				{
				errorProcExt2(ERR_INVALID_LAMBDA, stuffString(lastPtr));
				return(0);
				}
			cell->type =  CELL_LAMBDA;
			cell->aux = (UINT)nilCell;
			goto GETNEXT;
			}
		else if(strcmp(token, "lambda-macro") == 0 || strcmp(token, "fn-macro") == 0)
			{
			if(cell->type != CELL_EXPRESSION)
				{
				errorProcExt2(ERR_INVALID_LAMBDA, stuffString(lastPtr));
				return(0);
				}
			cell->type =  CELL_MACRO;
			cell->aux = (UINT)nilCell;
			goto GETNEXT;
			}

		else if(strncmp(token, "[text]", 6) == 0) 
			{
			newCell = makeCell(CELL_STRING, (UINT)readStreamText(stream, "[/text]", &tklen));
            if(newCell->contents == 0)
				{
				deleteList(newCell); 
				errorProc(ERR_MISSING_TEXT_END);
				}
			newCell->aux = tklen + 1;
			break;
			}
		newCell = getCell(CELL_SYMBOL);
		if(*token == '$')
			{
			newCell->contents = (UINT)translateCreateSymbol(
				token, CELL_NIL, mainContext, TRUE);
			((SYMBOL *)newCell->contents)->flags |= SYMBOL_GLOBAL;
			}
		else
			newCell->contents = (UINT)translateCreateSymbol(
				token, CELL_NIL, currentContext, 0);
		break;

	case TKN_CONTEXT:
		contextPtr = NULL;
		if(currentContext != mainContext)
			{
			if(strcmp(currentContext->name, token) == 0)
				contextPtr = currentContext;
			else
				contextPtr = lookupSymbol(token, currentContext);
			}

		if(contextPtr == NULL)
			{
			contextPtr = translateCreateSymbol(
				token, CELL_CONTEXT, mainContext, TRUE);
			}

		contextCell = (CELL *)contextPtr->contents;

		if(getToken(stream, token, &tklen) != TKN_SYMBOL)
			errorProcExt2(ERR_SYMBOL_EXPECTED, stuffString(lastPtr));

		/* context does not exist */
		if(contextCell->type != CELL_CONTEXT 
		   || contextPtr != (SYMBOL*)contextCell->contents)
			{
			newCell = getCell(CELL_DYN_SYMBOL);
			newCell->aux = (UINT)contextPtr;
			newCell->contents = (UINT)allocMemory(tklen + 1);
			strncpy((char *)newCell->contents, token, tklen + 1);
			break;
			}

		/* context exists make a symbol for it */
		newCell = getCell(CELL_SYMBOL);
		newCell->contents = (UINT)translateCreateSymbol(
				token, CELL_NIL, contextPtr, TRUE);
		break;

	case TKN_QUOTE:
		newCell = getCell(CELL_QUOTE);
		linkCell(cell, newCell, listFlag);
		compileExpression(stream, newCell);
		break;

	case TKN_LEFT_PAR:
		++parStackCounter;
		newCell = getCell(CELL_EXPRESSION);
		linkCell(cell, newCell, listFlag);
		compileExpression(stream, newCell);
		break;

	case TKN_RIGHT_PAR:
		if(parStackCounter == 0) errorMissingPar(stream);
		--parStackCounter;
		cell->next = nilCell;
		return(TRUE);

	default:
		errorProcExt2(ERR_EXPRESSION, stuffString(lastPtr));
		return(0);

	}

linkCell(cell, newCell, listFlag);

if(cell->type == CELL_QUOTE && listFlag == TRUE)
	return(TRUE);

listFlag = 0;
cell = newCell;

if(parStackCounter != 0)
	{
	if(*(stream->ptr) != 0) goto GETNEXT;
	else errorMissingPar(stream);
	}

return(0);
}


void linkCell(CELL * left, CELL * right, int linkFlag)
{
if(linkFlag == 0)
	left->next = right;
else left->contents = (UINT)right;
}

int getToken(STREAM * stream, char * token, int * ptr_len)
{
char *tkn;
char chr;
int tknLen;
int floatFlag;
int bracketBalance;
char buff[4];

tkn = token;
tknLen = floatFlag = 0;
*tkn = 0;

STRIP:
if(stream->ptr > (stream->buffer + stream->size - 4 * MAX_STRING))
	{
	if(stream->handle == 0)
		{
        /* coming from commmand line or p_evalString */
		stream->buffer = stream->ptr;
		}
	else
		{
		stream->position += (stream->ptr - stream->buffer);
        		lseek((int)stream->handle, stream->position, SEEK_SET);
		memset(stream->buffer, 0, stream->size + 1);

		if(read(stream->handle, stream->buffer, stream->size) > 0)
	       	stream->ptr = stream->buffer;
		else
			{
			*stream->ptr = 0;
			return(TKN_EMPTY);
			}
		}
	}

while((unsigned char)*stream->ptr <= ' ' && (unsigned char)*stream->ptr != 0)
	++stream->ptr;

if(*stream->ptr == 0) return(TKN_EMPTY);

/* check for comments */
if(*stream->ptr == ';' || *stream->ptr == '#')
	{
	stream->ptr++;
	for(;;)
		{
		if(*stream->ptr == 0) return(TKN_EMPTY);
		if(*stream->ptr == '\n' || *stream->ptr == '\r')
			break;
		stream->ptr++;
		}
	stream->ptr++;
	goto STRIP;
	}


if( *stream->ptr == '-' || *stream->ptr == '+')
	{
	if(isDigit((unsigned char)*(stream->ptr + 1)) )
		*(tkn++) = *(stream->ptr++), tknLen++;
	}

	
if(isDigit((unsigned char)*stream->ptr) || 
                (*stream->ptr == lc_decimal_point && 
                isDigit((unsigned char)*(stream->ptr + 1))))
	{
	if(*stream->ptr == '0' && isDigit((unsigned char)*(stream->ptr + 1)))
		{
		*(tkn++) = *(stream->ptr++), tknLen++;
		while(*stream->ptr < '8' && *stream->ptr >= '0' && *stream->ptr != 0)
			*(tkn++) = *(stream->ptr++), tknLen++;
		*tkn = 0;
		return(TKN_DECIMAL);
		}
		
	while(isDigit((unsigned char)*stream->ptr) && tknLen < MAX_SYMBOL)
		*(tkn++) = *(stream->ptr++), tknLen++;
	
	if(toupper(*stream->ptr) == 'X' && token[0] == '0')
		{
		*(tkn++) = *(stream->ptr++), tknLen++;
		while(isxdigit((unsigned char)*stream->ptr) && tknLen < MAX_SYMBOL)
			*(tkn++) = *(stream->ptr++), tknLen++;
		*tkn = 0;
		return(TKN_HEX);
		}

	if(*stream->ptr == lc_decimal_point)
		{
		*(tkn++) = *(stream->ptr++), tknLen++;
		while(isDigit((unsigned char)*stream->ptr) && tknLen < MAX_SYMBOL)
			*(tkn++) = *(stream->ptr++), tknLen++;
		floatFlag = TRUE;
		}
	else if(toupper(*stream->ptr) != 'E')
		{
		*tkn = 0;
		return(TKN_DECIMAL);
		}
	
	if(toupper(*stream->ptr) == 'E') 
		{
		if(isDigit((unsigned char)*(stream->ptr+2))
		&& ( *(stream->ptr+1) == '-' || *(stream->ptr+1) == '+') )
			*(tkn++) = *(stream->ptr++), tknLen++;
		if(isDigit((unsigned char)*(stream->ptr+1)))
			{
			*(tkn++) = *(stream->ptr++), tknLen++;
			while(isDigit((unsigned char)*stream->ptr) && tknLen < MAX_SYMBOL)
				*(tkn++) = *(stream->ptr++), tknLen++;
			}
		else 
			{
			*tkn = 0;
			if(floatFlag == TRUE) return(TKN_FLOAT);
			else return(TKN_DECIMAL);
			}
		}
	*tkn = 0;
	return(TKN_FLOAT);
	}
else
	{
	chr = *stream->ptr;
	*(tkn++) = *(stream->ptr++), tknLen++;
	switch(chr)
	 {
	 case '"':
		--tkn; --tknLen;
		while(*stream->ptr != '"' && *stream->ptr != 0 
					  && tknLen < MAX_STRING) 
 			{
			if(*stream->ptr == '\\')
				{
				stream->ptr++;
				if(isDigit((unsigned char)*stream->ptr) && 
				          isDigit((unsigned char)*(stream->ptr+1)) && 
				          isDigit((unsigned char)*(stream->ptr+2)))
					{
					memcpy(buff, stream->ptr, 3);
					buff[3] = 0;
					*(tkn++) = atoi(buff);
                    tknLen++;
					stream->ptr += 3;
					continue;
					}

				switch(*stream->ptr)
					{
					case 0:
						goto SRING_TO_LONG_ERROR;
					    break;
					case 'n':
					    *(tkn++) = '\n'; break;
					case '\\':
					    *(tkn++) = '\\'; break;
					case 'r':
					    *(tkn++) = '\r'; break;
					case 't':
					    *(tkn++) = '\t'; break;
					case '"':
					    *(tkn++) = '"';  break;
					case 'x':
						if(isxdigit((unsigned char)*(stream->ptr + 1)) &&
						   isxdigit((unsigned char)*(stream->ptr + 2)))
							{
							buff[0] = '0';
							buff[1] = (unsigned char)*(stream->ptr + 1);
							buff[2] = (unsigned char)*(stream->ptr + 2);
							buff[3] = 0;
							*(tkn++) = strtol(buff, NULL, 16);
							stream->ptr += 2;
							break;
							}
					default:
					    *(tkn++) = *stream->ptr;
					}
				stream->ptr++;
                tknLen++;
				}
			else *(tkn++) = *(stream->ptr++), tknLen++;
			}
		if(*stream->ptr == '\"')
			{
			*tkn = 0;
			stream->ptr++;
			*ptr_len = tknLen;
			return(TKN_STRING);
			}
		else
			{
			goto SRING_TO_LONG_ERROR;
			}
		break;

	 case '\'':
	 case '(':
	 case ')':
		*tkn = 0;
		return(chr);
	 case '{':
		--tkn; --tknLen;
		bracketBalance = 1;
		while(*stream->ptr != 0  && tknLen < MAX_STRING) 
 			{
			if(*stream->ptr == '{') ++bracketBalance;
			if(*stream->ptr == '}') --bracketBalance;
			if(bracketBalance == 0) break;
			*(tkn++) = *(stream->ptr++), tknLen++;
			}
		if(*stream->ptr == '}')
			{
			*tkn = 0;
			stream->ptr++;
                        *ptr_len = tknLen;
			return(TKN_STRING);
			}
		else
			{
			goto SRING_TO_LONG_ERROR;
			}
		break;

		
	 case ',':
	 case ':':
		*tkn = 0;
		*ptr_len = tknLen;
		return(TKN_SYMBOL);

	case '[':
		while( tknLen < MAX_SYMBOL && *stream->ptr != 0 && *stream->ptr != ']')
			*(tkn++) = *(stream->ptr++), tknLen++;
		if(*stream->ptr == 0) return(TKN_ERROR);
		*tkn++ = ']';
		*tkn = 0;
		*ptr_len = ++tknLen;
		stream->ptr++;

		return(TKN_SYMBOL);

	 default:
		while(  tknLen < MAX_SYMBOL
			&& (unsigned char)*stream->ptr > ' ' 
			&& *stream->ptr != '"' && *stream->ptr != '\''
			&& *stream->ptr != '(' && *stream->ptr != ')'
			&& *stream->ptr != ':' && *stream->ptr != ','
                        && *stream->ptr != 0)
				*(tkn++) = *(stream->ptr++), tknLen++;
		*tkn = 0;
		*ptr_len = tknLen;
		if(*stream->ptr == ':') 
			{
			stream->ptr++;
			return(TKN_CONTEXT);
			}
		return(TKN_SYMBOL);
	 }
	}
*tkn=0;
return(TKN_ERROR);

SRING_TO_LONG_ERROR:
*tkn = 0;
errorProcExt2(ERR_STRING_TOO_LONG, 
	stuffStringN(token, strlen(token) < 40 ? strlen(token) : 40));
return(TKN_ERROR);
}

/* -------------------------- utilities ------------------------------------ */

size_t listlen(CELL * listHead)
{
size_t len = 0;

while(listHead != nilCell)
  {
  len++;
  listHead = listHead->next;
  }
  
return(len);
}

/* -------------------------- functions to get parameters ------------------ */

int getFlag(CELL * params)
{
params = evaluateExpression(params);
return(!isNil(params));
}

CELL * getInteger(CELL * params, UINT * number)
{
CELL * cell;
	
cell = evaluateExpression(params);

#ifndef NEWLISP64
if(cell->type == CELL_INT64)
	{
	if(*(INT64 *)&cell->aux >  0xFFFFFFFF) *number = 0xFFFFFFFF;
	else if(*(INT64 *)&cell->aux < INT32_MIN_AS_INT64) *number = 0x80000000;
	else *number = *(INT64 *)&cell->aux;
	}
else if(cell->type == CELL_LONG)
	*number = cell->contents;
else if(cell->type == CELL_FLOAT)
	{
#ifdef WIN_32
	if(isnan(*(double *)&cell->aux) || !_finite(*(double *)&cell->aux)) *number = 0;
#else
	if(isnan(*(double *)&cell->aux)) *number = 0; 
#endif
	else if(*(double *)&cell->aux >  4294967295.0) *number = 0xFFFFFFFF;
	else if(*(double *)&cell->aux < -2147483648.0) *number = 0x80000000;
	else *number = *(double *)&cell->aux;
	}
#else
if(cell->type == CELL_LONG)
    *number = cell->contents;
else if(cell->type == CELL_FLOAT)
    {
    if(isnan(*(double *)&cell->contents)) *number = 0;
    else if(*(double *)&cell->contents >  9223372036854775807.0) *number = 0x7FFFFFFFFFFFFFFFLL;
    else if(*(double *)&cell->contents < -9223372036854775808.0) *number = 0x8000000000000000LL;
    else *number = *(double *)&cell->contents;
    }
#endif
else
	{
	*number = 0;
	return(errorProcArgs(ERR_NUMBER_EXPECTED, params));
	}

return(params->next);
}

#ifndef NEWLISP64
CELL * getInteger64(CELL * params, INT64 * number)
{
CELL * cell;
	
cell = evaluateExpression(params);

if(cell->type == CELL_INT64)
	*number = *(INT64 *)&cell->aux;
else if(cell->type == CELL_LONG)
	*number = (int)cell->contents;
else if(cell->type == CELL_FLOAT)
	{
	if(isnan(*(double *)&cell->aux)) *number = 0; 
	else if(*(double *)&cell->aux >  9223372036854775807.0) *number = 0x7FFFFFFFFFFFFFFFLL;
	else if(*(double *)&cell->aux < -9223372036854775808.0) *number = 0x8000000000000000LL;
	else *number = *(double *)&cell->aux;
	}
else
	{
	*number = 0;
	return(errorProcArgs(ERR_NUMBER_EXPECTED, params));
	}

return(params->next);
}

#else /* NEWLISP64 */
CELL * getInteger64(CELL * params, INT64 * number)
{
CELL * cell;

cell = evaluateExpression(params);

if(cell->type == CELL_LONG)
    *number = cell->contents;
else if(cell->type == CELL_FLOAT)
    {
    if(isnan(*(double *)&cell->contents)) *number = 0;
    else if(*(double *)&cell->contents >  9223372036854775807.0) *number = 0x7FFFFFFFFFFFFFFFLL;
    else if(*(double *)&cell->contents < -9223372036854775808.0) *number = 0x8000000000000000LL;
    else *number = *(double *)&cell->contents;
    }
else
    {
    *number = 0;
    return(errorProcArgs(ERR_NUMBER_EXPECTED, params));
    }

return(params->next);
}
#endif

CELL * getIntegerExt(CELL * params, UINT * number, int evalFlag)
{
CELL * cell;

if(evalFlag)
	cell = evaluateExpression(params);
else cell = params;

#ifndef NEWLISP64
if(cell->type == CELL_INT64)
	{
	if(*(INT64 *)&cell->aux >  0xFFFFFFFF) *number = 0xFFFFFFFF;
	else if(*(INT64 *)&cell->aux < INT32_MIN_AS_INT64) *number = 0x80000000;
	else *number = *(INT64 *)&cell->aux;
	}
else if(cell->type == CELL_LONG)
	*number = cell->contents;
else if(cell->type == CELL_FLOAT)
	{
#ifdef WIN_32
	if(isnan(*(double *)&cell->aux) || !_finite(*(double *)&cell->aux)) *number = 0;
#else
	if(isnan(*(double *)&cell->aux)) *number = 0; 
#endif
	else if(*(double *)&cell->aux >  4294967295.0) *number = 0xFFFFFFFF;
	else if(*(double *)&cell->aux < -2147483648.0) *number = 0x80000000;
	else *number = *(double *)&cell->aux;
	}
#else
if(cell->type == CELL_LONG)
    *number = cell->contents;
else if(cell->type == CELL_FLOAT)
    {
    if(isnan(*(double *)&cell->contents)) *number = 0;
    else if(*(double *)&cell->contents >  9223372036854775807.0) *number = 0x7FFFFFFFFFFFFFFFLL;
    else if(*(double *)&cell->contents < -9223372036854775808.0) *number = 0x8000000000000000LL;
    else *number = *(double *)&cell->contents;
    }
#endif
else
	{
	*number = 0;
	return(errorProcArgs(ERR_NUMBER_EXPECTED, params));
	}

return(params->next);
}


CELL * getFloat(CELL * params, double * floatNumber)
{
CELL * cell;

cell = evaluateExpression(params);

#ifndef NEWLISP64
if(cell->type == CELL_FLOAT)
	*floatNumber = *(double *)&cell->aux;
else if(cell->type == CELL_INT64)
	*floatNumber = *(INT64 *)&cell->aux;
#else
if(cell->type == CELL_FLOAT)
    *floatNumber = *(double *)&cell->contents;
#endif
else if(cell->type == CELL_LONG)
	*floatNumber = (long)cell->contents;
else
	{
	*floatNumber = 0.0;
	return(errorProcArgs(ERR_NUMBER_EXPECTED, params));
	}

return(params->next);
}


CELL * getString(CELL * params, char * * stringPtr)
{
CELL * cell;
SYMBOL * sPtr;

cell = evaluateExpression(params);

if(cell->type == CELL_CONTEXT)
	{
	sPtr = translateCreateSymbol( ((SYMBOL*)cell->contents)->name, CELL_NIL,
		(SYMBOL*)cell->contents, TRUE);
	cell = (CELL *)sPtr->contents;
	}

if(cell->type != CELL_STRING)
	{
	*stringPtr = "";
	return(errorProcArgs(ERR_STRING_EXPECTED, params));
	}

*stringPtr = (char *)cell->contents;
return(params->next);
}


CELL * getStringSize(CELL * params, char * * stringPtr, size_t * size, int evalFlag)
{
CELL * cell;
SYMBOL * sPtr;

if(evalFlag)
	cell = evaluateExpression(params);
else 
	cell = params;

if(cell->type == CELL_CONTEXT)
	{
	sPtr = translateCreateSymbol( ((SYMBOL*)cell->contents)->name, CELL_NIL,
		(SYMBOL*)cell->contents, TRUE);
	symbolCheck = sPtr;
	cell = (CELL *)sPtr->contents;
	}

if(cell->type != CELL_STRING)
	{
	*stringPtr = "";
	return(errorProcArgs(ERR_STRING_EXPECTED, params));
	}

*stringPtr = (char *)cell->contents;

if(size) *size = cell->aux - 1;
return(params->next);
}


CELL * getSymbol(CELL * params, SYMBOL * * symbol)
{
CELL * cell;

cell = evaluateExpression(params);

if(cell->type != CELL_SYMBOL)
	{
	if(cell->type == CELL_DYN_SYMBOL)
		{
		*symbol = getDynamicSymbol(cell);
		return(params->next);
		}
	*symbol = nilSymbol;
	return(errorProcArgs(ERR_SYMBOL_EXPECTED, params));
	}

*symbol = (SYMBOL *)cell->contents;
return(params->next);
}

/* only used for internal syms: $timer, $error-event, $prompt-event, $command-event
   $transfer-event, and $signal-1-> $signal-32 
   If a quoted symbols hasn't been passed take the evaluated params as contents
   of the system event symbols starting with $ */
CELL * getCreateSymbol(CELL * params, SYMBOL * * symbol, char * name)
{
CELL * cell;

cell = evaluateExpression(params);

if(cell->type != CELL_SYMBOL)
    {
    if(cell->type == CELL_DYN_SYMBOL)
        {
        *symbol = getDynamicSymbol(cell);
        return(params->next);
        }
	*symbol = translateCreateSymbol(name, CELL_NIL, mainContext, TRUE);
    (*symbol)->flags |= SYMBOL_PROTECTED | SYMBOL_GLOBAL;
	deleteList((CELL *)(*symbol)->contents);
	(*symbol)->contents = (UINT)nilCell;
	if(isNil(cell)) 
		*symbol = nilSymbol;
	else if(cell->type != CELL_LAMBDA && cell->type != CELL_MACRO && cell->type != CELL_PRIMITIVE)
		{
		*symbol = nilSymbol;
		return(errorProcExt(ERR_INVALID_PARAMETER, params));
		}
	else
		(*symbol)->contents = (UINT)copyCell(cell);
    }
else
	*symbol = (SYMBOL *)cell->contents;

return(params->next);
}


CELL * getContext(CELL * params, SYMBOL * * context)
{
CELL * cell;

cell = evaluateExpression(params);

if(cell->type == CELL_CONTEXT || cell->type == CELL_SYMBOL)
	*context = (SYMBOL *)cell->contents;
else	
	{
	*context = NULL;
	return(errorProcArgs(ERR_CONTEXT_EXPECTED, params));
	}

if(symbolType(*context) != CELL_CONTEXT)
	return(errorProcExt(ERR_CONTEXT_EXPECTED, params));

return(params->next);
}


CELL * getEvalDefault(CELL * params, CELL * * result)
{
CELL * cell;

cell = evaluateExpression(params);

if(cell->type == CELL_CONTEXT)
	{
	symbolCheck = translateCreateSymbol( ((SYMBOL*)cell->contents)->name, CELL_NIL,
		(SYMBOL*)cell->contents, TRUE);
	cell = (CELL *)symbolCheck->contents;
	}

*result = cell;

return(params->next);
}

/* gets the first element, without list envelope in head
   and return the list with envelope
*/
CELL * getListHead(CELL * params, CELL * * head)
{
CELL * cell;
SYMBOL * sPtr;

cell = evaluateExpression(params);

if(cell->type == CELL_CONTEXT)
	{
	sPtr = translateCreateSymbol( ((SYMBOL*)cell->contents)->name, CELL_NIL,
		(SYMBOL*)cell->contents, TRUE);
	cell = (CELL *)sPtr->contents;
	}

if(!isList(cell->type))
	{
	*head = nilCell;
	return(errorProcExt(ERR_LIST_EXPECTED, params));
	}

*head = (CELL *)cell->contents;

return(params->next);
}

/* ------------------------------- core predicates ------------------------ */

CELL * p_setLocale(CELL * params)
{
struct lconv * lc;
char * locale;
UINT category;
CELL * cell;

if(params != nilCell)
	params = getString(params, &locale);
else locale = NULL;

getEvalDefault(params, &cell);
if(isNumber(cell->type)) /* second parameter */
	getIntegerExt(cell, &category, FALSE);
else category = LC_ALL;

locale = setlocale(category, locale);

if(locale == NULL)
	return(nilCell);

stringOutputRaw = (strcmp(locale, "C") == 0);

lc = localeconv();	
#ifdef WIN_32
if(cell->type == CELL_STRING) /* second parameter */
	*lc->decimal_point = *(char *)cell->contents; 
#endif
lc_decimal_point = *lc->decimal_point;

cell = getCell(CELL_EXPRESSION);
addList(cell, stuffString(locale));
addList(cell, stuffStringN(lc->decimal_point, 1));
return(cell);
}

CELL * p_quote(CELL * params)
{
return(copyCell(params));
}


CELL * p_eval(CELL * params)
{
CELL * result;

params = evaluateExpression(params);
result = evaluateExpression(params);
pushResultFlag = FALSE;
return(result);
}


CELL * p_catch(CELL * params)
{
jmp_buf errorJumpSave;
UINT * envStackIdxSave;
UINT * lambdaStackIdxSave;
int recursionCountSave;
int value;
CELL * expr;
CELL * result;
SYMBOL * symbol = NULL;
SYMBOL * contextSave;
CELL * objSave;
CELL * objCellSave;

expr = params;
if(params->next != nilCell)
    {
    getSymbol(params->next, &symbol);
    if(isProtected(symbol->flags))
        return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol)));
    }

memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));
/* save general environment */
envStackIdxSave = envStackIdx;
recursionCountSave = recursionCount;
lambdaStackIdxSave = lambdaStackIdx;
contextSave = currentContext;
/* save FOOP environment */
objSave = (CELL *)objSymbol.contents;
objCellSave = objCell;
itSymbol->contents = (UINT)nilCell;

if((value = setjmp(errorJump)) != 0)
    {
    memcpy(errorJump, errorJumpSave, (sizeof(jmp_buf)));
	/* restore general environment */
    recoverEnvironment(envStackIdxSave);
    recursionCount = recursionCountSave;
    lambdaStackIdx = lambdaStackIdxSave;
    currentContext = contextSave;
	/* restore FOOP environment */
	objSymbol.contents = (UINT)objSave;
	objCell = objCellSave;

    evalCatchFlag--;
    if(value == EXCEPTION_THROW)
        {
		if(symbol == NULL) return(throwResult);
        deleteList((CELL*)symbol->contents);
        symbol->contents = (UINT)throwResult;
        return(trueCell);
        }
        
    if(errorStream.buffer != NULL)
        {
		if(symbol == NULL) 
			{
			if(errorEvent == nilSymbol && evalCatchFlag == 0)
				varPrintf(OUT_CONSOLE, "\n%.1024s\n", errorStream.buffer);
			longjmp(errorJump, value);
			}
        deleteList((CELL*)symbol->contents);
        symbol->contents = (UINT)stuffString(errorStream.buffer);
        }

    return(nilCell);
    }

evalCatchFlag++;
result = copyCell(evaluateExpression(expr));
evalCatchFlag--;
memcpy(errorJump, errorJumpSave, sizeof(jmp_buf));

if(symbol == NULL) return(result);

deleteList((CELL*)symbol->contents);
symbol->contents = (UINT)result;

return(trueCell);
}


CELL * p_throw(CELL * params)
{
if(evalCatchFlag == 0) 
    return(errorProc(ERR_THROW_WO_CATCH));

throwResult = copyCell(evaluateExpression(params));
longjmp(errorJump, EXCEPTION_THROW);

return(trueCell);
}

CELL * p_throwError(CELL * params)
{
evalFunc = NULL;
errorProcExt(ERR_USER_ERROR, evaluateExpression(params));
return(nilCell);
}

CELL * evalString(CELL * params, int mode);

CELL * p_evalString(CELL * params) { return(evalString(params, EVAL_STRING)); }
CELL * p_readExpr(CELL * params) { return(evalString(params, READ_EXPR)); }

CELL * evalString(CELL * params, int mode)
{
SYMBOL * context = currentContext;
char * evalStr;

params = getString(params, &evalStr);

if(params != nilCell)
	{
	if((context = getCreateContext(params, TRUE)) == NULL)
		return(errorProcExt(ERR_SYMBOL_OR_CONTEXT_EXPECTED, params));
	}

if(mode == EVAL_STRING)
	return(copyCell(sysEvalString(evalStr, context, params->next, mode)));

/* returns a new object not yet marked for deletion */
return(sysEvalString(evalStr, context, params->next, mode));
}

/* modes:
EVAL_STRING  
  the classic eval-string: read the string, compile to s-ezpression , evaluate
READ_EXPR_SYNC
  used by p_sync() in nl-filesys.c 
READ_EXPR
  used by p_readExpr 
*/


CELL * sysEvalString(char * evalString, SYMBOL * context, CELL * proc, int mode)
{
CELL * program;
STREAM stream;
CELL * resultCell = nilCell;
SYMBOL * contextSave = NULL;
UINT * resultIdxSave;
jmp_buf errorJumpSave;
int recursionCountSave;
UINT * envStackIdxSave;
UINT offset;
CELL * xlate;

makeStreamFromString(&stream, evalString);
if(proc->next != nilCell)
	{
	getInteger(proc->next, &offset);
	stream.ptr += offset;
	}

resultIdxSave = resultStackIdx;
contextSave = currentContext;
currentContext = context;

if(proc != nilCell)
	{
	recursionCountSave = recursionCount;
	envStackIdxSave = envStackIdx;
	evalCatchFlag++;
	memcpy(errorJumpSave, errorJump, sizeof(jmp_buf));

	if(setjmp(errorJump) != 0)
		{
		memcpy(errorJump, errorJumpSave, (sizeof(jmp_buf)));
		recoverEnvironment(envStackIdxSave);
		evalCatchFlag--;
		recursionCount = recursionCountSave;
		currentContext = contextSave;
		if(mode == READ_EXPR)
			return(copyCell(evaluateExpression(proc)));
		return(evaluateExpression(proc));
		}
	}

while(TRUE)
	{
	program = getCell(CELL_QUOTE);
	if(compileExpression(&stream, program) == 0) 
		{
		pushResult(program);
		break;
		}
	if(readerEvent != nilSymbol)
		{
		executeSymbol(readerEvent, program, &xlate);
		program = makeCell(CELL_QUOTE, (UINT)xlate);
		}
	pushResult(program);
	if(mode == EVAL_STRING)
		{
		resultCell = evaluateExpression((CELL *)program->contents);
		/* note that resultCell is already pushed for deletion
		   as part of program cell */
		}
	else /* READ_EXPR or READ_EXPR_SYNC */
		{
		/* in a future version this will go into a $count sysvar instead
           same in replace */
		deleteList((CELL *)sysSymbol[0]->contents);
		sysSymbol[0]->contents = (UINT)stuffInteger(stream.ptr - stream.buffer); 
		resultCell = (CELL *)program->contents;
		program->contents = (UINT)nilCell; /* de-couple */
		/* note that resultCell is not marked for deletion
		   because decoupled from cell program */
		break;
		}

	if(resultStackIdx > resultStackTop - 256)
		{
		program = popResult(); /* leave last result */
		cleanupResults(resultIdxSave);
		pushResult(program);
		}
	}

currentContext = contextSave;

if(proc != nilCell)
	{
	memcpy(errorJump, errorJumpSave, (sizeof(jmp_buf)));
	evalCatchFlag--;
	}

return(resultCell);
}

#ifdef old_curry
CELL * p_curry(CELL * params)
{
CELL * lambda;
CELL * cell;
SYMBOL * xPtr;

xPtr = translateCreateSymbol("_x", CELL_NIL, currentContext, TRUE);
lambda = getCell(CELL_LAMBDA);
cell = getCell(CELL_EXPRESSION);
lambda->contents =  (UINT)cell;

cell->contents = (UINT)stuffSymbol(xPtr); 
cell->next = getCell(CELL_EXPRESSION);
cell = cell->next;
cell->contents = (UINT)copyCell(params);
cell = (CELL *)cell->contents;
cell->next = copyCell(params->next);
cell = cell->next;
cell->next = stuffSymbol(xPtr);

return(lambda);
}
#endif

CELL * p_curry(CELL * params)
{
CELL * lambda;
CELL * cell;

cell = getCell(CELL_EXPRESSION);
lambda = makeCell(CELL_LAMBDA, (UINT)cell);
cell->next = getCell(CELL_EXPRESSION);
cell = cell->next;
cell->contents = (UINT)copyCell(params);
cell = (CELL *)cell->contents;
cell->next = copyCell(params->next);
cell = cell->next;
cell->next = makeCell(CELL_EXPRESSION, (UINT)stuffSymbol(argsSymbol));
cell = cell->next;
cell = (CELL *)cell->contents;
cell->next = stuffInteger(0);

return(lambda);
}

CELL * p_apply(CELL * params)
{
CELL * expr;
CELL * args;
CELL * cell;
CELL * result;
CELL * func;
ssize_t count, cnt;
UINT * resultIdxSave;

func = evaluateExpression(params);

cell = copyCell(func);
expr = makeCell(CELL_EXPRESSION, (UINT)cell);
params = getEvalDefault(params->next, &args);

if(args->type != CELL_EXPRESSION)
	{
	if(isNil(args))
		{
		pushResult(expr);
		return(copyCell(evaluateExpression(expr)));
		}
	else
		return(errorProcExt(ERR_LIST_EXPECTED, args));
	}

if(params != nilCell)
	getInteger(params, (UINT *)&count);
else count = -1;
if(count < 2) count = MAX_LONG;

resultIdxSave = resultStackIdx + 2;

args = (CELL *)args->contents;        
cnt = count;
for(;;)
	{
	while(args != nilCell && cnt-- > 0)
		{
		if(isSelfEval(args->type))
			{
			cell->next = copyCell(args);
			cell = cell->next;
			}
		else
			{
			cell->next = getCell(CELL_QUOTE);
			cell = cell->next;
			cell->contents = (UINT)copyCell(args);
			}
		args = args->next;
		}
#define REF_APPLY
#ifdef REF_APPLY
	pushResult(expr);
	if(args == nilCell)
		{
		result = evaluateExpression(expr);
		if(symbolCheck) 
			{
			pushResultFlag = FALSE;
			return(result);
			}
		else return(copyCell(result));
		}		
	result = copyCell(evaluateExpression(expr));
#else
	pushResult(expr);
	result = copyCell(evaluateExpression(expr));
	if(args == nilCell) return(result);
#endif
	cell = copyCell(func);
	expr = makeCell(CELL_EXPRESSION, (UINT)cell);
	cell->next = getCell(CELL_QUOTE);
	cell = cell->next;
	cell->contents = (UINT)result;
	cnt = count - 1;
	cleanupResults(resultIdxSave);
	}
}


CELL * p_args(CELL * params)
{
if(params != nilCell) 
	return(copyCell(implicitIndexList((CELL*)argsSymbol->contents, params)));
return(copyCell((CELL*)argsSymbol->contents));
}

/* in-place expansion, if symbol==NULL all uppercase, non-nil vars are expanded */
CELL * expand(CELL * expr, SYMBOL * symbol)
{
CELL * cell = nilCell;
SYMBOL * sPtr;
int enable = 1;
CELL * cont;
int wchar;

if(isList(expr->type) || expr->type == CELL_QUOTE)
	cell = (CELL*)expr->contents;
else if(expr->type == CELL_SYMBOL && expr->contents == (UINT)symbol)
	expandSymbol(expr, symbol);

while(cell != nilCell)
	{	
	if(cell->type == CELL_SYMBOL && (cell->contents == (UINT)symbol || symbol == NULL) )
		{
		sPtr = (SYMBOL *)cell->contents;
		if(symbol == NULL)
			{
#ifndef SUPPORT_UTF8
			wchar = *sPtr->name;
#else
    		utf8_wchar(sPtr->name, &wchar);
#endif
			enable = (wchar > 64 && wchar < 91);
			cont = (CELL*)sPtr->contents;
			enable = (enable && cont->contents != (UINT)nilCell 
							&& cont->contents != (UINT)nilSymbol);
			}

		if(symbol || enable)
			expandSymbol(cell, sPtr);
		}

	else if(isEnvelope(cell->type)) expand(cell, symbol);
	cell = cell->next;
	}

return(expr);
}


void expandSymbol(CELL * cell, SYMBOL * sPtr)
{
CELL * rep;

rep = copyCell((CELL*)sPtr->contents);
cell->type = rep->type;
cell->aux = rep->aux;
cell->contents = rep->contents;
rep->type = CELL_LONG;
rep->aux = 0;
rep->contents = 0;
deleteList(rep);
}

CELL * blockExpand(CELL * block, SYMBOL * symbol)
{
CELL * expanded = nilCell;
CELL * next = nilCell;

while(block != nilCell)
	{
	if(expanded == nilCell)
		{
		next = expand(copyCell(block), symbol);
		expanded = next;
		}
	else
		{
		next->next = expand(copyCell(block), symbol);
		next = next->next;
		}
	block = block->next;
	}

return(expanded);
}


CELL * p_expand(CELL * params)
{
SYMBOL * symbol;
CELL * expr;
CELL * next;
CELL * list;
CELL * cell;
int evalFlag;

params = getEvalDefault(params, &expr);

if((next = params) == nilCell)
	return(expand(copyCell(expr), NULL));

while((params = next) != nilCell)
	{
	next = params->next;
	params = evaluateExpression(params);
	if(params->type == CELL_SYMBOL)
		symbol = (SYMBOL*)params->contents;
	else if(params->type == CELL_DYN_SYMBOL)
		symbol = getDynamicSymbol(params);
	else if(params->type == CELL_EXPRESSION)
		{
		evalFlag = getFlag(next);
		list = (CELL*)params->contents;
		while(list != nilCell)
			{
			if(list->type != CELL_EXPRESSION)
				return(errorProcExt(ERR_LIST_EXPECTED, list));
			cell = (CELL *)list->contents;
			if(cell->type != CELL_SYMBOL)
				return(errorProcExt(ERR_SYMBOL_EXPECTED, cell));
			symbol = (SYMBOL*)cell->contents;
			pushEnvironment(symbol->contents);
			pushEnvironment(symbol);
			if(evalFlag)
				symbol->contents = (UINT)evaluateExpression(cell->next);
			else
				symbol->contents = (UINT)cell->next;
			expr = expand(copyCell(expr), symbol);
			symbol = (SYMBOL*)popEnvironment();
			symbol->contents = popEnvironment();
			pushResult(expr);
			list = list->next;
			continue;
			}
		break;
		}
	else 
		return(errorProcExt(ERR_LIST_OR_SYMBOL_EXPECTED, params));
	expr = expand(copyCell(expr), symbol);
	pushResult(expr);
	}

return(copyCell(expr));
}


CELL * defineOrMacro(CELL * params, UINT cellType)
{
SYMBOL * symbol;
CELL * argsPtr;
CELL * args;
CELL * lambda;

if(params->type != CELL_EXPRESSION)
	return(errorProcExt(ERR_LIST_OR_SYMBOL_EXPECTED, params));

/* symbol to be defined */
argsPtr = (CELL *)params->contents;
if(argsPtr->type != CELL_SYMBOL)
	{
	if(argsPtr->type == CELL_DYN_SYMBOL)
		symbol = getDynamicSymbol(argsPtr);
	else
		return(errorProcExt(ERR_SYMBOL_EXPECTED, params));
	}
else symbol = (SYMBOL *)argsPtr->contents;

if(isProtected(symbol->flags))
	return(errorProcExt(ERR_SYMBOL_PROTECTED, params));

/* local symbols */
argsPtr = copyList(argsPtr->next);

args = getCell(CELL_EXPRESSION);
args->contents = (UINT)argsPtr;
/* body expressions */
args->next = copyList(params->next);

lambda = makeCell(cellType, (UINT)args);

deleteList((CELL *)symbol->contents);

symbol->contents = (UINT)lambda;

pushResultFlag = FALSE;
return(lambda);
}


CELL * p_define(CELL * params)
{
if(params->type != CELL_SYMBOL)
	{
	if(params->type != CELL_DYN_SYMBOL)
		return(defineOrMacro(params, CELL_LAMBDA));
	return(setDefine(getDynamicSymbol(params), params->next, SET_SET));
	}

return(setDefine((SYMBOL *)params->contents, params->next, SET_SET));
}


CELL * p_defineMacro(CELL * params)
{
return(defineOrMacro(params, CELL_MACRO));
}


/* also called from setq */
CELL * p_setf(CELL *params)
{
SYMBOL * symbolRef = NULL;
CELL * cell;
CELL * new;
CELL * stringRef;
char * indexRefPtr;

SETF_BEGIN:
if(params->next == nilCell)
	return(errorProc(ERR_MISSING_ARGUMENT));
		
cell = evaluateExpression(params);
if(cell == nilCell || cell == trueCell)
	errorProcExt(ERR_IS_NOT_REFERENCED, cell);
			
symbolRef = symbolCheck;
stringRef = stringCell;
indexRefPtr = stringIndexPtr;

if(symbolRef && isProtected(symbolRef->flags))
	return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbolRef)));

itSymbol->contents = (UINT)cell;
new = copyCell(evaluateExpression(params->next));
itSymbol->contents = (UINT)nilCell;
	
params = params->next; 
params = params->next; 

if(stringRef && indexRefPtr)
	{
	cell = setNthStr((CELL *)stringRef, new, indexRefPtr);
	if(params != nilCell) goto SETF_BEGIN;
	return(cell);
	}

/* delete contents of original cell */
if(isEnvelope(cell->type))
	{
	if(cell->type == CELL_ARRAY)
		deleteArray(cell);
	else
		deleteList((CELL *)cell->contents);
	}
else if(cell->type == CELL_STRING || cell->type == CELL_DYN_SYMBOL) 
	freeMemory( (void *)cell->contents);
	
	
/* get new contents */	
cell->type = new->type;
cell->aux = new->aux;
cell->contents = new->contents;

/* free cell */
new->type = CELL_FREE;
new->aux = 0;
new->contents = 0;
new->next = firstFreeCell;
firstFreeCell = new;
--cellCount;

if(params != nilCell) goto SETF_BEGIN;

/* return modified cell */
symbolCheck = symbolRef;
pushResultFlag = FALSE;
return(cell);
}


CELL * p_set(CELL *params)
{
SYMBOL * symbol;
CELL * next;

for(;;)
	{
	params = getSymbol(params, &symbol);
	next = params->next;
	if(params == nilCell)
		return(errorProc(ERR_MISSING_ARGUMENT));
	if(next == nilCell) return(setDefine(symbol, params, SET_SET));
	setDefine(symbol, params, SET_SET);
	params = next;
	}
}


CELL * p_constant(CELL *params)
{
SYMBOL * symbol;
CELL * next;
UINT * idx = envStackIdx;

for(;;)
	{
	params = getSymbol(params, &symbol);
	/* make sure symbol is not used as local in call hierachy */
	while(idx > envStack)
		{
		if(symbol == (SYMBOL *)*(--idx))
			errorProcExt2(ERR_CANNOT_PROTECT_LOCAL, stuffSymbol(symbol));
		--idx;
		}

	/* protect contexts from being set, but not vars holding contexts */
	if(symbolType(symbol) == CELL_CONTEXT && (SYMBOL *)((CELL *)symbol->contents)->contents == symbol)
		return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol)));
	next = params->next;
	if(symbol->context != currentContext)
		return(errorProcExt2(ERR_NOT_CURRENT_CONTEXT, stuffSymbol(symbol)));
	symbol->flags |= SYMBOL_PROTECTED;
	if(params == nilCell)
		return(copyCell((CELL*)symbol->contents));
	if(next == nilCell) 
		{
		next = setDefine(symbol, params, SET_CONSTANT);
		pushResultFlag = TRUE;
		return(copyCell(next));
		}
	setDefine(symbol, params, SET_CONSTANT);
	pushResultFlag = TRUE;
	params = next;
	}
}


CELL * setDefine(SYMBOL * symbol, CELL * params, int type)
{
CELL * cell;

if(isProtected(symbol->flags))
	{
	if(type == SET_CONSTANT)
		{
		if(symbol == nilSymbol || symbol == trueSymbol)
			return(errorProcExt2(ERR_SYMBOL_EXPECTED, stuffSymbol(symbol)));
		}
	else
		return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol)));
	}

cell = copyCell(evaluateExpression(params));

deleteList((CELL *)symbol->contents);
symbol->contents = (UINT)(cell);

symbolCheck = symbol;
pushResultFlag = FALSE; 

return(cell);
}


CELL * p_global(CELL * params)
{
SYMBOL * sPtr;

do
	{
	params = getSymbol(params, &sPtr);
	if(sPtr->context != mainContext || currentContext != mainContext)
		return(errorProcExt2(ERR_NOT_IN_MAIN, stuffSymbol(sPtr)));
	else 
		sPtr->flags |= SYMBOL_GLOBAL;
	} while (params != nilCell);

return(stuffSymbol(sPtr));
}

#define LET_STD 0
#define LET_NEST 1
#define LET_EXPAND 2
#define LET_LOCAL 3

CELL * let(CELL * params, int type);

CELL * p_let(CELL * params) { return(let(params, LET_STD)); }
CELL * p_letn(CELL * params) { return(let(params, LET_NEST)); }
CELL * p_letExpand(CELL * params) { return(let(params, LET_EXPAND)); }
CELL * p_local(CELL * params) { return(let(params, LET_LOCAL)); }

CELL * let(CELL * params, int type)
{
CELL * inits;
CELL * cell;
CELL * result = nilCell;
CELL * args = NULL, * list = NULL;
CELL * body;
SYMBOL * symbol;
int localCount = 0;

if(params->type != CELL_EXPRESSION)
	return(errorProcExt(ERR_INVALID_LET, params));

/* evaluate symbol assignments in parameter list 
   handle double syntax classic: (let ((s1 e1) (s2 e2) ...) ...) 
                            and: (let (s1 e1 s2 e2 ...) ...)
*/
inits = (CELL*)params->contents;
body = params->next;

if(type == LET_LOCAL)
	{
	while(inits != nilCell)
		{
		if(inits->type != CELL_SYMBOL)
			return(errorProcExt(ERR_SYMBOL_EXPECTED, inits));
		symbol = (SYMBOL *)inits->contents;
		if(isProtected(symbol->flags))
        		return(errorProcExt(ERR_SYMBOL_PROTECTED, inits));
		pushEnvironment(symbol->contents);
		pushEnvironment(symbol);
		symbol->contents = (UINT)copyCell(nilCell);
		localCount++;
		inits = inits->next;
		}
	goto EVAL_LET_BODY;	
	}

while(inits != nilCell)
	{
	if(inits->type != CELL_EXPRESSION)
		{
		if(inits->type != CELL_SYMBOL)
			return(errorProcExt(ERR_INVALID_LET, inits));
		cell = inits;
		inits = ((CELL*)cell->next)->next;
		}
	else 
		{
		cell = (CELL *)inits->contents;
		if(cell->type != CELL_SYMBOL)
			return(errorProcExt(ERR_SYMBOL_EXPECTED, inits));
		inits = inits->next;
		}

	if(type == LET_STD || type == LET_EXPAND)
		{
		if(localCount == 0) 
			list = args = copyCell(evaluateExpression(cell->next));
		else 
			{
			args->next = copyCell(evaluateExpression(cell->next));
			args = args->next;
			}
		}
	else /* LET_NEST */
		{
		symbol = (SYMBOL *)cell->contents;
		if(isProtected(symbol->flags))
        		return(errorProcExt(ERR_SYMBOL_PROTECTED, cell));
		args = copyCell(evaluateExpression(cell->next));
		pushEnvironment((CELL *)symbol->contents);
		pushEnvironment((UINT)symbol);
		symbol->contents = (UINT)args;
		}

	localCount++;
	}

/* save symbols and get new bindings */
if(type == LET_STD || type == LET_EXPAND) 
	{
	inits = (CELL*)params->contents;
	while(inits != nilCell)
		{
		if(inits->type == CELL_EXPRESSION)
			{
			cell = (CELL *)inits->contents;
			inits = inits->next;
			}
		else
			{
			cell = inits;
			inits = ((CELL*)cell->next)->next;
			}	

		symbol = (SYMBOL *)cell->contents;

		if(isProtected(symbol->flags))
			return(errorProcExt(ERR_SYMBOL_PROTECTED, cell));

		pushEnvironment((CELL *)symbol->contents);
		pushEnvironment((UINT)symbol);
		symbol->contents = (UINT)list;

		args = list;
		list = list->next;
		args->next = nilCell; /* decouple */

		/* hook in LET_EXPAND mode here */
		if(type == LET_EXPAND)
			{
			body = blockExpand(body, symbol);
			pushResult(body);
			}

		}
	}

EVAL_LET_BODY:
/* evaluate body expressions */
while(body != nilCell)
	{
	if(result != nilCell) deleteList(result);
	result = copyCell(evaluateExpression(body));
	body = body->next;
	}

/* restore environment */
while(localCount--)
	{
	symbol = (SYMBOL *)popEnvironment();
	deleteList((CELL *)symbol->contents);
	symbol->contents = popEnvironment();
	}

return(result);
}

CELL * p_first(CELL * params)
{
char str[2];
CELL * cell;
CELL * result;

getEvalDefault(params, &cell);

if(cell->type == CELL_STRING)
	{
	stringCell = cell;
	if((str[0] = *(char *)cell->contents) == 0)
		return(stuffString(""));
	
#ifndef SUPPORT_UTF8
	str[1] = 0;
	result = stuffString(str);
#else
	result = stuffStringN((char*)cell->contents, utf8_1st_len((char*)cell->contents));
#endif

	stringIndexPtr = (char *)cell->contents;
	if(symbolCheck)
		{
		pushResult(result);
		pushResultFlag = FALSE;
		}
	return(result);
	}

else if(isList(cell->type))
	{
	if(cell->contents == (UINT)nilCell)
		return(errorProcExt(ERR_LIST_EMPTY, params));

	pushResultFlag = FALSE;
	return((CELL *)cell->contents);
	}

else if(cell->type == CELL_ARRAY)
	{
	pushResultFlag = FALSE;
	return(*(CELL * *)cell->contents);
	}

return(errorProcExt(ERR_ARRAY_LIST_OR_STRING_EXPECTED, params));
}


CELL * p_rest(CELL * params)
{
CELL * cell;
CELL * tail;

/* cell = evaluateExpression(params); */
getEvalDefault(params, &cell);

if(isList(cell->type))
	{
	tail = makeCell(CELL_EXPRESSION, (UINT)copyList(((CELL*)cell->contents)->next));
	return(tail);
	}
else if(cell->type == CELL_ARRAY)
	return(subarray(cell, 1, MAX_LONG));

else if(cell->type == CELL_STRING)
	{
	if(*(char *)cell->contents == 0)
		return(stuffString(""));
#ifndef SUPPORT_UTF8
	return(stuffString((char *)(cell->contents + 1)));
#else
	return(stuffString((char *)(cell->contents + utf8_1st_len((char *)cell->contents))));
#endif
	}

return(errorProcExt(ERR_ARRAY_LIST_OR_STRING_EXPECTED, params));
}

CELL * implicitNrestSlice(CELL * num, CELL * params)
{
CELL * list;
ssize_t  n, len;

getIntegerExt(num, (UINT *)&n, FALSE);
list = evaluateExpression(params);

if(list->type == CELL_CONTEXT)
	list = (CELL *)(translateCreateSymbol(
		((SYMBOL*)list->contents)->name,
		CELL_NIL,
		(SYMBOL*)list->contents,
		TRUE))->contents;

/* slice  */
if(isNumber(list->type))
	{
	getIntegerExt(list, (UINT*)&len, FALSE);
	list = evaluateExpression(params->next);

	if(list->type == CELL_CONTEXT)
	list = (CELL *)(translateCreateSymbol(
		((SYMBOL*)list->contents)->name,
		CELL_NIL,
		(SYMBOL*)list->contents,
		TRUE))->contents;

	if(isList(list->type))    
		return(sublist((CELL *)list->contents, n, len));
	else if(list->type == CELL_STRING)
		return(substring((char *)list->contents, list->aux-1, n, len));
	else if(list->type == CELL_ARRAY)
		return(subarray(list, n, len));
	}
    
/* nrest lists */
else if(isList(list->type))
	{
	list = (CELL *)list->contents;

	if(n < 0) n = convertNegativeOffset(n, list);
    
	while(n-- && list != nilCell)
		list = list->next;
  
	return(makeCell(CELL_EXPRESSION, (UINT)copyList(list)));
	}

/* nrest strings 
   this was UTF-8 sensitive before 9.1.11, but only the
   explicit first/last/rest should be UTF8-sensitive
*/
else if(list->type == CELL_STRING) 
	return(substring((char *)list->contents, list->aux - 1, n, MAX_LONG));

else if(list->type == CELL_ARRAY)
	return(subarray(list, n, MAX_LONG));

return(errorProcExt(ERR_ILLEGAL_TYPE, params));
}


CELL * p_cons(CELL * params)
{
CELL * cons;
CELL * head;
CELL * tail;

if(params == nilCell)
	return(getCell(CELL_EXPRESSION));

head = copyCell(evaluateExpression(params));
cons = makeCell(CELL_EXPRESSION, (UINT)head);
params = params->next;

if(params != nilCell)
	{
	tail = evaluateExpression(params);
    
	if(isList(tail->type))
		{
		head->next = copyList((CELL *)tail->contents);
		cons->type = tail->type;	
		}
	else
		head->next = copyCell(tail);
	}

return(cons);
}


CELL * p_list(CELL * params)
{
CELL * list;
CELL * lastCopy = NULL;
CELL * copy;
CELL * cell;
UINT * resultIdxSave;

list = getCell(CELL_EXPRESSION);

resultIdxSave = resultStackIdx;
while(params != nilCell)
	{
	cell = evaluateExpression(params);
	if(cell->type == CELL_ARRAY)
		copy = arrayList(cell);
	else
		copy = copyCell(cell);
	if(lastCopy == NULL)
		list->contents = (UINT)copy;
	else lastCopy->next = copy;
	lastCopy = copy;
	cleanupResults(resultIdxSave);
	params = params->next;
	}

return(list);
}


CELL * p_last(CELL * params)
{
CELL * cell;
CELL * listPtr;
CELL * result;
char * str;
#ifdef SUPPORT_UTF8
char * ptr;
int len;
#endif

getEvalDefault(params, &cell);

if(cell->type == CELL_STRING)
	{
	stringCell = cell;
	str = (char *)cell->contents;
	if(*str == 0) return(copyCell(cell));
#ifndef SUPPORT_UTF8
	str += (cell->aux - 2);
	result = stuffString(str);
#else
	ptr = str;
	while((len = utf8_1st_len(str)) != 0)
		{
		ptr = str;
		str += len;
		}
	result = stuffStringN(ptr, utf8_1st_len(ptr));
	str = ptr;
#endif
	stringIndexPtr = (char *)str;
	if(symbolCheck)
		{
		pushResult(result);
		pushResultFlag = FALSE;
		}
	return(result);
	}

else if(isList(cell->type))
	{
	if(cell->contents == (UINT)nilCell)
		return(errorProcExt(ERR_LIST_EMPTY, params));

	if(cell->aux != (UINT)nilCell) 
		{
		pushResultFlag = FALSE;
		return((CELL *)cell->aux);
		}
		
	listPtr = (CELL *)cell->contents;
	while(listPtr->next != nilCell) listPtr = listPtr->next;
	cell->aux = (UINT)listPtr;
	pushResultFlag = FALSE;
	return(listPtr);
	}

else if(cell->type == CELL_ARRAY)
	{
	pushResultFlag = FALSE;
	return(*((CELL * *)cell->contents + (cell->aux - 1) / sizeof(UINT) - 1));
	}

return(errorProcExt(ERR_ARRAY_LIST_OR_STRING_EXPECTED, params));
}


/* -------------------------- program flow  and logical ------------------ */

CELL * evaluateBlock(CELL * cell)
{
CELL * result;

result = nilCell;

while(cell != nilCell)
	{
	result = evaluateExpression(cell);
	cell = cell->next;
	}
return(result);
}


CELL * p_if(CELL * params)
{
CELL * cell;

cell = evaluateExpression(params);
while(isNil(cell) || isEmpty(cell))
	{
	params = params->next;
	if(params->next == nilCell) 
		{
		pushResultFlag = FALSE;
		return(cell);
		}
	params = params->next;
	cell = evaluateExpression(params);
	}

if(params->next != nilCell) 
	cell = evaluateExpression(params->next);
	
pushResultFlag = FALSE;
return(cell);
}


CELL * p_ifNot(CELL * params)
{
CELL * cell;

cell = evaluateExpression(params);
if(!isNil(cell) && !isEmpty(cell))
	params = params->next;

cell = evaluateExpression(params->next);

pushResultFlag = FALSE;
return(cell);
}


CELL * p_when(CELL * params)
{
CELL * cell;

cell = evaluateExpression(params);
if(isNil(cell) || isEmpty(cell)) goto WHEN_END;

while((params = params->next) != nilCell)
	cell = evaluateExpression(params);

WHEN_END:
pushResultFlag = FALSE;
return(cell);
}

CELL * p_unless(CELL * params)
{
CELL * cell;

cell = evaluateExpression(params);
if(!isNil(cell) && !isEmpty(cell)) goto UNLESS_END;

while((params = params->next) != nilCell)
	cell = evaluateExpression(params);

UNLESS_END:
pushResultFlag = FALSE;
return(cell);
}


CELL * p_condition(CELL * params)
{
CELL * condition;
CELL * eval = nilCell;

while(params != nilCell)
	{
	if(params->type == CELL_EXPRESSION)
		{
		condition = (CELL *)params->contents;
		eval = evaluateExpression(condition);
		if(!isNil(eval) && !isEmpty(eval))
			{
			if(condition->next != nilCell)
				eval = evaluateBlock(condition->next);
			break;
			}
		params = params->next;
		}
	else return(errorProc(ERR_LIST_EXPECTED));
	}

pushResultFlag = FALSE;
return(eval);
}


CELL * p_case(CELL * params)
{
CELL * cases;
CELL * cond;
CELL * eval;

cases = params->next;
params = evaluateExpression(params);
while(cases != nilCell)
  {
  if(cases->type == CELL_EXPRESSION)
    {
    cond = (CELL *)cases->contents;
    if(compareCells(params, cond) == 0
	  || (cond->type == CELL_SYMBOL && symbolType((SYMBOL *)cond->contents) == CELL_TRUE)
          || cond->type == CELL_TRUE)
		{
		eval = evaluateBlock(cond->next);
        pushResultFlag = FALSE;
        return(eval);
		}
    }
	cases = cases->next;
  }

return(nilCell);
}

#define REPEAT_WHILE 0
#define REPEAT_DOWHILE 1
#define REPEAT_UNTIL 2
#define REPEAT_DOUNTIL 3

CELL * p_while(CELL * params) { return(repeat(params, REPEAT_WHILE)); }
CELL * p_doWhile(CELL * params) { return(repeat(params, REPEAT_DOWHILE)); }
CELL * p_until(CELL * params) { return(repeat(params, REPEAT_UNTIL)); }
CELL * p_doUntil(CELL * params) { return(repeat(params, REPEAT_DOUNTIL)); }


CELL * repeat(CELL * params, int type)
{
CELL * result;
CELL * cell;
CELL * cellIdx;
UINT * resultIdxSave;
SYMBOL * symbolRef = NULL;

cellIdx = initIteratorIndex();

resultIdxSave = resultStackIdx;
result = nilCell;
while(TRUE)
    {
    switch(type)
        {
        case REPEAT_WHILE:
            cell = evaluateExpression(params);
            if(isNil(cell) || isEmpty(cell)) goto END_REPEAT;
            cleanupResults(resultIdxSave);
            result = evaluateBlock(params->next);
            symbolRef = symbolCheck;
            break;
        case REPEAT_DOWHILE:
            result = evaluateBlock(params->next);
            symbolRef = symbolCheck;
            cell = evaluateExpression(params);
            if(isNil(cell) || isEmpty(cell)) goto END_REPEAT;
            cleanupResults(resultIdxSave);
            break;
        case REPEAT_UNTIL:
            cell = evaluateExpression(params);
            if(!isNil(cell) && !isEmpty(cell)) 
				{
				if(params->next == nilCell)
					result = cell;
				goto END_REPEAT;
				}
            cleanupResults(resultIdxSave);
            result = evaluateBlock(params->next);
            symbolRef = symbolCheck;
            break;
        case REPEAT_DOUNTIL:
            result = evaluateBlock(params->next);
            symbolRef = symbolCheck;
            cell = evaluateExpression(params);
            if(!isNil(cell) && !isEmpty(cell))
				{
				if(params->next == nilCell)
					result = cell;
				goto END_REPEAT;
				}
            cleanupResults(resultIdxSave);
            break;
        default:
            break;
        }

   	if(cellIdx->type == CELL_LONG) cellIdx->contents += 1;
    }

END_REPEAT:
recoverIteratorIndex(cellIdx);

symbolCheck = symbolRef;
pushResultFlag = FALSE;
return(result);
}


CELL * getPushSymbolParam(CELL * params, SYMBOL * * sym)
{
SYMBOL * symbol;
CELL * cell;

if(params->type != CELL_EXPRESSION)
	return(errorProcExt(ERR_LIST_EXPECTED, params));

cell = (CELL *)params->contents;
if(cell->type != CELL_SYMBOL)
	return(errorProcExt(ERR_SYMBOL_EXPECTED, cell));

*sym = symbol = (SYMBOL *)cell->contents;
if(isProtected(symbol->flags))
	return(errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol)));

pushEnvironment((CELL *)symbol->contents);
pushEnvironment((UINT)symbol);
symbol->contents = (UINT)nilCell;

return(cell->next);
}


CELL * initIteratorIndex(void)
{
CELL * cell = stuffInteger(0);

pushEnvironment(dolistIdxSymbol->contents);
pushEnvironment(dolistIdxSymbol);
dolistIdxSymbol->contents = (UINT)cell;

return(cell);
}

void recoverIteratorIndex(CELL * cellIdx)
{
deleteList(cellIdx);
dolistIdxSymbol = (SYMBOL*)popEnvironment();
dolistIdxSymbol->contents = (UINT)popEnvironment();
}

CELL * loop(CELL * params, int forFlag)
{
CELL * cell;
CELL * cond = nilCell;
CELL * block;
SYMBOL * symbol = NULL;
double fromFlt, toFlt, interval, step, cntFlt;
INT64 stepCnt, i;
INT64 fromInt64 = 0, toInt64 = 0;
int intFlag;
UINT * resultIdxSave;

cell = getPushSymbolParam(params, &symbol);

/* integer loops for dotimes and (for (i from to) ...) */
if((intFlag = ((CELL *)cell->next)->next == nilCell))
	{
	if(forFlag)
		{
		cell = getInteger64(cell, &fromInt64);
		getInteger64(cell, &toInt64);
		stepCnt = (toInt64 > fromInt64) ? toInt64 - fromInt64 : fromInt64 - toInt64;
		}
	else /* dotimes */
		{
		fromInt64 = toInt64 = 0;
		cond = getInteger64(cell, &stepCnt);
		}
	}
else /* float (for (i from to step) ...) */
	{
	cell = getFloat(cell, &fromFlt);
	cell = getFloat(cell, &toFlt);
	cond = getFloat(cell, &step);
	if(isnan(fromFlt) || isnan(toFlt) || isnan(step))
		return(errorProc(ERR_INVALID_PARAMETER_NAN));
	if(step < 0) step = -step;
	if(fromFlt > toFlt) step = -step;
	cntFlt = (fromFlt < toFlt) ? (toFlt - fromFlt)/step : (fromFlt - toFlt)/step;
	stepCnt = (cntFlt > 0.0) ? floor(cntFlt + 0.0000000001) : floor(-cntFlt + 0.0000000001);
	}
	
block = params->next;
resultIdxSave = resultStackIdx;
cell = nilCell;
for(i = 0; i <= stepCnt; i++)
	{
	if(!forFlag && i == stepCnt) break;
	deleteList((CELL *)symbol->contents);
	if(intFlag)	
		{
		symbol->contents = 
			(UINT)stuffInteger64((fromInt64 > toInt64) ? fromInt64 - i: 
                                                         fromInt64 + i);
		}
	else
		{
		interval = fromFlt + i * step;
		symbol->contents = (UINT)stuffFloat(&interval);
		}
	/* cleanupResults(resultIdxSave);*/
	while(resultStackIdx > resultIdxSave) deleteList(popResult());
	if(cond != nilCell)  
			{
			cell = evaluateExpression(cond);
			if(!isNil(cell)) break;
			}
	cell = evaluateBlock(block);
	}


if(symbolCheck && cell != (CELL *)symbol->contents && symbol != symbolCheck)
	pushResultFlag = FALSE;
else
	cell = copyCell(cell);

deleteList((CELL *)symbol->contents);
symbol = (SYMBOL*)popEnvironment();
symbol->flags &= ~SYMBOL_PROTECTED;
symbol->contents = (UINT)popEnvironment();

return(cell);
}


CELL * p_dotimes(CELL * params)
{
return(loop(params, 0));
}

CELL * p_for(CELL * params)
{
return(loop(params, 1));
}


#define DOLIST 0
#define DOTREE 1
#define DOARGS 2
#define DOSTRING 3

CELL * p_dolist(CELL * params)
{
return(dolist(params, DOLIST));
}

CELL * p_dotree(CELL * params)
{
return(dolist(params, DOTREE));
}

CELL * p_doargs(CELL * params)
{
return(dolist(params, DOARGS));
}

CELL * p_dostring(CELL * params)
{
return(dolist(params, DOSTRING));
}

CELL * dolist(CELL * params, int doType)
{
CELL * cell;
CELL * list = nilCell;
char * str;
#ifdef SUPPORT_UTF8
int chr;
#endif
CELL * cond = nilCell;
SYMBOL * symbol = NULL;
SYMBOL * sPtr;
CELL * cellIdx;
UINT * resultIdxSave;

cell = getPushSymbolParam(params, &symbol);
cellIdx = initIteratorIndex();

switch(doType)
	{
	case DOLIST:
		/* list = copyCell(evaluateExpression(cell)); */
		getEvalDefault(cell, &list);
		list = copyCell(list);
		if(!isList(list->type))
			return(errorProcExt(ERR_LIST_EXPECTED, cell));
		cond = cell->next;
		break;
	case DOTREE:
		getContext(cell, &sPtr);
		list = getCell(CELL_EXPRESSION);
		collectSymbols((SYMBOL *)((CELL *)sPtr->contents)->aux, list);
		cond = (getFlag(cell->next) == 1) ? trueCell : nilCell;
		break;	
	case DOARGS:
		list = copyCell((CELL *)argsSymbol->contents);
		cond = cell;
		break;
	case DOSTRING:
		getString(cell, &str);
		resultIdxSave = resultStackIdx;
		cond = cell->next;
		while(*str)
			{
			cleanupResults(resultIdxSave);
			deleteList((CELL *)symbol->contents);
#ifdef SUPPORT_UTF8
			str = utf8_wchar(str, &chr);
			symbol->contents = (UINT)stuffInteger(chr);
#else
			symbol->contents = (UINT)stuffInteger((int)*str++);
#endif
			if(cond != nilCell)
				{
				cell = evaluateExpression(cond);
				if(!isNil(cell)) break;
				}
			cell = evaluateBlock(params->next);
			if(cellIdx->type == CELL_LONG) cellIdx->contents += 1;
			}
		goto FINISH_DO;
		break;
	}

/* make sure worklist gets destroyed */
pushResult(list); 
list = (CELL *)list->contents;

resultIdxSave = resultStackIdx;
cell = nilCell;
while(list!= nilCell)
	{
	cleanupResults(resultIdxSave);
	deleteList((CELL *)symbol->contents);
	symbol->contents = (UINT)copyCell(list);
	if(cond != nilCell)
		{
		if(doType == DOTREE)
			{
			sPtr = (SYMBOL *)list->contents;
			if(*sPtr->name != '_') goto DO_CONTINUE;
			}
		else
			{
			cell = evaluateExpression(cond);
			if(!isNil(cell)) break;
			}
		}
	cell = evaluateBlock(params->next);
	if(cellIdx->type == CELL_LONG) cellIdx->contents += 1;
	DO_CONTINUE:	
	list = list->next;
	}

FINISH_DO:
if(symbolCheck && cell != (CELL *)symbol->contents && symbol != symbolCheck)
	pushResultFlag = FALSE;
else
	cell = copyCell(cell);

recoverIteratorIndex(cellIdx);

deleteList((CELL *)symbol->contents);
symbol = (SYMBOL*)popEnvironment();
symbol->contents = (UINT)popEnvironment();

return(cell);
}


CELL * p_evalBlock(CELL * params)
{
CELL * cell;

cell = evaluateBlock(params);
pushResultFlag = FALSE;
return(cell);
}

CELL * p_copy(CELL * params)
{
CELL * copy;

copy = copyCell(evaluateExpression(params));
symbolCheck = NULL;
return(copy);
}

CELL * p_silent(CELL * params)
{
CELL * cell;
evalSilent  = TRUE;

cell = evaluateBlock(params);
if(symbolCheck)
	{
	pushResultFlag = FALSE;
	return(cell);
	}
	
return(copyCell(cell));
}


CELL * p_and(CELL * params)
{
CELL * result = trueCell;

while(params != nilCell)
	{
	result = evaluateExpression(params);
	if(isNil(result) || isEmpty(result)) return(copyCell(result));
	params = params->next;
	}

if(symbolCheck)
	{
	pushResultFlag = FALSE;
	return(result);
	}
	
return(copyCell(result));     
}


CELL * p_or(CELL * params)
{
CELL * result = nilCell;

while(params != nilCell)
	{
	result = evaluateExpression(params);
	if(!isNil(result) && !isEmpty(result)) 
		{
		if(symbolCheck)
			{
			pushResultFlag = FALSE;
			return(result);
			}
		return(copyCell(result));
		}
	params = params->next;
	}

return(copyCell(result));
}


CELL * p_not(CELL * params)
{
CELL * eval;

eval = evaluateExpression(params);
if(isNil(eval) || isEmpty(eval)) 
    return(trueCell);
return(nilCell);
}



/* ------------------------------ I / O --------------------------------- */

CELL * p_print(CELL * params)
{
return println(params, FALSE);
}


CELL * p_println(CELL * params)
{
return println(params, TRUE);
}


CELL * println(CELL * params, int lineFeed)
{
CELL * result;

result = nilCell;
while(params != nilCell)
	{
	result = evaluateExpression(params);
	printCell(result, 0, OUT_DEVICE);
	params = params->next;
	}

if(lineFeed) varPrintf(OUT_DEVICE, LINE_FEED);

return(copyCell(result));
}


CELL * p_device(CELL * params)
{
if(params != nilCell)
	getInteger(params, &printDevice);
return(stuffInteger(printDevice));
}


CELL * p_load(CELL * params)
{
char * fileName;
CELL * result = nilCell;
CELL * next;
SYMBOL * context;
int count = 0;

/* get last parameter */
if((next = params) == nilCell)
	errorProc(ERR_MISSING_ARGUMENT);
while(next->next != nilCell)
	{
	count++;
	next = next->next;
	}

next = evaluateExpression(next);
if(next->type == CELL_STRING)
	{
	count++;
	context = mainContext;
	}
else
	{
	if(count == 0)
		errorProcExt(ERR_STRING_EXPECTED, next);
	if((context = getCreateContext(next, FALSE)) == NULL)
		errorProcExt(ERR_SYMBOL_OR_CONTEXT_EXPECTED, next);
	next = NULL;
	}

while(count--)
	{
	/* if last arg was a string, avoid double evaluation */
	if(count == 0 && next != NULL)
		getStringSize(next, &fileName, NULL, FALSE);
	else 
		params = getString(params, &fileName);

	result = loadFile(fileName, 0, 0, context);

	if(result == NULL)
	    return(errorProcExt2(ERR_ACCESSING_FILE, stuffString(fileName)));
	}

return(result);
}


void saveContext(SYMBOL * sPtr, UINT device)
{
SYMBOL * contextSave;

contextSave = currentContext;

currentContext = sPtr;

if(currentContext != mainContext)
	{
	varPrintf(device, "%s(context '%s)%s%s", 
		LINE_FEED, sPtr->name, LINE_FEED, LINE_FEED);
	/* make sure 'set' is not overwritten */
	if((sPtr = lookupSymbol("set", currentContext)) != NULL)
		{
		deleteList((CELL *)sPtr->contents);
		sPtr->contents = (UINT)copyCell(nilCell);
		}
	}
			

saveSymbols((SYMBOL *)((CELL*)currentContext->contents)->aux, device);

if(currentContext != mainContext)
	varPrintf(device, "%s(context MAIN)%s%s", 
		LINE_FEED, LINE_FEED, LINE_FEED);

currentContext = contextSave;
}


void saveSymbols(SYMBOL * sPtr, UINT device)
{
int type;

if(sPtr != NIL_SYM && sPtr != NULL)
	{
	saveSymbols(sPtr->left, device);
	type = symbolType(sPtr);
	if(type == CELL_CONTEXT)
		{
		if(sPtr == (SYMBOL *)((CELL *)sPtr->contents)->contents)
			{
			if(sPtr != currentContext && *sPtr->name != '$') saveContext(sPtr, device);
			}
		else printSymbol(sPtr, device);	
		}
	/* don't save primitives, symbols containing nil and the trueSymbol */
	else if(type != CELL_PRIMITIVE && type != CELL_NIL
		&& sPtr != trueSymbol && type != CELL_IMPORT_CDECL
#if defined(WIN_32) || defined(CYGWIN)
		&& type != CELL_IMPORT_DLL
#endif
		)
		if(*sPtr->name != '$') printSymbol(sPtr, device);
	saveSymbols(sPtr->right, device);
	}
}


CELL * p_save(CELL * params)
{
char * fileName;
STREAM strStream = {0, NULL, NULL, 0, 0};
CELL * result;
SYMBOL * contextSave;
CELL * dataCell;
int errorFlag = 0;

contextSave = currentContext;
currentContext = mainContext;

params = getString(params, &fileName);

openStrStream(&strStream, MAX_STRING, 0);
serializeSymbols(params, (UINT)&strStream);

/* check for URL format */
if(my_strnicmp(fileName, "http://", 7) == 0)
	{
	dataCell = stuffString(strStream.buffer);
	result = getPutPostDeleteUrl(fileName, dataCell, HTTP_PUT, CONNECT_TIMEOUT);
	pushResult(result);
	deleteList(dataCell);
	errorFlag = (strncmp((char *)result->contents, "ERR:", 4) == 0);
	}
else
	errorFlag = writeFile(fileName, strStream.buffer, strStream.position, "w");
		
closeStrStream(&strStream);

currentContext = contextSave;

if(errorFlag)
	return(errorProcExt2(ERR_SAVING_FILE, stuffString(fileName)));

return(trueCell);
}

void serializeSymbols(CELL * params, UINT device)
{
SYMBOL * sPtr;

if(params->type == CELL_NIL)
	saveSymbols((SYMBOL *)((CELL*)currentContext->contents)->aux, device);
else
    while(params != nilCell)
	{
	params = getSymbol(params, &sPtr);
	if(symbolType(sPtr) == CELL_CONTEXT)
		saveContext((SYMBOL*)((CELL *)sPtr->contents)->contents, device);
	else        
		printSymbol(sPtr, device);
	}
}

/* ----------------------- copy a context with 'new' -------------- */
static SYMBOL * fromContext;
static SYMBOL * newContext;
static int overWriteFlag;

CELL * copyContextList(CELL * cell);
UINT * copyContextArray(CELL * array);


CELL * copyContextCell(CELL * cell)
{
CELL * newCell;
SYMBOL * sPtr;
SYMBOL * newSptr;

if(firstFreeCell == NULL) allocBlock();
newCell = firstFreeCell;
firstFreeCell = newCell->next;
++cellCount;

newCell->type = cell->type;
newCell->next = nilCell;
newCell->aux = cell->aux;
newCell->contents = cell->contents;

if(cell->type == CELL_DYN_SYMBOL)
	{
	sPtr = (SYMBOL*)cell->aux;
	if(sPtr->context == fromContext)
		newCell->aux =
			(UINT)translateCreateSymbol(sPtr->name, 0, newContext, TRUE);
	newCell->contents = (UINT)allocMemory(strlen((char *)cell->contents) + 1);
	memcpy((void *)newCell->contents,
		(void*)cell->contents, strlen((char *)cell->contents) + 1);
	}

if(cell->type == CELL_SYMBOL)
	{
	/* if the cell copied itself contains a symbol copy it recursevely,
	   if new, if not done here it might not been seen as new later and left
           without contents */
	sPtr = (SYMBOL *)cell->contents;
	if(sPtr->context == fromContext && !(sPtr->flags & SYMBOL_BUILTIN))
		{
		if((newSptr = lookupSymbol(sPtr->name, newContext)) == NULL)
			{
			newSptr = translateCreateSymbol(sPtr->name, symbolType(sPtr), newContext, TRUE);
			deleteList((CELL *)newSptr->contents);
			newSptr->contents = (UINT)copyContextCell((CELL*)sPtr->contents);
			}
		newCell->contents = (UINT)newSptr;
		}
	}

if(isEnvelope(cell->type))
	{
	if(cell->type == CELL_ARRAY)
		newCell->contents = (UINT)copyContextArray(cell);
	else
		{
		/* undo push last optimization */
		newCell->aux = (UINT)nilCell;
		newCell->contents = (UINT)copyContextList((CELL *)cell->contents);
		}
	}
else if(cell->type == CELL_STRING)
	{
	newCell->contents = (UINT)allocMemory((UINT)cell->aux);
	memcpy((void *)newCell->contents, (void*)cell->contents, (UINT)cell->aux);
	}

return(newCell);
}


CELL * copyContextList(CELL * cell)
{
CELL * firstCell;
CELL * newCell;

if(cell == nilCell || cell == trueCell) return(cell);

firstCell = newCell = copyContextCell(cell);

while((cell = cell->next) != nilCell)
	{
	newCell->next = copyContextCell(cell);
	newCell = newCell->next;
	}
	
return(firstCell);
}


UINT * copyContextArray(CELL * array)
{
CELL * * newAddr;
CELL * * orgAddr;
CELL * * addr;
size_t size;

addr = newAddr = (CELL * *)callocMemory(array->aux);

size = (array->aux - 1) / sizeof(UINT);
orgAddr = (CELL * *)array->contents;

while(size--)
	*(newAddr++) = copyContextCell(*(orgAddr++));
	
return((UINT*)addr);
}


void iterateSymbols(SYMBOL * sPtr)
{
int type, newFlag = FALSE;
SYMBOL * newPtr;

if(sPtr != NIL_SYM && sPtr != NULL && !(sPtr->flags & SYMBOL_BUILTIN))
	{
	iterateSymbols(sPtr->left);
	type = symbolType(sPtr);

	/* optimized check for default symbol, translate default symbol to default symbol */
	if(*sPtr->name == *fromContext->name && strcmp(sPtr->name, fromContext->name) == 0)
		{
		if((newPtr = lookupSymbol(newContext->name, newContext)) == NULL)
			{
			newPtr = translateCreateSymbol(newContext->name, type, newContext, TRUE);
			newFlag = TRUE;
			}
		}
	else
		{
		if((newPtr = lookupSymbol(sPtr->name, newContext)) == NULL)
			{
			newPtr = translateCreateSymbol(sPtr->name, type, newContext, TRUE);
			newFlag = TRUE;
			}
		}

	if(overWriteFlag == TRUE || newFlag == TRUE)
		{
		deleteList((CELL *)newPtr->contents);
		newPtr->contents = (UINT)copyContextCell((CELL*)sPtr->contents);
		}

	iterateSymbols(sPtr->right);
	}
}



CELL * p_new(CELL * params)
{
CELL * next;

overWriteFlag = FALSE;

params = getContext(params, &fromContext);
if(!fromContext) return(nilCell); /* for debug mode */

next = params->next;

if(params == nilCell)
	newContext = currentContext;
else 
	{
	params = evaluateExpression(params);
	if(params->type == CELL_CONTEXT || params->type == CELL_SYMBOL)
		newContext = (SYMBOL *)params->contents;
	else
		return(errorProcExt(ERR_CONTEXT_EXPECTED, params));

        overWriteFlag = (evaluateExpression(next)->type != CELL_NIL);

	/* allow symbols to be converted to contexts */
	if(symbolType(newContext) != CELL_CONTEXT)
		{
		if(isProtected(newContext->flags))
			return(errorProcExt(ERR_SYMBOL_PROTECTED, params));

		if(newContext->context != mainContext)
			return(errorProcExt2(ERR_NOT_IN_MAIN, stuffSymbol(newContext)));

		deleteList((CELL *)newContext->contents);
		makeContextFromSymbol(newContext, NULL);
		}
	}

if(newContext == mainContext)
	return(errorProc(ERR_TARGET_NO_MAIN));

iterateSymbols((SYMBOL *)((CELL*)fromContext->contents)->aux);

return(copyCell((CELL*)newContext->contents));
}


CELL * p_defineNew(CELL * params)
{
SYMBOL * sourcePtr;
SYMBOL * targetPtr;
char * name;

params = getSymbol(params, &sourcePtr);
if(params != nilCell)
	{
	params = getSymbol(params, &targetPtr);
	name = targetPtr->name;
	newContext = targetPtr->context;
	}
else
	{
	name = sourcePtr->name;
	newContext = currentContext;
	}

if(newContext == mainContext)
	return(errorProc(ERR_TARGET_NO_MAIN));

fromContext = sourcePtr->context;
targetPtr = translateCreateSymbol(name, symbolType(sourcePtr), newContext, TRUE);

deleteList((CELL *)targetPtr->contents);
targetPtr->contents = (UINT)copyContextCell((CELL*)sourcePtr->contents);

return(stuffSymbol(targetPtr));
}
	


/* ------------------------------ system ------------------------------ */

CELL * isType(CELL *, int);

CELL * p_isNil(CELL * params)
{
params = evaluateExpression(params);

if(isNil(params))
        return(trueCell);

return(nilCell);
}

CELL * p_isEmpty(CELL * params)
{
CELL * cell;

getEvalDefault(params, &cell);
return(isEmptyFunc(cell));
}

CELL * isEmptyFunc(CELL * cell)
{
if(cell->type == CELL_STRING)
    {
    if(*(char*)cell->contents == 0)
        return(trueCell);
    else return(nilCell);
    }

if(!isList(cell->type))
		return(errorProcExt(ERR_LIST_OR_STRING_EXPECTED, cell));
if(cell->contents == (UINT)nilCell)
	return(trueCell);
return(nilCell);
}

CELL * isZero(CELL * cell)
{
#ifndef NEWLISP64
if(cell->type == CELL_INT64)
	{
	if(*(INT64 *)&cell->aux == 0)
		return(trueCell);
	else
		return(nilCell);
	}
#endif

if(cell->type == CELL_FLOAT)
	{
#ifndef NEWLISP64
	if(*(double *)&cell->aux == 0.0)
#else
    if(*(double *)&cell->contents == 0.0)
#endif
		return(trueCell);
	else
		return(nilCell);
	}	

if(cell->type == CELL_LONG)
	{
	if(cell->contents == 0)
		return(trueCell);
	}

return(nilCell);
}


CELL * p_isNull(CELL * params)
{
CELL * cell;

cell = evaluateExpression(params);
if(isNil(cell))
	return(trueCell);

if( (cell->type == CELL_STRING || isList(cell->type)))
	return(isEmptyFunc(cell));

#ifndef NEWLISP64
if(cell->type == CELL_FLOAT && (isnan(*(double *)&cell->aux)) )
#else
if(cell->type == CELL_FLOAT && (isnan(*(double *)&cell->contents)))
#endif
	return(trueCell);

return(isZero(cell));
}


CELL * p_isZero(CELL * params)
{
params = evaluateExpression(params);
return(isZero(params));
}


CELL * p_isTrue(CELL * params)
{
params = evaluateExpression(params);
if(!isNil(params) && !isEmpty(params))
        return(trueCell);

return(nilCell);
}

CELL * p_isInteger(CELL * params)
{
params = evaluateExpression(params);
if((params->type & COMPARE_TYPE_MASK) == CELL_INT)
	return(trueCell);
return(nilCell);
}


CELL * p_isFloat(CELL * params)
	{ return(isType(params, CELL_FLOAT)); }
	
CELL * p_isNumber(CELL * params)
{
params = evaluateExpression(params);
if(isNumber(params->type)) return(trueCell);
return(nilCell);
}

CELL * p_isString(CELL * params)
	{ return(isType(params, CELL_STRING)); }

CELL * p_isSymbol(CELL * params)
        { return(isType(params, CELL_SYMBOL)); }

CELL * p_isContext(CELL * params)
{
char * symStr;
SYMBOL * ctx;

/* check type */
if(params->next == nilCell) 
    return(isType(params, CELL_CONTEXT)); 

/* check for existense of symbol */
params = getContext(params, &ctx);
if(!ctx) return(nilCell); /* for debug mode */
getString(params, &symStr);

return (lookupSymbol(symStr, ctx) ? trueCell : nilCell);    
}

CELL * p_isPrimitive(CELL * params)
	{ return(isType(params, CELL_PRIMITIVE)); }


CELL * p_isGlobal(CELL * params)
{
params = evaluateExpression(params);
if(isSymbol(params->type) && isGlobal(((SYMBOL *)params->contents)->flags))
	return(trueCell);
return(nilCell);
}

CELL * p_isProtected(CELL * params)
{
params = evaluateExpression(params);
if(isSymbol(params->type) && isProtected(((SYMBOL *)params->contents)->flags))
	return(trueCell);
return(nilCell);
}
	
CELL * p_isAtom(CELL * params)
{
if(params == nilCell)
	return(errorProc(ERR_MISSING_ARGUMENT));
params = evaluateExpression(params);
if(params->type & ENVELOPE_TYPE_MASK) return(nilCell);
return(trueCell);
}

CELL * p_isQuote(CELL *params)
	{ return(isType(params, CELL_QUOTE)); }

CELL * p_isList(CELL * params)
	{ return(isType(params, CELL_EXPRESSION)); }

CELL * p_isLambda(CELL * params)
	{ return(isType(params, CELL_LAMBDA)); }

CELL * p_isMacro(CELL * params)
	{ return(isType(params, CELL_MACRO)); }

CELL * p_isArray(CELL * params)
	{ return(isType(params, CELL_ARRAY)); }

CELL * isType(CELL * params, int operand)
{
CELL * contextCell;

if(params == nilCell)
	return(errorProc(ERR_MISSING_ARGUMENT));
params = evaluateExpression(params);
if((UINT)operand == params->type) return(trueCell);
switch(operand)
	{
	case CELL_PRIMITIVE:
		if(params->type == CELL_IMPORT_CDECL
#if defined(WIN_32) || defined(CYGWIN)
		|| params->type == CELL_IMPORT_DLL 
#endif
		)
			return(trueCell);
		break;
	case CELL_EXPRESSION:
		if(isList(params->type)) return(trueCell);
                break;
	case CELL_SYMBOL:
		if(params->type == CELL_DYN_SYMBOL) /* check if already created */
			{
			contextCell = (CELL *)((SYMBOL *)params->aux)->contents;
			if(contextCell->type != CELL_CONTEXT)
				fatalError(ERR_CONTEXT_EXPECTED, 
					stuffSymbol((SYMBOL*)params->aux), TRUE);
			if(lookupSymbol((char *)params->contents, (SYMBOL*)contextCell->contents))
				return(trueCell);
			}
			
		break;
	default:
		break;
	}

return(nilCell);
}


CELL * p_isLegal(CELL * params)
{
char * symStr;

getString(params, &symStr);

if(isLegalSymbol(symStr)) return(trueCell);

return(nilCell);
}


int isLegalSymbol(char * source)
{
STREAM stream;
char token[MAX_SYMBOL + 1];
int tklen;

if(*source == (char)'"' || *source == (char)'{' 
   || (unsigned char)*source <= (unsigned char)' ' || *source == (char)';' || *source == (char)'#')
        return(0);

makeStreamFromString(&stream, source);

return(getToken(&stream, token, &tklen) == TKN_SYMBOL && tklen == stream.size - 4 * MAX_STRING);
}


CELL * p_exit(CELL * params)
{
UINT result;

if(demonMode) 
	{
	fclose(IOchannel);
#ifndef WIN_32
	IOchannel = NULL;
#endif
	longjmp(errorJump, ERR_USER_RESET);
	}

if(params != nilCell) getInteger(params, &result);
else result = 0;
exit(result);
return(trueCell);
}


CELL * p_reset(CELL * params)
{
if(params != nilCell)
	{
	if(!getFlag(params)) 
	/* only for experimental purose not documented
	   returns all free cell blocks to the OS */
		freeCellBlocks();
#ifndef LIBRARY
#ifndef WIN_32
	else
		execv(MainArgs[0], MainArgs);
#endif
#endif
	}
else
	longjmp(errorJump, ERR_USER_RESET);

return(trueCell);
}

CELL * setEvent(CELL * params, SYMBOL * * eventSymPtr, char * sysSymName)
{
if(params != nilCell) getCreateSymbol(params, eventSymPtr, sysSymName);
return(makeCell(CELL_SYMBOL, (UINT)*eventSymPtr));
}

CELL * p_errorEvent(CELL * params)
{
return(setEvent(params, &errorEvent, "$error-event"));
}

CELL * p_promptEvent(CELL * params)
{
return(setEvent(params, &promptEvent, "$prompt-event"));
}

CELL * p_commandEvent(CELL * params)
{
return(setEvent(params, &commandEvent, "$command-event"));
}

CELL * p_transferEvent(CELL * params)
{
return(setEvent(params, &transferEvent, "$transfer-event"));
}

CELL * p_readerEvent(CELL * params)
{
return(setEvent(params, &readerEvent, "$reader-event"));
}


#ifndef WIN_32

CELL * p_timerEvent(CELL * params)
{
double seconds;
UINT timerOption = 0;
struct itimerval timerVal;
struct itimerval outVal;
static double duration;

if(params != nilCell) 
  {
  params = getCreateSymbol(params, &timerEvent, "$timer");

  if(params != nilCell)
    {
    params = getFloat(params, &seconds);
    duration = seconds;
    if(params != nilCell)
        getInteger(params, &timerOption);
    memset(&timerVal, 0, sizeof(timerVal));
    timerVal.it_value.tv_sec = seconds;
    timerVal.it_value.tv_usec = (seconds - timerVal.it_value.tv_sec) * 1000000;
    if(setitimer((int)timerOption, &timerVal, &outVal) == -1)
      return(nilCell);
    return(stuffInteger(0));
    }
  else
    getitimer(timerOption, &outVal);

  seconds = duration - (outVal.it_value.tv_sec + outVal.it_value.tv_usec / 1000000.0);
  return(stuffFloat(&seconds));
  }
  
return(makeCell(CELL_SYMBOL, (UINT)timerEvent));
}
#endif

#define IGNORE_S 0
#define DEFAULT_S 1
#define RESET_S 2
CELL * p_signal(CELL * params)
{
SYMBOL * signalEvent;
UINT sig;
char sigStr[12];
char mode;

params = getInteger(params, &sig);
if(sig > 32 || sig < 1) return(nilCell);

if(params->type == CELL_STRING)
	{
	mode = toupper(*(char *)params->contents);
	symHandler[sig - 1] = nilSymbol;
	if(mode == 'I') /* "ignore" */
		return(signal(sig, SIG_IGN) == SIG_ERR ? nilCell: trueCell);
	else if(mode == 'D') /* "default" */
		return(signal(sig, SIG_DFL) == SIG_ERR ? nilCell: trueCell);
	else if(mode == 'R') /* "reset" */
		return(signal(sig, signal_handler) == SIG_ERR ? nilCell: trueCell);
	}
else if(params != nilCell)
	{
	snprintf(sigStr, 11, "$signal-%ld", sig);
	getCreateSymbol(params, &signalEvent, sigStr);
	symHandler[sig - 1] = signalEvent;
	if(signal(sig, signal_handler) == SIG_ERR) return(nilCell);
	}
  
return(makeCell(CELL_SYMBOL, (UINT)symHandler[sig - 1]));
}


CELL * p_lastError(CELL * params)
{
CELL * result;
char * sPtr;
UINT errNum = errorReg;

if(params != nilCell)
	getInteger(params, &errNum);

if(!errNum) return(nilCell);

result = makeCell(CELL_EXPRESSION, (UINT)stuffInteger(errNum));
if(params != nilCell)
	sPtr = (errNum > MAX_ERROR_NUMBER) ? UNKNOWN_ERROR : errorMessage[errNum];
else
	sPtr = errorStream.buffer;

((CELL *)result->contents)->next = stuffString(sPtr);

return(result);
}


CELL * p_dump(CELL * params)
{
CELL * blockPtr;
int i;
CELL * cell;

if(params != nilCell)
	{
	cell = evaluateExpression(params);
	return(stuffIntegerList
           (5, cell, cell->type, cell->next, cell->aux, cell->contents));
	}

blockPtr = cellMemory;
while(blockPtr != NULL)
	{
	for(i = 0; i <  MAX_BLOCK; i++)
		{
		if(*(UINT *)blockPtr != CELL_FREE)
			{
			varPrintf(OUT_DEVICE, "address=%lX type=%d contents=", blockPtr, blockPtr->type);
			printCell(blockPtr, TRUE, OUT_DEVICE);
			varPrintf(OUT_DEVICE,LINE_FEED);
			}
		++blockPtr;
		}
	blockPtr = blockPtr->next;
	}
return(trueCell);
}


CELL * p_mainArgs(CELL * params)
{
CELL * cell;
ssize_t idx;

cell = (CELL*)mainArgsSymbol->contents;
if(params != nilCell && cell->type == CELL_EXPRESSION)
    {
    getInteger(params, (UINT *)&idx);
    cell = (CELL *)cell->contents;
    if(idx < 0) idx = convertNegativeOffset(idx, (CELL *)cell);
    while(idx--) cell = cell->next;
    }

return(copyCell(cell));
}


CELL * p_context(CELL * params)
{
CELL * cell;
SYMBOL * sPtr;
SYMBOL * cPtr;
char * newSymStr;

if(params->type == CELL_NIL)
	return(copyCell((CELL *)currentContext->contents));

if((cPtr = getCreateContext(params, TRUE)) == NULL)
	return(errorProcExt(ERR_SYMBOL_OR_CONTEXT_EXPECTED, params));
    
if(params->next == nilCell)
 	{
 	currentContext = cPtr;
 	return(copyCell( (CELL *)currentContext->contents));
	}
    
params = params->next;
cell = evaluateExpression(params);
if(cell->type == CELL_STRING)
	newSymStr = (char *)cell->contents;
else if(cell->type == CELL_SYMBOL)	
	newSymStr = ((SYMBOL *)cell->contents)->name;
else if(cell->type == CELL_DYN_SYMBOL)
	{
	sPtr = getDynamicSymbol(cell);
	newSymStr = sPtr->name;
	}
else
	return(errorProcExt(ERR_ILLEGAL_TYPE, cell));


if(params->next == nilCell)
	{
	pushResultFlag = FALSE;
	sPtr = lookupSymbol(newSymStr, cPtr);
	if(sPtr == NULL)
		return(nilCell);
	else
		return((CELL *)sPtr->contents);
	}


sPtr = translateCreateSymbol(newSymStr, CELL_NIL, cPtr, TRUE);

return(setDefine(sPtr, params->next, SET_SET));
}


SYMBOL * getCreateContext(CELL * cell, int evaluate)
{
SYMBOL * contextSymbol;

if(evaluate)
	cell = evaluateExpression(cell);
if(cell->type == CELL_SYMBOL || cell->type == CELL_CONTEXT)
    contextSymbol = (SYMBOL *)cell->contents;
else
    return(NULL);


if(symbolType(contextSymbol) != CELL_CONTEXT)
	{
	if(contextSymbol->context != mainContext)
		{
		contextSymbol= translateCreateSymbol(
			contextSymbol->name, CELL_CONTEXT, mainContext, 1);
		}

	if(symbolType(contextSymbol) != CELL_CONTEXT)
		{
        if(isProtected(contextSymbol->flags))
        	errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(contextSymbol));

		deleteList((CELL *)contextSymbol->contents);
		makeContextFromSymbol(contextSymbol, NULL);
		}
	}

/* if this is a context var retrieve the real context symbol */
return((SYMBOL *)((CELL *)contextSymbol->contents)->contents);
}


CELL * p_default(CELL * params)
{
SYMBOL * contextSymbol;

getContext(params, &contextSymbol);

symbolCheck = translateCreateSymbol(contextSymbol->name, CELL_NIL, contextSymbol, TRUE);
pushResultFlag = FALSE;
return((CELL *)symbolCheck->contents);
}


/* FOOP fuctions */

/* filled in colon, only used internally and by obj function */
/* need stack for objSymbol.contents */
/* what happens to stack when using catch/throw */

CELL * p_colon(CELL * params)
{
SYMBOL * contextSymbol = NULL;
SYMBOL * methodSymbol;
SYMBOL * sPtr;
CELL * proc;
CELL * cell;
CELL * obj;
CELL * objSave;
CELL * objCellSave;
SYMBOL * objSymbolContextSave;
int objSymbolFlagsSave;

if(params->type != CELL_SYMBOL)
	return(errorProcExt(ERR_SYMBOL_EXPECTED, params));

methodSymbol = (SYMBOL *)params->contents;
params = getEvalDefault(params->next, &obj);

objSymbolFlagsSave = objSymbol.flags;
objSymbolContextSave = objSymbol.context;
if(symbolCheck)	
	{
	objSymbol.flags = symbolCheck->flags;
	objSymbol.context = symbolCheck->context;
	}	

objSave = (CELL *)objSymbol.contents;
objCellSave = objCell;
objCell = obj;

#ifdef FOOP_DEBUG
printf("entering colon saving in objSave:");
printCell(objSave, TRUE, OUT_CONSOLE);
puts("");
#endif

cell = (CELL *)obj->contents;
if(obj->type != CELL_EXPRESSION)
	return(errorProcExt(ERR_LIST_EXPECTED, cell));

if(cell->type == CELL_SYMBOL || cell->type == CELL_CONTEXT)
    contextSymbol = (SYMBOL *)cell->contents;
if(contextSymbol == NULL || symbolType(contextSymbol) != CELL_CONTEXT)
	return(errorProcExt(ERR_CONTEXT_EXPECTED, cell));

sPtr = methodSymbol;
if((methodSymbol = lookupSymbol(sPtr->name, contextSymbol)) == NULL)
	return(errorProcExt2(ERR_INVALID_FUNCTION, stuffSymbol(sPtr)));

cell = stuffSymbol(methodSymbol);
proc = makeCell(CELL_EXPRESSION, (UINT)cell);

while(params != nilCell)
	{
	cell->next = copyCell(params);
	cell = cell->next;
	params = params->next;
	}

pushResult(proc);

#ifdef FOOP_DEBUG
printf("colon calling %s in %s with objCell:", methodSymbol->name, contextSymbol->name);
printCell(objCell, TRUE, OUT_CONSOLE);
puts("");
#endif

cell = copyCell(evaluateExpression(proc));

objSymbol.flags = objSymbolFlagsSave;
objSymbol.context = objSymbolContextSave;

objSymbol.contents = (UINT)objSave;
objCell = objCellSave;

#ifdef FOOP_DEBUG
printf("leavin colon objCell restored to:");
printCell(obj, TRUE, OUT_CONSOLE);
puts("\n");
#endif


return(cell);
}

CELL * p_self(CELL * params)
{
CELL * result;

if(objSymbol.contents == (UINT)nilCell)
	return(nilCell);

if(params == nilCell)
	{
	symbolCheck = &objSymbol;
	pushResultFlag = FALSE;
	return((CELL *)objSymbol.contents);
	}

result = implicitIndexList((CELL*)objSymbol.contents, params);

symbolCheck = &objSymbol;
pushResultFlag = FALSE;

return(result);
}


CELL * p_systemSymbol(CELL * params)
{
UINT idx;

getInteger(params, &idx);

if(idx > 15 || idx < 0) return(nilCell);

return(copyCell((CELL*)sysSymbol[idx]->contents));
}


/* end of file */
