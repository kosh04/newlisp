/* newlisp.c --- entry point and main functions for newLISP


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
#include "primes.h"

#ifdef WINDOWS
#include <winsock2.h>
#else
#include <sys/socket.h>
#endif

#ifdef READLINE
#include <readline/readline.h>
/* take following line out on Slackware Linux */
#include <readline/history.h>
#endif /* end READLINE */

#ifdef SUPPORT_UTF8
#include <wctype.h>
#endif

#define freeMemory free

#define INIT_FILE "init.lsp"

#ifdef WINDOWS
#define fprintf win_fprintf
#define fgets win_fgets
#define fclose win_fclose
#endif

#ifdef LIBRARY
extern STREAM libStrStream;
int newlispLibConsoleFlag = 0;
#endif

#if defined(LINUX) || defined(KFREEBSD)
#ifdef ANDROID
int opsys = 11;
#else
int opsys = 1;
#endif
#endif

#ifdef _BSD
int opsys = 2;
#endif

#ifdef MAC_OSX
#ifdef EMSCRIPTEN
int opsys = 11;
#else
int opsys = 3;
#endif
#endif

#ifdef SOLARIS
int opsys = 4;
#endif

#ifdef WINDOWS
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


/* opsys = 11 taken for ANDROID; see LINUX */

int bigEndian = 1; /* gets set in main() */

int version = 10705;

char copyright[]=
"\nnewLISP v.10.7.5 Copyright (c) 2016 Lutz Mueller. All rights reserved.\n\n%s\n\n";

#ifndef NEWLISP64
#ifdef SUPPORT_UTF8
char banner[]=
"newLISP v.10.7.5 32-bit on %s IPv4/6 UTF-8%s%s\n\n";
#else
char banner[]=
"newLISP v.10.7.5 32-bit on %s IPv4/6%s%s\n\n";
#endif
#else /* NEWLISP64 */
#ifdef SUPPORT_UTF8
char banner[]=
"newLISP v.10.7.5 64-bit on %s IPv4/6 UTF-8%s%s\n\n";
#else
char banner[]=
"newLISP v.10.7.5 64-bit on %s IPv4/6%s%s\n\n";
#endif 
#endif /* NEWLISP64 */

char banner2[]= ", options: newlisp -h";

void linkSource(char *, char *, char *);
char linkOffset[] = "&&&&@@@@";
char preLoad[] = 
#ifdef EMSCRIPTEN
    "(set (global 'module) (fn ($x) (load (append {/newlisp-js/} $x))))"
#else
    "(set (global 'module) (fn ($x) (load (append (env {NEWLISPDIR}) {/modules/} $x))))"
#endif
    "(context 'Tree) (constant 'Tree:Tree) (context MAIN)"
    "(define (Class:Class) (cons (context) (args)))";
void printHelpText(void);
#ifdef READLINE
char ** newlisp_completion (char * text, int start, int end);
#endif
/* --------------------- globals -------------------------------------- */

/* interactive command line */

int isTTY = FALSE;
int daemonMode = 0;

int noPromptMode = 0;
int forcePromptMode = 0;
int httpMode = 0;
int httpSafe = 0;
int evalSilent = 0;


#ifdef WINDOWS
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
INT MAX_CELL_COUNT = 0x10000000;
#else
INT MAX_CELL_COUNT = 0x800000000000000LL;
#endif
INT blockCount = 0;

CELL * firstFreeCell = NULL;

CELL * nilCell;
CELL * trueCell;
CELL * lastCellCopied;
CELL * countCell;
SYMBOL * nilSymbol;
SYMBOL * trueSymbol;
SYMBOL * starSymbol;
SYMBOL * plusSymbol;
SYMBOL * questionSymbol;
SYMBOL * atSymbol;
SYMBOL * currentFunc;
SYMBOL * argsSymbol;
SYMBOL * mainArgsSymbol;
SYMBOL * listIdxSymbol;
SYMBOL * itSymbol;
SYMBOL * sysxSymbol;
SYMBOL * countSymbol;
SYMBOL * beginSymbol;
SYMBOL * expandSymbol;

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

extern PRIMITIVE primitive[];

/* debugger in nl-debug.c */
extern char debugPreStr[];
extern char debugPostStr[];
extern CELL * debugPrintCell;

int traceFlag = 0;
int evalCatchFlag = 0;
int recursionCount = 0;

int prettyPrintPars = 0;
int prettyPrintCurrent = 0;
int prettyPrintFlags = 0;
int prettyPrintLength = 0;
char * prettyPrintTab = " ";
char * prettyPrintFloat = "%1.16g";
#define MAX_PRETTY_PRINT_LENGTH 80
UINT prettyPrintMaxLength =  MAX_PRETTY_PRINT_LENGTH;
int stringOutputRaw = TRUE;

#define pushLambda(A) (*(lambdaStackIdx++) = (UINT)(A))

int pushResultFlag = TRUE;

char startupDir[PATH_MAX]; /* start up directory, if defined via -w */
char * tempDir; /* /tmp on unix or geten("TMP") on Windows */
char logFile[PATH_MAX]; /* logFile, is define with -l, -L */

/* nl-filesys.c */
int pagesize;

/* ============================== MAIN ================================ */

#ifndef EMSCRIPTEN
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

#ifndef WINDOWS

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
#ifndef WINDOWS
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

#ifdef WINDOWS
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
#endif /* no EMSCRIPTEN */
 
char * which(char * name, char * buff)
{
char *path_list, *test, *tmp, *path_parsed;
struct stat filestat;
int count = 1;
int i, len, nlen;
int found = FALSE;

path_list = getenv("PATH");
if (!path_list) path_list = "/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin";

len = strlen(path_list);
nlen = strlen(name);
path_parsed = alloca(len + 1);
strncpy(path_parsed, path_list, len + 1);

test = path_parsed;
while (TRUE) 
    {
#ifdef WINDOWS
    tmp = strchr(test, ';');
#else
    tmp = strchr(test, ':');
#endif
    if (tmp == NULL) break;
    *tmp = 0;
    test = tmp + 1;
    count++;
    }   

test = path_parsed;
for (i = 0; i < count; i++) 
    {
    len = strlen(test);
    if((len + nlen + 2) > PATH_MAX) 
	    return(NULL);
    strncpy(buff, test, len + 1);
    buff[len] = '/';
    memcpy(buff + len + 1, name, nlen);
    buff[len + 1 + nlen] = 0;
    if (stat (buff, &filestat) == 0 && filestat.st_mode & S_IXUSR)
        {
        found = TRUE;
        break;
        }
    test += (len + 1);
    }

if(!found) return(NULL);
errno = 0;
return(buff);
}

#ifndef LIBRARY
void loadStartup(char * name)
{
char initFile[PATH_MAX];
char * envPtr;
int len;

/* normal newLISP start up */
if(strncmp(linkOffset + 4, "@@@@", 4) == 0)
    {
    if(getenv("HOME"))
        strncpy(initFile, getenv("HOME"), PATH_MAX - 16);
    else if(getenv("USERPROFILE"))
        strncpy(initFile, getenv("USERPROFILE"), PATH_MAX - 16);
    else if(getenv("DOCUMENT_ROOT"))
        strncpy(initFile, getenv("DOCUMENT_ROOT"), PATH_MAX - 16);

    len = strlen(initFile);
    memcpy(initFile + len, "/.", 2);
    memcpy(initFile + len + 2, INIT_FILE, 8);
    initFile[len + 2 + 8] = 0;
    if(loadFile(initFile, 0, 0, mainContext) == NULL)
        {
        envPtr = getenv("NEWLISPDIR");
        if(envPtr)
            {
            strncpy(initFile, envPtr, PATH_MAX - 16);
            len = strlen(envPtr);
            memcpy(initFile + len, "/", 1);
            memcpy(initFile + len + 1, INIT_FILE, 8);
            initFile[len + 1 + 8] = 0;
            loadFile(initFile, 0, 0, mainContext);      
            }
        }
    }
/* load part at offset no init.lsp or .init.lsp is loaded */
else
    {
#ifdef WINDOWS
	name = win_getExePath(alloca(MAX_PATH));
    loadFile(name, *(unsigned int *)linkOffset, 1, mainContext);
#else /* if not Win32 get full pathname of file in name */
    if(strchr(name, '/') == NULL) 
        if((name = which(name, alloca(PATH_MAX))) == NULL)
            {
            printf("%s: %s\n", strerror(ENOENT), name);
            exit(ENOENT);
            }
    loadFile(name, *(unsigned int *)linkOffset, 1, mainContext);
#endif
    }
}
#endif /* LIBRARY */

#ifdef _BSD
struct lconv    *localeconv(void);
char            *setlocale(int, const char *);  
#endif

void initLocale(void)
{
#ifndef ANDROID
struct lconv * lc;
#endif
char * locale;

#ifndef SUPPORT_UTF8
locale = setlocale(LC_ALL, "C");
#else
locale = setlocale(LC_ALL, "");
#endif

if (locale != NULL)
  stringOutputRaw = (strcmp(locale, "C") == 0);

#ifdef ANDROID
lc_decimal_point = '.';
#else
lc = localeconv();
lc_decimal_point = *lc->decimal_point;
#endif
}

/* set NEWLISPDIR only if not set already */
void initNewlispDir(void)
{
#ifdef WINDOWS
char * varValue;
char * newlispDir;
int len;

if(getenv("NEWLISPDIR") == NULL)
    {
    newlispDir = alloca(MAX_PATH);
    varValue = getenv("PROGRAMFILES");
    if(varValue != NULL)
        {
        len = strlen(varValue);
        strncpy(newlispDir, varValue, MAX_PATH - 12);
        memcpy(newlispDir + len, "/newlisp", 8);
        newlispDir[len + 8] = 0; 
        setenv("NEWLISPDIR", newlispDir, TRUE);
        }
    else setenv("NEWLISPDIR", "newlisp", TRUE);
    }
#else
if(getenv("NEWLISPDIR") == NULL)
    setenv("NEWLISPDIR", NEWLISPDIR, TRUE);
#endif
}

void initTempDir()
{
#ifdef WINDOWS
if((tempDir = getenv("TMP")) == NULL)
    {
    printf("Environment variable TMP not set, assuming /tmp .");
    tempDir = "/tmp";
    }
#else
#ifdef ANDROID
tempDir = "/data/tmp";
#else /* all UNIX */
tempDir = "/tmp";
#endif
#endif
return;
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

#ifndef WINDOWS
char ** MainArgs;
#endif 

CELL * getMainArgs(char * mainArgs[])
{
CELL * argList;
int idx = 0;

#ifndef WINDOWS
MainArgs = mainArgs;
#endif

argList = getCell(CELL_EXPRESSION);

while(mainArgs[idx] != NULL)
    addList(argList, stuffString(mainArgs[idx++]));

return(argList);
}

char * getCommandLine(int batchMode, int * length);
    
int main(int argc, char * argv[])
{
char command[MAX_COMMAND_LINE];
STREAM cmdStream = {NULL, NULL, 0, 0, 0};
char * cmd;
int idx;

#ifdef WINDOWS
WSADATA WSAData;
if(WSAStartup(MAKEWORD(2,2), &WSAData) != 0)
    {
    printf("Winsocket initialization failed\n");
    exit(-1);
    }
pagesize = 4096;

/* replace '_CRT_fmode = _O_BINARY' in nl-filesys.c for 10.4.8, thanks to Kosh */
_setmode(_fileno(stdin), _O_BINARY);
_setmode(_fileno(stdout), _O_BINARY);
_setmode(_fileno(stderr), _O_BINARY);
#endif

#ifdef SUPPORT_UTF8
opsys += 128;
#endif

#ifdef NEWLISP64
opsys += 256;
#endif

#ifdef FFI
opsys += 1024;
initFFI();
#endif

#ifndef WINDOWS
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
initTempDir();

IOchannel = stdin;
bigEndian = (*((char *)&bigEndian) == 0);

initStacks();
initialize();
initDefaultInAddr(); 

#ifdef WINDOWS
#ifdef SUPPORT_UTF8
 {
   /*
     command line parameter is MBCS.
     MBCS -> Unicode(UTF-16) -> UTF-8
   */
   char **argv_utf8 = allocMemory((argc + 1)* sizeof(char *)) ;
   {
   for(idx = 0 ; idx<argc ; idx++)
      {
      WCHAR *p_argvW = ansi_mbcs_to_utf16(argv[idx]) ;
      char *p_argvU = utf16_to_utf8(p_argvW) ;
      argv_utf8[idx] = p_argvU ;
      }
   argv_utf8[idx] = NULL ;
   argv = argv_utf8 ;
   }
 }
#endif
#endif
mainArgsSymbol->contents = (UINT)getMainArgs(argv);

if((errorReg = setjmp(errorJump)) != 0) 
    {
    if((errorEvent != nilSymbol) || (errorReg == ERR_USER_RESET)) 
        executeSymbol(errorEvent, NULL, NULL);
    else exit(-1);
    goto AFTER_ERROR_ENTRY;
    }

setupAllSignals();

sysEvalString(preLoad, mainContext, nilCell, EVAL_STRING);

/* loading of init.lsp will be suppressed with -n, -x or -h as first option
   but is never done when program is link.lsp'd */

if(argc < 2 || (strncmp(argv[1], "-n", 2) && strncmp(argv[1], "-h", 2)) )
    {
    if(!(argc >= 2 && strcmp(argv[1], "-x") == 0)) 
        loadStartup(argv[0]);
    }
   
errno = 0;

if(realpath(".", startupDir) == NULL)
    fatalError(ERR_IO_ERROR, 0, 0);

for(idx = 1; idx < argc; idx++)
    {
    if(strncmp(argv[idx], "-c", 2) == 0)
        {
        noPromptMode = TRUE;
        continue;
        }

    if(strncmp(argv[idx], "-C", 2) == 0)
        {
        forcePromptMode = TRUE;
        continue;
        }

    if(strncmp(argv[idx], "-http", 5) == 0)
        {
        noPromptMode = TRUE;
        httpMode = TRUE;
        if(strncmp(argv[idx], "-http-safe", 10) == 0)
            httpSafe = TRUE;
        continue;
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
            daemonMode = TRUE;

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

    if(strncmp(argv[idx], "-l", 2) == 0 || strncmp(argv[idx], "-L", 2) == 0)
        {
        logTraffic = (strncmp(argv[idx], "-L", 2) == 0) ? LOG_MORE : LOG_LESS;
        if(realpath(getArg(argv, argc, &idx), logFile) == NULL)
            close(openFile(logFile, "w", 0));

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
        continue;
        }

    if(strcmp(argv[idx], "-v") == 0)
        {
        varPrintf(OUT_CONSOLE, banner, OSTYPE, LIBFFI, ".");
        exit(0);
        }

    if(strncmp(argv[idx], "-e", 2) == 0)
        {
        executeCommandLine(getArg(argv, argc, &idx), OUT_CONSOLE, &cmdStream);
        exit(0);
        }       

    if(strncmp(argv[idx], "-x", 2) == 0)
        {
        if(argc == 4)
            linkSource(argv[0], argv[idx + 1], argv[idx + 2]);
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
        varPrintf(OUT_CONSOLE, banner, OSTYPE, LIBFFI, banner2);
    }
else
    {
#ifdef WINDOWS
    if(!IOchannelIsSocketStream) 
#endif
        setbuf(IOchannel,0);
    if(forcePromptMode)
        varPrintf(OUT_CONSOLE, banner, OSTYPE, LIBFFI, banner2);
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
rl_attempted_completion_function = (char ** (*) (const char *, int, int))newlisp_completion;
#if defined(LINUX) || defined(_BSD) || defined(KFREEBSD)
/* in Bash .inputrc put 'set blink-matching-paren on' */
rl_set_paren_blink_timeout(300000); /* 300 ms */
#endif
#endif

while(TRUE)
    {
    cleanupResults(resultStack);
    if(isTTY)  
        {
        cmd = getCommandLine(FALSE, NULL);
        executeCommandLine(cmd, OUT_CONSOLE, &cmdStream);
        free(cmd);
        continue;
        }

    if(IOchannel != stdin || forcePromptMode) 
        varPrintf(OUT_CONSOLE, "%s", prompt());

    /* daemon mode timeout if nothing read after accepting connection */
    if(connectionTimeout && IOchannel && daemonMode)
        {
#ifdef WINDOWS
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
        if(!daemonMode)  exit(1);
        if(IOchannel != NULL) fclose(IOchannel);
        setupServer(1);
        continue;
        }

    executeCommandLine(command, OUT_CONSOLE, &cmdStream);
    }

#ifndef WINDOWS
return 0;
#endif
}
#endif /* not LIBRARY */

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

char ** completion_matches(const char * text,  char * (*commands)(const char *, int)); 

char ** newlisp_completion (char * text, int start, int end)
{
return(completion_matches(text,  (char * (*) (const char *, int) )command_generator));
}

#endif /* READLINE */


char * getCommandLine(int batchMode, int * length)
{
char * cmd;
int len;

#ifndef READLINE
if(!batchMode) varPrintf(OUT_CONSOLE, "%s", prompt());
cmd = calloc(MAX_COMMAND_LINE + 4, 1);
if(fgets(cmd, MAX_COMMAND_LINE - 1, IOchannel) == NULL) 
    {
    puts("");
    exit(0);
    }
len = strlen(cmd);
/* cut off line terminators  left by fgets */
*(cmd + len - LINE_FEED_LEN) = 0;
len -= LINE_FEED_LEN; /* v.10.6.2 */
#else /*  READLINE */
int errnoSave = errno;
if((cmd = readline(batchMode ? "" : prompt())) == NULL) 
    {
    puts("");
    exit(0);
    }
errno = errnoSave; /* reset errno, set by readline() */
len = strlen(cmd);
if(len > 0) 
    add_history(cmd);
#endif  

if(length != NULL) *length = len;
return(cmd);
}

#ifndef LIBRARY

void printHelpText(void)
{
varPrintf(OUT_CONSOLE, copyright, 
    "usage: newlisp [file | url ...] [options ...] [file | url ...]\n\noptions:");
varPrintf(OUT_CONSOLE, 
    "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n",
    " -h this help (no init.lsp)",
    " -n no init.lsp (must be first)",
    " -x <source> <target> link (no init.lsp)",
    " -v version",
    " -s <stacksize>",
    " -m <max-mem-MB> cell memory",
    " -e <quoted lisp expression>",
    " -l <path-file> log connections",
    " -L <path-file> log all",
    " -w <working dir>",
    " -c no prompts, net-eval, HTTP",
    " -C force prompts",
    " -t <usec-server-timeout>",
    " -p <port-no>",
    " -d <port-no> daemon mode",
    " -http only mode",
    " -http-safe safe mode",
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

#ifdef WINDOWS
else    IOchannelIsSocketStream = TRUE; 

if(!IOchannelIsSocketStream)
#endif
    setbuf(IOchannel,0);

if(!reconnect && !noPromptMode)
    varPrintf(OUT_CONSOLE, banner, OSTYPE, LIBFFI, ".");
}

#endif /* ifndef LIBRARY */

char * prompt(void)
{
char * contextName = "";
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
    contextName = currentContext->name;

if(traceFlag & TRACE_SIGINT) 
    {
    traceFlag &= ~TRACE_SIGINT;
    longjmp(errorJump, errorReg);
    }
    
if(traceFlag && !(traceFlag & TRACE_PRINT_EVAL))
    snprintf(string, 63, "%d %s> ", recursionCount, contextName);
else
    snprintf(string, 63, "%s> ", contextName);

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
#ifdef XML_SUPPORT
xmlTags = NULL; /* force recreation */
#endif
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
int len;

memset(buff + MAX_COMMAND_LINE -2, 0, 2);

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
        writeLog(command, TRUE);
#ifndef LIBRARY
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
#endif
    else if(!httpMode) goto EXEC_COMMANDLINE;
    return;
    }

if(httpMode) goto RETURN_BATCHMODE;

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
            cmd = getCommandLine(TRUE, &len);
            if(len > (MAX_COMMAND_LINE - 3))
                len = MAX_COMMAND_LINE - 3;
            memcpy(buff, cmd, len);
            memcpy(buff + len, LINE_FEED, LINE_FEED_LEN);
            buff[len + LINE_FEED_LEN] = 0; 
            free(cmd);
            }
        else
            if(fgets(buff, MAX_COMMAND_LINE - 1, IOchannel) == NULL) break;
        if( (memcmp(buff, "[/cmd]", 6) == 0 && batchMode == 2) || 
                (batchMode == 1 && (*buff == '\n' || *buff == '\r' || *buff == 0)))
            {
            if(logTraffic) 
                writeLog(cmdStream->buffer, 0);
            makeStreamFromString(&stream, cmdStream->buffer);
            evaluateStream(&stream, outDevice, 0);
            return;
            }
        writeStreamStr(cmdStream, buff, 0);
        }
    closeStrStream(cmdStream);
RETURN_BATCHMODE:
    if(!daemonMode)  exit(1);
    if(IOchannel != NULL) fclose(IOchannel);
#ifndef LIBRARY
    setupServer(1);
#endif
    return;
    }

if(logTraffic == LOG_LESS) writeLog(command, TRUE);
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
CELL * xlate;
UINT * resultIdxSave = resultStackIdx;
int result = TRUE;

while(result)
    {
    pushResult(program = getCell(CELL_QUOTE));
    result = compileExpression(stream, program);
    if(readerEvent != nilSymbol && result)
        {
        --resultStackIdx; /* program cell consumed by executeSymbol() */
        executeSymbol(readerEvent, program, &xlate);
        pushResult(program = makeCell(CELL_QUOTE, (UINT)xlate));
        }
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
                writeLog("-> ", 0);
                printCell(eval, TRUE, OUT_LOG);
                writeLog("", TRUE);
                }
#ifdef EMSCRIPTEN
            if(outDevice) fflush(NULL);
#endif
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
UINT * resultIdxSave = resultStackIdx;

if(symbol == nilSymbol || symbol == trueSymbol || symbol == NULL)   return(0);
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


/* -------------------------- initialization -------------------- */

void initialize()
{
SYMBOL * symbol;
CELL * pCell;
char  symName[8];
int i;

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
listIdxSymbol = translateCreateSymbol("$idx", CELL_NIL, mainContext, TRUE);
itSymbol = translateCreateSymbol("$it", CELL_NIL, mainContext, TRUE);
countSymbol = translateCreateSymbol("$count", CELL_NIL, mainContext, TRUE);
sysxSymbol = translateCreateSymbol("$x", CELL_NIL, mainContext, TRUE);
beginSymbol = translateCreateSymbol("begin", CELL_NIL, mainContext, TRUE);
expandSymbol = translateCreateSymbol("expand", CELL_NIL, mainContext, TRUE);

symbol = translateCreateSymbol("ostype", CELL_STRING, mainContext, TRUE);
symbol->flags = SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
symbol->contents = (UINT)stuffString(OSTYPE);

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
listIdxSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
itSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
countSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN | SYMBOL_PROTECTED;
sysxSymbol->flags |= SYMBOL_GLOBAL | SYMBOL_BUILTIN;

countCell = stuffInteger(0);
countSymbol->contents = (UINT)countCell ;
argsSymbol->contents = (UINT)getCell(CELL_EXPRESSION);
objSymbol.contents = (UINT)nilCell;
objSymbol.context = mainContext;
objCell = nilCell;

/* init signal handlers */
for(i = 0; i < 32; i++)
  symHandler[i] = nilSymbol;

/* init system wide string streams */
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
#ifdef ISO_C90
CELL * result;
UINT * resultIdxSave = resultStackIdx;
CELL * args = NULL;
CELL * pCell = NULL;
SYMBOL * newContext = NULL;
SYMBOL * sPtr = NULL;
#endif

symbolCheck = NULL;
stringCell = NULL;

if(isSelfEval(cell->type))
    return(cell);

if(cell->type == CELL_SYMBOL || cell->type == CELL_CONTEXT)
    {
    symbolCheck = (SYMBOL *)cell->contents;
    return((CELL *)symbolCheck->contents);
    }

#ifndef ISO_C90
CELL * result;
UINT * resultIdxSave = resultStackIdx;
CELL * args = NULL;
CELL * pCell = NULL;
SYMBOL * newContext = NULL;
SYMBOL * sPtr = NULL;
#endif

switch(cell->type)
    {
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

            /* check for 'default' functor
            * allow function call with context name, i.e: (ctx)
            * assumes that a ctx:ctx contains a function
            */
        if(pCell->type == CELL_CONTEXT)
            {
            newContext = (SYMBOL *)pCell->contents;
            sPtr= translateCreateSymbol(newContext->name, CELL_NIL, newContext, TRUE);
            pCell = (CELL *)sPtr->contents;

            /* if the default functor contains nil, it works like a hash function */
            if(isNil(pCell))
                {
                result = evaluateNamespaceHash(args->next, newContext);
                break;
                }
            }

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
            pushLambda(cell);
            result = evaluateLambda((CELL *)pCell->contents, args->next, newContext); 
            --lambdaStackIdx; 
            break; 
            }
        
        if(pCell->type == CELL_FEXPR)
            { 
            pushLambda(cell);
            result = evaluateLambdaMacro((CELL *)pCell->contents, args->next, newContext);
            --lambdaStackIdx; 
            break;
            }
#ifndef EMSCRIPTEN
        /* simple ffi with CDECL or DLL and extended libffi */
        if(pCell->type & IMPORT_MASK) 
            {
            result = executeLibfunction(pCell, args->next);  
            break;
            }
#endif
        /* implicit indexing or resting for list, array or string 
        */
        if(args->next != nilCell)
            {
            /* implicit indexing array */
            if(pCell->type == CELL_EXPRESSION)
                {
                if(!sPtr) sPtr = symbolCheck;
                result = implicitIndexList(pCell, args->next);
                symbolCheck = sPtr;
                pushResultFlag = FALSE;
                }

            /* implicit indexing array */
            else if(pCell->type == CELL_ARRAY)
                {
                if(!sPtr) sPtr = symbolCheck;
                result = implicitIndexArray(pCell, args->next);
                symbolCheck = sPtr;
                pushResultFlag = FALSE;
                }   

            /* implicit indexing string */
            else if(pCell->type == CELL_STRING)
                {
                if(sPtr || (sPtr = symbolCheck))
                    {
                    result = implicitIndexString(pCell, args->next);
                    /* result is always a copy */
                    pushResult(result);
                    symbolCheck = sPtr;
                    --recursionCount;
                    return(result);
                    }
                else
                    result = implicitIndexString(pCell, args->next); 
                }

            /* implicit resting for lists and strings */
            else if(isNumber(pCell->type))
                result = implicitNrestSlice(pCell, args->next);
    
            else 
                result = errorProcExt(ERR_INVALID_FUNCTION, cell);                              
            } /* implixit indexing, resting on lists and strings */
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
if(pCell->type == CELL_STRING || isNumber(pCell->type))
    {
    /* set contents */
    if(args->next != nilCell)
        {
        sPtr = makeSafeSymbol(pCell, newContext, TRUE);
        
        itSymbol->contents = sPtr->contents; 
        /* itSymbol may occur in evaluateExpression() */
        itSymbol->contents = (UINT)copyCell(evaluateExpression(args->next));
        deleteList((CELL *)sPtr->contents);
        sPtr->contents = itSymbol->contents;
        itSymbol->contents = (UINT)nilCell;
        
        if(isNil((CELL *)sPtr->contents))
            {
            deleteAndFreeSymbol(sPtr, FALSE);
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
        if(pCell->type == CELL_STRING || isNumber(pCell->type))
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
    return(associationsFromTree(newContext));

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
        (SYMBOL*)contextCell->contents, /* contextPtr */
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

/* put unassigned args in protected $args */
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
while (TRUE)
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
            else break;
            }
        else break;
        }
  else break;

  if(isProtected(symbol->flags))
    return(errorProcExt(ERR_SYMBOL_PROTECTED, local));

  pushEnvironment(symbol->contents);
  pushEnvironment((UINT)symbol);
  symbol->contents = (UINT)copyCell(arg);
  local = local->next;
  arg = arg->next;
  localCount++;
  }

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
    cell = cell->next = stuffInteger(va_arg(ap, UINT));

va_end(ap);

return(list);
}

#ifdef BIGINT
CELL * stuffBigint(char * token)
{
int len;
CELL * cell;

cell = getCell(CELL_BIGINT);
cell->contents = (UINT)strToBigint(token, strlen(token), &len);
cell->aux = len + 1;

return(cell);
}
#endif

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

CELL * stuffFloat(double floatVal)
{
CELL * cell;

cell = getCell(CELL_FLOAT);
#ifndef NEWLISP64
*(double *)&cell->aux = floatVal;
#else
*(double *)&cell->contents = floatVal;
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
int len = 0;

while(list != nilCell)
    {
    ++len;
    list = list->next;
    }
offset = len + offset;
if(offset < 0) 
    errorProc(ERR_LIST_INDEX_INVALID);

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
#ifdef ISO_C90
CELL * newCell;
CELL * list;
UINT len;
#endif

/* avoids copy if cell on resultStack */
if(cell == (CELL *)*(resultStackIdx))
    {
    if(cell != nilCell && cell != trueCell)
        return(popResult());
    }

#ifndef ISO_C90
CELL * newCell;
CELL * list;
UINT len;
#endif

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
#ifdef BIGINT
else if(cell->type == CELL_BIGINT)
    {
    newCell->contents = (UINT)allocMemory((UINT)cell->aux * sizeof(int));
    memcpy((void *)newCell->contents, (void*)cell->contents, (UINT)cell->aux * sizeof(int));
    }
#endif

return(newCell);
}


/* this routine must be called with the list head
   if copying with envelope call copyCell() instead */
CELL * copyList(CELL * cell)
{
#ifdef ISO_C90
CELL * firstCell;
CELL * newCell;
#endif

if(cell == nilCell) 
    {
    lastCellCopied = nilCell;
    return(cell);
    }

#ifndef ISO_C90
CELL * firstCell;
CELL * newCell;
#endif

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

    else if(cell->type == CELL_STRING || cell->type == CELL_DYN_SYMBOL 
#ifdef BIGINT
                || cell->type == CELL_BIGINT
#endif
            )
        freeMemory( (void *)cell->contents);
    
    /* free cell changes in 10.6.3 */
    if(cell == nilCell || cell == trueCell) 
        cell = cell->next;
    else    
        {
        next = cell->next;
        cell->type = CELL_FREE;
        cell->next = firstFreeCell;
        firstFreeCell = cell;
        --cellCount;
        cell = next;
        }

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

if(cellCount > MAX_CELL_COUNT - MAX_BLOCK)  
    {
    printErrorMessage(ERR_NOT_ENOUGH_MEMORY, NULL, 0);
    exit(ERR_NOT_ENOUGH_MEMORY);
    }

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
++ blockCount;
}


/* Return unused blocks to OS, this is normally only called under error 
   conditions but can also be forced issuing a (reset nil)

   Older versions also did a complete cell mark and sweep. Now all
   error conditons clean out allocated cells and memory before doing
   the longjmp().
*/

/* not used, not tested
void freeAllCells()
{
CELL * blockPtr = cellMemory;
int i, j;

for(i = 0; i < blockCount; i++)
    {
    for(j = 0; j < MAX_BLOCK; j++)
        {
        if(*(UINT *)blockPtr != CELL_FREE)
            {
            deleteList(blockPtr);
            }
        blockPtr++;
        }
    blockPtr = blockPtr->next;
    }        
}
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
        --blockCount;
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
        if(!newlispLibConsoleFlag)
            {
            writeStreamStr(&libStrStream, buffer, 0);
            freeMemory(buffer);
            fflush(NULL);
            return;
            }
        else
#endif
        if(IOchannel == stdin)
            {
            printf("%s", buffer);
#if defined(MAC_OSX) || defined(_BSD) /* 10.7.3 */
            fflush(NULL);
#else
            if(!isTTY) fflush(NULL);
#endif
            }
        else if(IOchannel != NULL) 
            fprintf(IOchannel, "%s", buffer);
        break;
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
#ifdef BIGINT
char * ptr;
#endif

if(cell == debugPrintCell)
    varPrintf(device, "%s", debugPreStr);

switch(cell->type)
    {
    case CELL_NIL:
        varPrintf(device, "nil"); break;

    case CELL_TRUE:
        varPrintf(device, "true"); break;
    
    case CELL_LONG:
        varPrintf(device,"%"PRIdPTR, cell->contents); break;
#ifndef NEWLISP64
    case CELL_INT64:
        varPrintf(device,"%"PRId64, *(INT64 *)&cell->aux); break;
#endif /* NEWLISP64 */
#ifdef BIGINT
    case CELL_BIGINT:
        ptr = bigintToDigits((int *)cell->contents, cell->aux - 1, 48, NULL);        
        varPrintf(device, "%sL", ptr);
        free(ptr);
        break;
#endif
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
        varPrintf(device,"%s@%lX", (char *)cell->aux, cell->contents);
        break;
    case CELL_IMPORT_CDECL:
    case CELL_IMPORT_FFI:
#if defined(WINDOWS) || defined(CYGWIN)
    case CELL_IMPORT_DLL:
#endif

#ifdef FFI
        if(cell->type == CELL_IMPORT_FFI)
            varPrintf(device,"%s@%lX", (char *)((FFIMPORT *)cell->aux)->name,
                                                 cell->contents);
        else
            varPrintf(device,"%s@%lX", (char *)cell->aux, cell->contents);
#else
        varPrintf(device,"%s@%lX", (char *)cell->aux, cell->contents);
#endif
        break;
    
    case CELL_QUOTE:
        varPrintf(device, "'");
        prettyPrintFlags |= PRETTYPRINT_DOUBLE;
        printCell((CELL *)cell->contents, printFlag, device);
        break;
    
    case CELL_EXPRESSION:
    case CELL_LAMBDA:
    case CELL_FEXPR:
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

if(cell == debugPrintCell)
    varPrintf(device, "%s", debugPostStr);

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
        case '\b': varPrintf(device,"\\b"); break;
        case '\f': varPrintf(device,"\\f"); break;
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
else if(cell->type == CELL_FEXPR) 
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

for(i = 0; i < prettyPrintPars; i++) 
    varPrintf(device, "%s", prettyPrintTab);

prettyPrintLength = prettyPrintCurrent = prettyPrintPars;
prettyPrintFlags |= PRETTYPRINT_DOUBLE;
}


void printSymbol(SYMBOL * sPtr, UINT device)
{
CELL * cell;
CELL * list = NULL;
char * setStr;
size_t offset, len;

prettyPrintCurrent = prettyPrintPars = 1;
prettyPrintLength = 0;
prettyPrintFlags &= ~PRETTYPRINT_DOUBLE;

if(sPtr->flags & SYMBOL_PROTECTED)
    setStr = "(constant ";
else
    setStr = "(set ";

switch(symbolType(sPtr))
    {
    case CELL_PRIMITIVE:
    case CELL_IMPORT_CDECL:
    case CELL_IMPORT_FFI:
#if defined(WINDOWS) || defined(CYGWIN) 
    case CELL_IMPORT_DLL:
#endif
        break;
    case CELL_SYMBOL:
    case CELL_DYN_SYMBOL:
        varPrintf(device, "%s", setStr);
        printSymbolNameExt(device, sPtr);
        varPrintf(device,"'");
        printCell((CELL *)sPtr->contents, TRUE, device);
        varPrintf(device, ")");
        break;
    case CELL_ARRAY:
    case CELL_EXPRESSION:
        varPrintf(device, "%s", setStr);
        printSymbolNameExt(device, sPtr);
        cell = (CELL *)sPtr->contents;

        if(symbolType(sPtr) == CELL_ARRAY)
            {
            varPrintf(device, "(array ");
            printArrayDimensions(cell, device);
            varPrintf(device, "(flat ");
            list = cell = arrayList(cell, TRUE);
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
    case CELL_FEXPR:
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
        varPrintf(device, "%s", setStr);
        printSymbolNameExt(device, sPtr);
        cell = (CELL *)sPtr->contents;
        if(cell->type == CELL_STRING && cell->aux > MAX_STRING) /* size > 2047 */
            {
            varPrintf(device, "%s ", "(append ");
            offset = 0;
            while(offset < cell->aux - 1)
                {
                varPrintf(device, "%s  ", LINE_FEED); 
                len = (cell->aux - 1 - offset);
                len = len > 72 ? 72 : len;
                printString((char *)(cell->contents + offset), device, len); 
                offset += len;
                }
            varPrintf(device, "))");
            break; 
            }
        printCell(cell, TRUE, device);
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
    if(symbolType(sPtr) == CELL_LAMBDA || symbolType(sPtr) == CELL_FEXPR)
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
    "invalid option",               /* 33 */
    "throw without catch",          /* 34 */
    "problem loading library",      /* 35 */
    "import function not found",    /* 36 */
    "symbol is protected",          /* 37 */
    "number out of range",          /* 38 */
    "regular expression",           /* 39 */
    "end of text [/text] tag",      /* 40 */
    "mismatch in number of arguments",  /* 41 */
    "problem in format string",     /* 42 */
    "data type and format don't match", /* 43 */
    "invalid parameter",            /* 44 */
    "invalid parameter: 0.0",       /* 45 */
    "invalid parameter: NaN",       /* 46 */
    "invalid UTF8 string",          /* 47 */
    "illegal parameter type",       /* 48 */
    "symbol not in MAIN context",   /* 49 */
    "symbol not in current context", /* 50 */
    "target cannot be MAIN",        /* 51 */
    "invalid list index",           /* 52 */
    "array index out of bounds",    /* 53 */
    "invalid string index",         /* 54 */
    "nesting level to deep",        /* 55 */
    "list reference changed",       /* 56 */
    "invalid syntax",               /* 57 */
    "user error",                   /* 58 */
    "user reset -",                 /* 59 */
    "received SIGINT -",            /* 60 */
    "function is not reentrant",    /* 61 */
    "not allowed on local symbol",  /* 62 */
    "no reference found",           /* 63 */
    "list is empty",                /* 64 */
    "I/O error",                    /* 65 */
    "no working directory found",   /* 66 */
    "invalid PID",                  /* 67 */
    "cannot open socket pair",      /* 68 */
    "cannot fork process",          /* 69 */
    "no comm channel found",        /* 70 */
    "ffi preparation failed",       /* 71 */
    "invalid ffi type",             /* 72 */
    "ffi struct expected",          /* 73 */
    "bigint type not applicable",   /* 74 */
    "not a number or infinite",     /* 75 */
    "cannot convert NULL to string",/* 76 */
    NULL
    };


void errorMissingPar(STREAM * stream)
{
char str[48]; 
snprintf(str, 40, "...%.40s", ((char *)((stream->ptr - stream->buffer) > 40 ? stream->ptr - 40 : stream->buffer)));
errorProcExt2(ERR_MISSING_PAR, stuffString(str));
}

CELL * errorProcAll(int errorNumber, CELL * expr, int deleteFlag)
{
if(!traceFlag) fatalError(errorNumber, expr, deleteFlag);
printErrorMessage(errorNumber, expr, deleteFlag);
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
#ifndef LIBRARY
closeTrace();
#endif
longjmp(errorJump, errorReg);
}

void printErrorMessage(UINT errorNumber, CELL * expr, int deleteFlag)
{
CELL * lambdaFunc;
CELL * lambdaExpr;
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
    lambdaExpr = (CELL *)*(--stackIdx);
    lambdaFunc = (CELL *)lambdaExpr->contents;
    if(lambdaFunc->type == CELL_SYMBOL)
        {
        writeStreamStr(&errorStream, LINE_FEED, 0);
        writeStreamStr(&errorStream, "called from user function ", 0);
        context = ((SYMBOL *)lambdaFunc->contents)->context;
        if(context != mainContext)
          {
          writeStreamStr(&errorStream, context->name, 0);
          writeStreamStr(&errorStream, ":", 0);
          }
        /* writeStreamStr(&errorStream, ((SYMBOL *)lambdaFunc->contents)->name, 0); */
        printCell(lambdaExpr, (errorNumber != ERR_USER_ERROR), (UINT)&errorStream); /* 10.6.3 */
        }
    }

if(!(traceFlag & TRACE_SIGINT)) evalFunc = NULL; 
parStackCounter = prettyPrintPars = 0;

if(evalCatchFlag && !((traceFlag & TRACE_SIGINT) 
        || (traceFlag & TRACE_IN_DEBUG))) return;

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

if(traceFlag & TRACE_PRINT_EVAL) tracePrint(errorStream.buffer, NULL);
}


extern UINT * lambdaStack;
extern UINT * lambdaStackIdx;
CELL * p_history(CELL * params)
{
CELL * history;
CELL * lambdaFunc;
CELL * lambdaExpr;
UINT * stackIdx = lambdaStackIdx;

history = getCell(CELL_EXPRESSION);
while(stackIdx > lambdaStack)
    {
    lambdaExpr = (CELL *)*(--stackIdx);
    lambdaFunc = (CELL *)lambdaExpr->contents;
    if(lambdaFunc->type == CELL_SYMBOL)
		{
		if(getFlag(params))
			addList(history, copyCell(lambdaExpr));
		else
			addList(history, copyCell((CELL*)lambdaExpr->contents));
		}
    }

return(history);
}


/* --------------------------- load source file ------------------------- */


CELL * loadFile(char * fileName, UINT offset, int linkFlag, SYMBOL * context)
{
CELL * result;
STREAM stream;
int errNo, sourceLen;
jmp_buf errorJumpSave;
SYMBOL * contextSave;
#ifdef LOAD_DEBUG
int i;
#endif

contextSave = currentContext;
currentContext = context;
if(linkFlag)
    sourceLen = *((int *) (linkOffset + 4));
else sourceLen = MAX_FILE_BUFFER;

#ifndef EMSCRIPTEN
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
#endif

if(makeStreamFromFile(&stream, fileName, sourceLen + 4 * MAX_STRING, offset) == 0) 
    return(NULL);

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


void linkSource(char * pathname, char * source, char * target)
{
int sourceLen;
char * buffer;
int size, offset = 0;
char * ptr;

#ifdef WINDOWS
/* gets full path of currently executing newlisp.exe */
pathname = win_getExePath(alloca(PATH_MAX));
#else /* Unix */
if(strchr(pathname, '/') == NULL) 
    pathname = which(pathname, alloca(PATH_MAX));
#endif

size = readFile(pathname, &buffer);
sourceLen = (size_t)fileSize(source);

if(errno) 
    {
    printf("%s\n", strerror(errno));
    exit(errno);
    }

ptr = buffer;

if(strncmp(linkOffset + 4, "@@@@", 4) != 0) return; /* already linked */
do  {
    offset = searchBuffer(ptr, size - (ptr - buffer) , "@@@@", 4, 1);
    ptr = ptr + offset + 4;
    } while (strncmp(ptr - 8, "&&&&", 4) != 0); /* the linkOffset */

offset = (ptr - buffer - 8);
*(int *)(buffer + offset) = (int)size;
*(int *)(buffer + offset + 4) = (int)sourceLen;
writeFile(target, buffer, size, "w");
readFile(source, &buffer);
writeFile(target, buffer, sourceLen, "a");

free(buffer);
}

/* -------------------------- parse / compile -----------------------------

   Takes source in a string stream and and envelope cell and compiles
   newLISP source into an internal LISP cell structure tree. The tree
   can be decompiled to source at any time and is processed by the
   evaluateExpression() function.

*/
int references(SYMBOL *, int);
int compileExpression(STREAM * stream, CELL * cell)
{
char token[MAX_STRING + 4];
double floatNumber;
CELL * newCell;
CELL * contextCell;
CELL * preCell;
SYMBOL * contextPtr;
SYMBOL * sPtr;
int listFlag, tklen;
char * lastPtr;
int errnoSave;
INT64 number;

listFlag = TRUE; /* cell is either quote or list envelope */

GETNEXT:
lastPtr = stream->ptr;
switch(getToken(stream, token, &tklen))
    {
    case TKN_ERROR:
        errorProcExt2(ERR_EXPRESSION, stuffStringN(lastPtr, 
            (strlen(lastPtr) < 60) ? strlen(lastPtr) : 60));
        return(FALSE);

    case TKN_EMPTY:
        if(parStackCounter != 0) errorMissingPar(stream);
        return(FALSE);

    case TKN_CHARACTER:
        newCell = stuffInteger((UINT)token[0]);
        break;

    case TKN_HEX:
        newCell = stuffInteger64((INT64)strtoull(token,NULL,0));
        break;

    case TKN_BINARY:
        newCell = stuffInteger64((INT64)strtoull(&token[2],NULL,2));
        break;
    
    case TKN_OCTAL:
        newCell = stuffInteger64(strtoll(token,NULL,0));
        break;

    case TKN_DECIMAL:
        errnoSave = errno;
        errno = 0;
#ifdef BIGINT
        if(*(token + tklen - 1) == 'L')
            {
            newCell = stuffBigint(token);
            break;
            }
#endif

#ifndef NEWLISP64
        number = (INT64)strtoll(token, NULL, 0);
#else
        number = strtoll(token, NULL, 0);
#endif
        
#ifdef BIGINT
        if(errno == ERANGE)
            {
            newCell = stuffBigint(token);
            errno = errnoSave;
            break;
            }
#endif

        newCell = stuffInteger64(number);
        errno = errnoSave;
        break;

    case TKN_FLOAT:
        floatNumber = (double)atof(token);
        newCell = stuffFloat(floatNumber);
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
                return(FALSE);
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
                return(FALSE);
                }
            cell->type =  CELL_FEXPR;
            cell->aux = (UINT)nilCell;
            goto GETNEXT;
            }
        else if(strncmp(token, "[text]", 6) == 0) 
            {
            newCell = makeCell(CELL_STRING, (UINT)readStreamText(stream, &tklen));
            if(newCell->contents == 0)
                {
                deleteList(newCell); 
                errorProc(ERR_TEXT_END_TAG);
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
        compileExpression(stream, newCell);
        break;

    case TKN_LEFT_PAR:
        ++parStackCounter;
        newCell = getCell(CELL_EXPRESSION);
        compileExpression(stream, newCell);
        if(((CELL *)newCell->contents)->type == CELL_SYMBOL)
            {
            sPtr = (SYMBOL *)((CELL *)newCell->contents)->contents;
            /* macro expansion */
            if(sPtr->flags & SYMBOL_MACRO)     
                {
                preCell = copyCell(evaluateExpression(newCell));
                deleteList(newCell);
                newCell = preCell;
                }
            }
        break;

    case TKN_RIGHT_PAR:
        if(parStackCounter == 0) errorMissingPar(stream);
        --parStackCounter;
        cell->next = nilCell;
        return(TRUE);

    default:
        errorProcExt2(ERR_EXPRESSION, stuffString(lastPtr));
        return(FALSE);

    }

linkCell(cell, newCell, listFlag);

if(cell->type == CELL_QUOTE && listFlag == TRUE)
    return(TRUE);

listFlag = FALSE;
cell = newCell;

if(parStackCounter != 0)
    {
    if(*(stream->ptr) != 0) goto GETNEXT;
    else errorMissingPar(stream);
    }

return(FALSE);
}


void linkCell(CELL * left, CELL * right, int linkFlag)
{
if(linkFlag == 0) left->next = right;
else left->contents = (UINT)right;
}

int getToken(STREAM * stream, char * token, int * ptr_len)
{
char *tkn;
char chr;
int tknLen;
#ifdef SUPPORT_UTF8
int len;
#endif
int floatFlag;
int bracketBalance;
char buff[8];

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
    if(isDigit((unsigned char)*(stream->ptr + 1)) 
        || *(stream->ptr + 1) == lc_decimal_point ) /* added 10.4.8 to allow -.9 */
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
        return(TKN_OCTAL);
        }
        
    while(isDigit((unsigned char)*stream->ptr) && tknLen < MAX_DIGITS)
        *(tkn++) = *(stream->ptr++), tknLen++;
    
    if(toupper(*stream->ptr) == 'X' && token[0] == '0')
        {
        *(tkn++) = *(stream->ptr++), tknLen++;
        while(isxdigit((unsigned char)*stream->ptr) && tknLen < MAX_HEX_NO)
            *(tkn++) = *(stream->ptr++), tknLen++;
        *tkn = 0;
        return(TKN_HEX);
        }

    if(toupper(*stream->ptr) == 'B' && token[0] == '0')
        {
        *(tkn++) = *(stream->ptr++), tknLen++;
        while((*stream->ptr == '0' || *stream->ptr == '1') && tknLen < MAX_BIN_NO)
            *(tkn++) = *(stream->ptr++), tknLen++;
        *tkn = 0;
        return(TKN_BINARY);
        }

    if(*stream->ptr == lc_decimal_point)
        {
        *(tkn++) = *(stream->ptr++), tknLen++;
        while(isDigit((unsigned char)*stream->ptr) && tknLen < MAX_DECIMALS)
            *(tkn++) = *(stream->ptr++), tknLen++;
        floatFlag = TRUE;
        }
    else if(toupper(*stream->ptr) != 'E')
        {
        if(*stream->ptr == 'L') *(tkn++) = *(stream->ptr++), tknLen++;
        *tkn = 0;
        *ptr_len = tknLen;
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
            else 
                {
                *ptr_len = tknLen;
                return(TKN_DECIMAL);
                }
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
                    case 'b':
                        *(tkn++) = '\b'; break;
                    case 'f':
                        *(tkn++) = '\f'; break;
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
                    case 'u':
                        if(isxdigit((unsigned char)*(stream->ptr + 1)) &&
                           isxdigit((unsigned char)*(stream->ptr + 2)) &&
                           isxdigit((unsigned char)*(stream->ptr + 3)) &&
                           isxdigit((unsigned char)*(stream->ptr + 4)))
                            {
#ifdef SUPPORT_UTF8
                            buff[0] = '0';
                            buff[1] = 'x';
                            memcpy(buff + 2, stream->ptr + 1, 4);
                            buff[6] = 0;
                            len = wchar_utf8(strtol(buff, NULL, 16), tkn);
                            stream->ptr += 4;
                            tkn += len;
                            tknLen += len -1;
#else
                            *(tkn++) = '\\';
                            memcpy(tkn, stream->ptr, 5);
                            tknLen = 5;   
                            tkn += 5;
                            stream->ptr += 4;
#endif
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
#ifdef BIGINT
INT64 longNum;
#endif
    
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
#ifdef WINDOWS
    if(isnan(*(double *)&cell->aux) || !_finite(*(double *)&cell->aux)) *number = 0;
#else
    if(isnan(*(double *)&cell->aux)) *number = 0; 
#endif
    else if(*(double *)&cell->aux >  4294967295.0) *number = 0xFFFFFFFF;
    else if(*(double *)&cell->aux < -2147483648.0) *number = 0x80000000;
    else *number = *(double *)&cell->aux;
    }
#else /* NEWLISP64 */
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
#ifdef BIGINT
    if(cell->type == CELL_BIGINT)
        {
        longNum = bigintToInt64(cell);
        *number = longNum;
#ifndef NEWLISP64
        if(longNum > 2147483647LL || longNum < -2147483648LL)
            return(errorProcExt(ERR_NUMBER_OUT_OF_RANGE, cell));
#endif
        }
    else
#endif
        {
        *number = 0;
        return(errorProcArgs(ERR_NUMBER_EXPECTED, params));
        }
    }

return(params->next);
}

#ifndef NEWLISP64
CELL * getInteger64Ext(CELL * params, INT64 * number, int evalFlag)
{
CELL * cell;
   
if(evalFlag) 
    cell = evaluateExpression(params);
else
    cell = params;

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
else /* check for bigint if size * != NULL, then return bigint address in number */
    {
#ifdef BIGINT
    if(cell->type == CELL_BIGINT)
        *number = bigintToInt64(cell);
    else
#endif
        {
        *number = 0; 
        return(errorProcExt(ERR_NUMBER_EXPECTED, params));
        }
    }

return(params->next);
}

#else /* NEWLISP64 */
CELL * getInteger64Ext(CELL * params, INT64 * number, int evalFlag)
{
CELL * cell;

if(evalFlag)
    cell = evaluateExpression(params);
else
    cell = params;

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
#ifdef BIGINT
    if(cell->type == CELL_BIGINT)
        *number = bigintToInt64(cell);
    else
#endif
        {
        *number = 0;
        return(errorProcArgs(ERR_NUMBER_EXPECTED, params));
        }
    }

return(params->next);
}
#endif

CELL * getIntegerExt(CELL * params, UINT * number, int evalFlag)
{
CELL * cell;
#ifdef BIGINT
INT64 longNum;
#endif

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
#ifdef WINDOWS
    if(isnan(*(double *)&cell->aux) || !_finite(*(double *)&cell->aux)) *number = 0;
#else
    if(isnan(*(double *)&cell->aux)) *number = 0; 
#endif
    else if(*(double *)&cell->aux >  4294967295.0) *number = 0xFFFFFFFF;
    else if(*(double *)&cell->aux < -2147483648.0) *number = 0x80000000;
    else *number = *(double *)&cell->aux;
    }
#else /* NEWLISP64 */
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
else /* if BIGNUM type throw ERR_NUMBER_OUT_OF_RANGE */
    {
#ifdef BIGINT
    if(cell->type == CELL_BIGINT)
        {
        longNum = bigintToInt64(cell);
        *number = longNum;
#ifndef NEWLISP64
        if(longNum > 2147483647LL || longNum < -2147483648LL)
            return(errorProcExt(ERR_NUMBER_OUT_OF_RANGE, cell));
#endif
        }
    else
     
#endif /* BIGINT */
        {
        *number = 0;
        return(errorProcArgs(ERR_NUMBER_EXPECTED, params));
        }
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
    *floatNumber = (INT)cell->contents;
else
    {
#ifdef BIGINT
    if(cell->type == CELL_BIGINT)
        *floatNumber = bigintCellToFloat(cell);
    else
#endif
        { 
        *floatNumber = 0.0;
        return(errorProcArgs(ERR_NUMBER_EXPECTED, params));
        }
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
CELL * cellForDelete;

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
    cellForDelete = (CELL *)(*symbol)->contents;
    if(isNil(cell)) 
        *symbol = nilSymbol;
    else if(cell->type != CELL_LAMBDA && cell->type != CELL_FEXPR && cell->type != CELL_PRIMITIVE)
        {
        *symbol = nilSymbol;
        deleteList(cellForDelete);
        return(errorProcExt(ERR_INVALID_PARAMETER, params));
        }
    else if(compareCells(cellForDelete, cell) != 0)
        {
        (*symbol)->contents = (UINT)copyCell(cell);
        deleteList(cellForDelete);
        }
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
#ifndef ANDROID
struct lconv * lc;
#endif
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

#ifndef ANDROID
lc = localeconv();  
lc_decimal_point = *lc->decimal_point;
#endif
cell = getCell(CELL_EXPRESSION);
addList(cell, stuffString(locale));
#ifdef ANDROID
addList(cell, stuffStringN(".", 1));
#else
addList(cell, stuffStringN(lc->decimal_point, 1));
#endif
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
  the classic eval-string: read the string, compile to s-expression , evaluate
READ_EXPR_SYNC
  used by p_sync() in nl-filesys.c 
READ_EXPR
  used by p_readExpr 
READ_EXPR_NET
  used by p_netEval introduces in 10.6.3, before READ_EXPR_SYNC was used
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
    pushResult(program = getCell(CELL_QUOTE));
    if(compileExpression(&stream, program) == 0) 
        break;
    if(readerEvent != nilSymbol)
        {
        --resultStackIdx;
        executeSymbol(readerEvent, program, &xlate);
        pushResult(program = makeCell(CELL_QUOTE, (UINT)xlate));
        }
    if(mode == EVAL_STRING)
        resultCell = evaluateExpression((CELL *)program->contents);
    else /* READ_EXPR, READ_EXPR_SYNC, READ_EXPR_NET */
        {
        if(resultCell != nilCell) pushResult(resultCell); /* 10.6.3 */
        countCell->contents = (UINT)(stream.ptr - stream.buffer);
        resultCell = (CELL *)program->contents;
        program->contents = (UINT)nilCell; /* de-couple */
        if(mode == READ_EXPR_SYNC || mode == READ_EXPR) /* 10.6.3 */
            break; /* only do first expression */
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

#ifdef EMSCRIPTEN
extern char *emscripten_run_script_string(const char *script);
char * evalJSbuff = NULL;

char * evalStringJS(char * cmd, size_t len)
{
if(evalJSbuff != NULL) free(evalJSbuff);

evalJSbuff = callocMemory(len + 1);
memcpy(evalJSbuff, cmd, len);

return(emscripten_run_script_string(evalJSbuff));
}


CELL * p_evalStringJS(CELL * params)
{
char * cmd;
size_t len;
char * result;

getStringSize(params, &cmd, &len, TRUE);
result = evalStringJS(cmd, len);

return(stuffString(result));
}
#endif


CELL * p_curry(CELL * params)
{
CELL * lambda;
CELL * cell;

cell = makeCell(CELL_EXPRESSION, (UINT)stuffSymbol(sysxSymbol));
lambda = makeCell(CELL_LAMBDA, (UINT)cell);
cell->next = getCell(CELL_EXPRESSION);
cell = cell->next;
cell->contents = (UINT)copyCell(params);
cell = (CELL *)cell->contents;
/* take left parameter */
cell->next = copyCell(params->next);
cell = cell->next;
cell->next = stuffSymbol(sysxSymbol);
/* take right parameter
cell->next = stuffSymbol(sysxSymbol);
cell = cell->next;
cell->next = copyCell(params->next);
*/
return(lambda);
}


CELL * p_apply(CELL * params)
{
CELL * expr;
CELL * args;
CELL * cell;
CELL * result;
CELL * func;
ssize_t count = 0, cnt;
UINT * resultIdxSave;

func = evaluateExpression(params);
params = getEvalDefault(params->next, &args);
cell = copyCell(func);
expr = makeCell(CELL_EXPRESSION, (UINT)cell);

if(args->type == CELL_ARRAY)
    {
    args = arrayList(args, FALSE);
    pushResult(args);
    }

if(args->type != CELL_EXPRESSION)
    {
    pushResult(expr);
    if(isNil(args))
        return(copyCell(evaluateExpression(expr)));
    else
        return(errorProcExt(ERR_LIST_EXPECTED, args));
    }

args = (CELL *)args->contents;        

if(params != nilCell)
    getInteger(params, (UINT *)&count);
if(count < 2) count = MAX_LONG;
cnt = count;

resultIdxSave = resultStackIdx + 2;
for(;;)
    {
    while(args != nilCell && cnt--)
        {
        if(isSelfEval(args->type))
            cell->next = copyCell(args);
        else
            cell->next = makeCell(CELL_QUOTE, (UINT)copyCell(args));
        cell = cell->next;
        args = args->next;
        }
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
    cell = copyCell(func);
    expr = makeCell(CELL_EXPRESSION, (UINT)cell);
    cell->next = makeCell(CELL_QUOTE, (UINT)result);
    cell = cell->next;
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
    expandExprSymbol(expr, symbol);

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
            expandExprSymbol(cell, sPtr);
        }

    else if(isEnvelope(cell->type)) expand(cell, symbol);
    cell = cell->next;
    }

return(expr);
}


void expandExprSymbol(CELL * cell, SYMBOL * sPtr)
{
CELL * rep;

rep = copyCell((CELL*)sPtr->contents);
/* check for and undo copyCell optimization */
while((UINT)rep == sPtr->contents)
    rep = copyCell((CELL*)sPtr->contents);

cell->type = rep->type;
cell->aux = rep->aux;
cell->contents = rep->contents;
rep->type = CELL_LONG;
deleteList(rep);
}


/* expands one or a chain of expressions */

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
        list = (CELL*)params->contents; /* expansion assoc list */
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
                symbol->contents = (UINT)copyCell(evaluateExpression(cell->next));
            else
                symbol->contents = (UINT)cell->next;
            expr = expand(copyCell(expr), symbol);
            if(evalFlag) deleteList((CELL *)symbol->contents); 
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


CELL * defineOrMacro(CELL * params, UINT cellType, int flag)
{
SYMBOL * symbol;
CELL * argsPtr;
CELL * lambda;
CELL * args;
CELL * body;
CELL * cell;

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
    return(errorProc(ERR_SYMBOL_PROTECTED));

/* local symbols */
argsPtr = copyList(argsPtr->next);

args = getCell(CELL_EXPRESSION);
args->contents = (UINT)argsPtr;
/* body expressions */
body = copyList(params->next);

/* if expansion macro insert expand symbol for body expansion
   (expand 'body) */
if(flag)
    {
    if(body->next != nilCell)
        {
        /* body has multiple expressions (expand '(begin ...)) */
        cell = stuffSymbol(beginSymbol);
        cell->next = body;
        body = makeCell(CELL_EXPRESSION, (UINT)cell);
        }
    cell = stuffSymbol(expandSymbol);
    cell->next = makeCell(CELL_QUOTE, (UINT)body);
    body = makeCell(CELL_EXPRESSION, (UINT)cell);
    symbol->flags |= SYMBOL_MACRO;
    }

args->next = body;
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
        return(defineOrMacro(params, CELL_LAMBDA, FALSE));
    return(setDefine(getDynamicSymbol(params), params->next, SET_SET));
    }

return(setDefine((SYMBOL *)params->contents, params->next, SET_SET));
}

CELL * p_defineMacro(CELL * params)
{
return(defineOrMacro(params, CELL_FEXPR, FALSE));
}

CELL * p_macro(CELL * params)
{
return(defineOrMacro(params, CELL_FEXPR, TRUE));
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

if(symbolRef && isProtected(symbolRef->flags) && symbolRef->contents == (UINT)cell)
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
else if(cell->type == CELL_STRING || cell->type == CELL_DYN_SYMBOL 
#ifdef BIGINT
        || cell->type == CELL_BIGINT
#endif
        )
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
	pushResultFlag = TRUE;
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
    if((symbolType(symbol) == CELL_CONTEXT && (SYMBOL *)((CELL *)symbol->contents)->contents == symbol)
            || symbol == countSymbol)
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
#ifdef SUPPORT_UTF8
size_t len;
#endif

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
    len =  utf8_1st_len((char*)cell->contents);
    if(len > cell->aux -1)
        return(errorProc(ERR_INVALID_UTF8));
    result = stuffStringN((char*)cell->contents, len);
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
#ifdef SUPPORT_UTF8
size_t size;
#endif

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
    size = utf8_1st_len((char *)cell->contents);
    if(size > cell->aux - 1)
        return(errorProc(ERR_INVALID_UTF8));
    return(stuffString((char *)(cell->contents + size)));
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
        copy = arrayList(cell, TRUE);
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
    str = utf8_index(str, utf8_wlen(str, str + cell->aux) -1);
    result = stuffString(str);
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
itSymbol->contents = (UINT)cell;
while(isNil(cell) || isEmpty(cell))
    {
    params = params->next;
    if(params->next == nilCell) 
        goto IF_RETURN;
    params = params->next;
    cell = evaluateExpression(params);
    }

if(params->next != nilCell) 
    cell = evaluateExpression(params->next);
   
IF_RETURN: 
itSymbol->contents = (UINT)nilCell;
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

pushEnvironment(listIdxSymbol->contents);
pushEnvironment(listIdxSymbol);
listIdxSymbol->contents = (UINT)cell;

return(cell);
}

void recoverIteratorIndex(CELL * cellIdx)
{
deleteList(cellIdx);
listIdxSymbol = (SYMBOL*)popEnvironment();
listIdxSymbol->contents = (UINT)popEnvironment();
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
        cell = getInteger64Ext(cell, &fromInt64, TRUE);
        getInteger64Ext(cell, &toInt64, TRUE);
        stepCnt = (toInt64 > fromInt64) ? toInt64 - fromInt64 : fromInt64 - toInt64;
        }
    else /* dotimes */
        {
        fromInt64 = toInt64 = 0;
        cond = getInteger64Ext(cell, &stepCnt, TRUE);
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
        symbol->contents = (UINT)stuffFloat(interval);
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
        if(isList(list->type)) list = copyCell(list);
        else if(list->type == CELL_ARRAY) list = arrayList(list, FALSE);
        else return(errorProcExt(ERR_LIST_EXPECTED, cell));
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
            if(*sPtr->name != '_') 
                {
                cell = nilCell;
                goto DO_CONTINUE;
                }
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
CELL * result = nilCell;

while(params != nilCell)
    {
    result = evaluateExpression(params);
    params = params->next;
    }

pushResultFlag = FALSE;
return(result);
}

extern UINT getAddress(CELL * params);

CELL * p_copy(CELL * params)
{
CELL * copy;

/* experimental: copy a cell from address, from:
   http://www.newlispfanclub.alh.net/forum/viewtopic.php?f=5&t=4548
   June 14, 2014 "get-cell function patch"
*/
if(params->next != nilCell && getFlag(params->next))
    return(copyCell((CELL *)getAddress(params)));

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
        && sPtr != trueSymbol && type != CELL_IMPORT_CDECL && type != CELL_IMPORT_FFI
#if defined(WINDOWS) || defined(CYGWIN)
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
STREAM strStream = {NULL, NULL, 0, 0, 0};
SYMBOL * contextSave;
#ifndef EMSCRIPTEN
CELL * result;
CELL * dataCell;
#endif
int errorFlag = 0;

contextSave = currentContext;
currentContext = mainContext;

params = getString(params, &fileName);

openStrStream(&strStream, MAX_STRING, 0);
serializeSymbols(params, (UINT)&strStream);

#ifndef EMSCRIPTEN
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
#endif
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
static SYMBOL * toContext;
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
            (UINT)translateCreateSymbol(sPtr->name, 0, toContext, TRUE);
    newCell->contents = (UINT)allocMemory(strlen((char *)cell->contents) + 1);
    memcpy((void *)newCell->contents,
        (void*)cell->contents, strlen((char *)cell->contents) + 1);
    }

if(cell->type == CELL_SYMBOL)
    {
    /* if the cell copied, itself contains a symbol copy it recursevely,
       if new, if not done here it might not been seen as new later and left
       without contents */
    sPtr = (SYMBOL *)cell->contents;
    /* don't copy symbols of builtins and libffi */
    if(sPtr->context == fromContext && !(sPtr->flags & (SYMBOL_BUILTIN | SYMBOL_FFI)))
        {
        if((newSptr = lookupSymbol(sPtr->name, toContext)) == NULL)
            {
            newSptr = translateCreateSymbol(sPtr->name, symbolType(sPtr), toContext, TRUE);
            deleteList((CELL *)newSptr->contents);
            newSptr->contents = (UINT)copyContextCell((CELL*)sPtr->contents);
            }
        newCell->contents = (UINT)newSptr;
        newSptr->flags = sPtr->flags;
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


void iterateCopyCreateSymbols(SYMBOL * sPtr)
{
int type, newFlag = FALSE;
SYMBOL * newPtr = NULL;

if(sPtr != NIL_SYM && sPtr != NULL && !(sPtr->flags & SYMBOL_BUILTIN))
    {
    iterateCopyCreateSymbols(sPtr->left);
    type = symbolType(sPtr);

    /* optimized check for default symbol, translate default symbol to default symbol */
    if(*sPtr->name == *fromContext->name && strcmp(sPtr->name, fromContext->name) == 0)
        {
        if((newPtr = lookupSymbol(toContext->name, toContext)) == NULL)
            {
            newPtr = translateCreateSymbol(toContext->name, type, toContext, TRUE);
            newFlag = TRUE;
            }
        }
    else
        {
        if((newPtr = lookupSymbol(sPtr->name, toContext)) == NULL)
            {
            newPtr = translateCreateSymbol(sPtr->name, type, toContext, TRUE);
            newFlag = TRUE;
            }
        }

    if(overWriteFlag == TRUE || newFlag == TRUE)
        {
        deleteList((CELL *)newPtr->contents);
        newPtr->contents = (UINT)copyContextCell((CELL*)sPtr->contents);
        }

    newPtr->flags |= sPtr->flags & SYMBOL_PROTECTED;
    iterateCopyCreateSymbols(sPtr->right);
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
    toContext = currentContext;
else 
    {
    params = evaluateExpression(params);
    if(params->type == CELL_CONTEXT || params->type == CELL_SYMBOL)
        toContext = (SYMBOL *)params->contents;
    else
        return(errorProcExt(ERR_CONTEXT_EXPECTED, params));

        overWriteFlag = (evaluateExpression(next)->type != CELL_NIL);

    /* allow symbols to be converted to contexts */
    if(symbolType(toContext) != CELL_CONTEXT)
        {
        if(isProtected(toContext->flags))
            return(errorProcExt(ERR_SYMBOL_PROTECTED, params));

        if(toContext->context != mainContext)
            return(errorProcExt2(ERR_NOT_IN_MAIN, stuffSymbol(toContext)));

        deleteList((CELL *)toContext->contents);
        makeContextFromSymbol(toContext, NULL);
        }
    }

if(toContext == mainContext)
    return(errorProc(ERR_TARGET_NO_MAIN));

iterateCopyCreateSymbols((SYMBOL *)((CELL*)fromContext->contents)->aux);

return(copyCell((CELL*)toContext->contents));
}


CELL * p_defineNew(CELL * params)
{
SYMBOL * sourcePtr;
SYMBOL * targetPtr;
char * name;

params = getSymbol(params, &sourcePtr);
if(params != nilCell)
    {
    getSymbol(params, &targetPtr);
    name = targetPtr->name;
    toContext = targetPtr->context;
    }
else
    {
    name = sourcePtr->name;
    toContext = currentContext;
    }

if(toContext == mainContext)
    return(errorProc(ERR_TARGET_NO_MAIN));

fromContext = sourcePtr->context;
targetPtr = translateCreateSymbol(name, symbolType(sourcePtr), toContext, TRUE);

deleteList((CELL *)targetPtr->contents);
targetPtr->contents = (UINT)copyContextCell((CELL*)sourcePtr->contents);

targetPtr->flags = sourcePtr->flags;

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
#ifdef BIGINT
int * numPtr;
#endif

switch(cell->type)
    {
#ifndef NEWLISP64
    case CELL_INT64:
        if(*(INT64 *)&cell->aux == 0)
            return(trueCell);
        break;
#endif
    case CELL_FLOAT:
#ifndef NEWLISP64
        if(*(double *)&cell->aux == 0.0)
#else
        if(*(double *)&cell->contents == 0.0)
#endif
            return(trueCell);
        break;
    case CELL_LONG:
        if(cell->contents == 0)
            return(trueCell);
        break;
#ifdef BIGINT
    case CELL_BIGINT:
        numPtr = (int *)(UINT)cell->contents;
        if(cell->aux == 2 && numPtr[1] == 0)
            return(trueCell);
        break;
#endif
    default:
        break;
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
return(isZero(evaluateExpression(params)));
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

#ifdef BIGINT
CELL * p_isBigInteger(CELL * params)
    { return(isType(params, CELL_BIGINT)); }
#endif

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
{ 
SYMBOL * sPtr;

if(params == nilCell)
    return(errorProc(ERR_MISSING_ARGUMENT));
params = evaluateExpression(params);
if(params->type == CELL_FEXPR) /* lambda-macro */
    return(trueCell);
if(params->type == CELL_SYMBOL)
    {
    sPtr = (SYMBOL *)params->contents;
    if(sPtr->flags & SYMBOL_MACRO)
        return(trueCell);
    }
return(nilCell);
}

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
        || params->type == CELL_IMPORT_FFI
#if defined(WINDOWS) || defined(CYGWIN)
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

#ifndef EMSCRIPTEN
if(daemonMode) 
    {
    fclose(IOchannel);
#ifndef WINDOWS
    IOchannel = NULL;
#endif
    longjmp(errorJump, ERR_USER_RESET);
    }
#else
return(nilCell);
#endif

if(params != nilCell) getInteger(params, &result);
else result = 0;

#ifdef HAVE_FORK
/* release spawn resources */
purgeSpawnList(TRUE);
#endif

exit(result);
return(trueCell);
}


#ifdef EMSCRIPTEN
void emscriptenReload(void)
{
char * cmd = "location.reload();";
printf("# newLISP is reloading ...\n");
evalStringJS(cmd, strlen(cmd));
}
#endif


CELL * p_reset(CELL * params)
{
int blockCountBefore = blockCount;

if(params != nilCell)
    {
    params = evaluateExpression(params);
    if(isNumber(params->type))
        {
        getIntegerExt(params, (UINT*)&MAX_CELL_COUNT, FALSE);
        if(MAX_CELL_COUNT < MAX_BLOCK) MAX_CELL_COUNT = MAX_BLOCK;
        return(stuffInteger(MAX_CELL_COUNT));
        }
    else if(isNil(params)) 
        {
        freeCellBlocks(); 
        return(stuffIntegerList(2, blockCountBefore, blockCount)); /* 10.3.3 */
        }
#ifndef LIBRARY
#ifndef WINDOWS
    else
        execv(MainArgs[0], MainArgs);
#endif
#endif
#ifdef EMSCRIPTEN
        emscriptenReload();
#endif
    }
else
#ifndef EMSCRIPTEN
    longjmp(errorJump, ERR_USER_RESET);
#else
    return(nilCell);
#endif

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


#ifndef WINDOWS

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
  return(stuffFloat(seconds));
  }
  
return(makeCell(CELL_SYMBOL, (UINT)timerEvent));
}
#endif

#ifndef EMSCRIPTEN
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
    snprintf(sigStr, 11, "$signal-%d", (int)sig);
    getCreateSymbol(params, &signalEvent, sigStr);
    symHandler[sig - 1] = signalEvent;
    if(signal(sig, signal_handler) == SIG_ERR) return(nilCell);
    }
  
return(makeCell(CELL_SYMBOL, (UINT)symHandler[sig - 1]));
}
#endif

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
CELL * cell;
UINT count = 0;
int i;

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
            varPrintf(OUT_DEVICE, LINE_FEED);
            ++count;
            }
        ++blockPtr;
        }
    blockPtr = blockPtr->next;
    }

return(stuffInteger(count));
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
    {
    if(cell->aux - 1 > MAX_SYMBOL)
        return(errorProcExt(ERR_STRING_TOO_LONG, cell));	
    newSymStr = (char *)cell->contents;
    }
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
printf("entering colon, saving in objSave:");
printCell(objSave, TRUE, OUT_CONSOLE);
puts("");
#endif

cell = (CELL *)obj->contents;
if(obj->type != CELL_EXPRESSION)
    return(errorProcExt(ERR_LIST_EXPECTED, obj));

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
printf("leavin colon, objCell restored to:");
printCell(obj, TRUE, OUT_CONSOLE);
puts("");
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

if(idx > 15) return(nilCell);

return(copyCell((CELL*)sysSymbol[idx]->contents));
}


/* end of file */
