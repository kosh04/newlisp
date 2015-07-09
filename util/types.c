/* types.c -- find out size of data types */

/* 

output on Mac OS X PPCi, memory model ILP32

type        bytes
-----------------
char          1
char *        4
void *        4
short int     2
int           4
long          4
long int      4
long long int 8
float         4
double        8
wchar_t       4
size_t        4
off_t         4
time_t        4
intptr_t      4

output on Windows MinGW 32-bit

type        bytes
-----------------
char          1
char *        4
void *        4
short int     2
int           4
long          4
long int      4
long long int 8
float         4
double        8
long double   12
wchar_t       2
size_t        4
off_t         4
time_t        4
intptr_t      4

output on AMD64 and Intel dual Core 2, memory model LP64

type        bytes
-----------------
char          1
char *        8
void *        8
short int     2
int           4 
long          8
long int      8
long long int 8
float         4
double        8
long double  16
wchar_t       4
size_t        8
off_t         8
time_t        8
intptr_t      8

LLP64 Windows when compiling with tdm gcc 64-bit

type        bytes
-----------------
char          1
char *        8
void *        8
short int     2
int           4
long          4
long int      4
long long int 8
float         4
double        8
long double   16
wchar_t       2
size_t        8
off_t         4
time_t        8
intptr_t      8


*/

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <fcntl.h>
#include <locale.h>
#include <inttypes.h>

#ifndef time_t
#include <time.h>
#endif

typedef struct {
    char one;
    short int two;
    int four;
    } alignment1;

typedef struct {
    char one;
    int four;
    short int two;
    } alignment2;

typedef struct {
    char one;
    short int two;
    long three;
    int four;
    } alignment3;

typedef struct {
    char one;
    long three;
    short int two;
    int four;
    } alignment4;

#define str2cmp(s1,s2) ( ( (*(unsigned char *)(s1) << 8) |  *((unsigned char *)(s1) + 1) ) - \
						 ( (*(unsigned char *)(s2) << 8) |  *((unsigned char *)(s2) + 1) )  )		 
void printMemoryModel(void);

int main(int argc, char * argv[])
{
int endian = 1;
long long int x, y, z;

alignment1 checkstruct1;
alignment2 checkstruct2;
alignment3 checkstruct3;
alignment4 checkstruct4;

printf("\n");
printf("type        bytes\n");
printf("-----------------\n");
printf("char          %lu\n", sizeof(char));
printf("char *        %lu\n", sizeof(char *));
printf("void *        %lu\n", sizeof(void *));
printf("short int     %lu\n", sizeof(short int));
printf("int           %lu\n", sizeof(int));
printf("long          %lu\n", sizeof(long));
printf("long int      %lu\n", sizeof(long int));
printf("long long int %lu\n", sizeof(long long int));
printf("float         %lu\n", sizeof(float));
printf("double        %lu\n", sizeof(double));
printf("long double   %lu\n", sizeof(long double));
printf("wchar_t       %lu\n", sizeof(wchar_t));
printf("size_t        %lu\n", sizeof(size_t));
printf("off_t         %lu\n", sizeof(off_t));
printf("time_t        %lu\n", sizeof(time_t));
#if defined(_INTTYPES_H_)
printf("intptr_t      %lu\n", sizeof(intptr_t));
#endif

printf("\nAlignment:\n");
printf("size of struct {char, short int, int} is: %ld\n", sizeof(checkstruct1));
printf("size of struct {char, int, short int} is: %ld\n", sizeof(checkstruct2));
printf("size of struct {char, short int, long, int} is: %ld\n", sizeof(checkstruct3));
printf("size of struct {char, long, short int, int} is: %ld\n", sizeof(checkstruct4));
printf("\n");

printf("format input                  output\n");
printf("--------------------------------------------------\n");
printf("%%d     0x7fffffff             %d\n", 0x7fffffff);
printf("%%d     0x80000000             %d\n", 0x80000000);
printf("%%d     0xffffffff             %d\n", 0xffffffff);
printf("%%u     0x7fffffff             %u\n", 0x7fffffff);
printf("%%u     0x80000000             %u\n", 0x80000000);
printf("%%u     0xffffffff             %u\n", 0xffffffff);
#ifndef TRUE64
/* will handle either 32bit or 64bit depending on platform
printf("%%ld   0x7fffffffffffffffL    %ld\n", 0x7FFFFFFFL);
printf("%%ld   0x8000000000000000L    %ld\n", 0x80000000L);
printf("%%ld   0xffffffffffffffffL    %ld\n", 0xFFFFFFFFL);
printf("%%lu   0x7fffffffffffffffL    %lu\n", 0x7FFFFFFFL);
printf("%%lu   0x8000000000000000L    %lu\n", 0x80000000L);
printf("%%lu   0xffffffffffffffffL    %ld\n", 0xFFFFFFFFL);
*/
#endif
printf("%%llx   0x7FFFFFFFFFFFFFFFLL   %llx\n", 0x7FFFFFFFFFFFFFFFLL);
printf("%%llx   0x8000000000000000LL   %llx\n", 0x8000000000000000LL);
printf("%%llx   0xFFFFFFFFFFFFFFFFLL   %llx\n", 0xFFFFFFFFFFFFFFFFLL);
printf("%%llX   0x7fffffffffffffffLL   %llX\n", 0x7fffffffffffffffLL);
printf("%%llX   0x8000000000000000LL   %llX\n", 0x8000000000000000LL);
printf("%%llX   0xffffffffffffffffLL   %llX\n", 0xffffffffffffffffLL);
#ifndef TRU64
printf("%%lld   0x7fffffffffffffffLL   %lld\n", 0x7fffffffffffffffLL);
printf("%%lld   0x8000000000000000LL   %lld\n", 0x8000000000000000LL);
printf("%%lld   0xffffffffffffffffLL   %lld\n", 0xffffffffffffffffLL);
printf("%%llu   0x7fffffffffffffffLL   %lld\n", 0x7fffffffffffffffLL);
printf("%%llu   0x8000000000000000LL   %llu\n", 0x8000000000000000LL);
printf("%%llu   0xffffffffffffffffLL   %llu\n", 0xffffffffffffffffLL);
#else
printf("%%ld   0x7fffffffffffffffLL    %ld\n", 0x7FFFFFFFFFFFFFFFLL);
printf("%%ld   0x8000000000000000LL    %ld\n", 0x8000000000000000LL);
printf("%%ld   0xffffffffffffffffLL    %ld\n", 0xFFFFFFFFFFFFFFFFLL);
printf("%%lu   0x7fffffffffffffffLL    %lu\n", 0x7FFFFFFFFFFFFFFFLL);
printf("%%lu   0x8000000000000000LL    %lu\n", 0x8000000000000000LL);
printf("%%lu   0xffffffffffffffffLL    %ld\n", 0xFFFFFFFFFFFFFFFFLL);
#endif

printf("\n");

if(*(char *)&endian == 1)
	printf("CPU is little endian\n");
else
	printf("CPU is big endian\n");
	
#ifdef __LITTLE_ENDIAN__
printf("The constant __LITTLE_ENDIAN__ is defined by the compiler\n");
#endif

#ifdef __BIG_ENDIAN__
printf("The constant __BIG_ENDIAN__ is defined by the compiler\n");
#endif

printMemoryModel();
printf("\n");

/*
x = 9223372036854775807LL; 
y = 1000LL;
z = x * y;
printf("\n9223372036854775807 * 1000 = %ld <- should be -1000\n", z);

x = 11;
y = -4;
z = x % y;

printf("11 %% -4 = %ld <- should be 3\n", z);

setlocale(LC_NUMERIC, "");
printf("$%'.2Lf\n", 123456789.00L);
printf("$%'.2Lf\n", 1234.56L);
printf("$%'.2Lf\n", 123.45L);
*/


exit(0);
}

void printMemoryModel(void)
{
short sc = sizeof(char) * 8;
short ss = sizeof(short) * 8;
short si = sizeof(int) * 8;
short sl = sizeof(long) * 8;
short sp = sizeof(void*) * 8;

printf("\nMemory model is ");
if (si == 32 && sl == 64 && sp == 64) { printf("LP64\n"); return;}
if (si == 64 && sl == 64 && sp == 64) { printf("ILP64\n"); return;}
if (si == 32 && sl == 32 && sp == 64) { printf("LLP64\n"); return;}
if (si == 32 && sl == 32 && sp == 32) { printf("ILP32\n"); return;}
if (si == 16 && sl == 32 && sp == 32) { printf("LP32\n"); return;}
printf("UNKNOWN\n"); 
}
