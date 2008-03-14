/* types.c -- find out size of data types */

/* 

output on Mac OS X PPC

type      bytes
---------------
char      1
char *    4
void *    4
short int 2
int       4
long      4
long int  4
long long 8
float     4
double    8
wchar_t   4

output on AMD64

type      bytes
---------------
char        1
char *      8
void *      8
short int   2
int         4
long        8
long int    8
long long   8
float       4
double      8
long double 16
wchar_t     4

*/

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

int main(int argc, char * argv[])
{
printf("\n");
printf("type      bytes\n");
printf("---------------\n");
printf("char        %d\n", sizeof(char));
printf("char *      %d\n", sizeof(char *));
printf("void *      %d\n", sizeof(void *));
printf("short int   %d\n", sizeof(short int));
printf("int         %d\n", sizeof(int));
printf("long        %d\n", sizeof(long));
printf("long int    %d\n", sizeof(long int));
printf("long long   %d\n", sizeof(long long));
printf("size_t      %d\n", sizeof(size_t));
printf("float       %d\n", sizeof(float));
printf("double      %d\n", sizeof(double));
printf("long double %d\n", sizeof(long double));
printf("wchar_t     %d\n", sizeof(wchar_t));

printf("\n");
printf("format input              output\n");
printf("------------------------------------------------\n");
printf("%%d     0xffffffff           %d\n", 0xffffffff);
printf("%%u     0xffffffff           %u\n", 0xffffffff);
printf("%%d     0x7fffffff           %d\n", 0x7fffffff);
printf("%%u     0x7fffffff           %u\n", 0x7fffffff);
printf("%%d     0x80000000           %d\n", 0x80000000);
printf("%%u     0x80000000           %u\n", 0x80000000);
printf("%%d     0x7fffffffffffffffLL %d\n", 0x7fffffffffffffffLL);
printf("%%u     0x7fffffffffffffffLL %u\n", 0x7fffffffffffffffLL);
printf("%%x     0xffffffffffffffffLL %x\n", 0xffffffffffffffffLL);
printf("%%X     0x7fffffffffffffffLL %x\n", 0x7fffffffffffffffLL);
printf("%%X     0x8000000000000000LL %x\n", 0x8000000000000000LL);
printf("%%llX   0xFFFFFFFFFFFFFFFFLL %x\n", 0xFFFFFFFFFFFFFFFFLL);
printf("%%llX   0x7FFFFFFFFFFFFFFFLL %x\n", 0x7FFFFFFFFFFFFFFFLL);
printf("%%llX   0x8000000000000000LL %x\n", 0x8000000000000000LL);
#ifndef TRU64
printf("%%lld   0xffffffffffffffffLL %lld\n", 0xFFFFFFFFFFFFFFFFLL);
printf("%%llu   0xffffffffffffffffLL %llu\n", 0xFFFFFFFFFFFFFFFFLL);
printf("%%lld   0x7fffffffffffffffLL %lld\n", 0x7fffffffffffffffLL);
printf("%%llu   0x7fffffffffffffffLL %lld\n", 0x7fffffffffffffffLL);
printf("%%lld   0x8000000000000000LL %lld\n", 0x8000000000000000LL);
printf("%%llu   0x8000000000000000LL %llu\n", 0x8000000000000000LL);
#else
printf("%%ld   0xffffffffffffffffLL %ld\n", 0xFFFFFFFFFFFFFFFFLL);
printf("%%lu   0xffffffffffffffffLL %ld\n", 0xFFFFFFFFFFFFFFFFLL);
printf("%%ld   0x7fffffffffffffffLL %ld\n", 0x7FFFFFFFFFFFFFFFLL);
printf("%%lu   0x7fffffffffffffffLL %lu\n", 0x7FFFFFFFFFFFFFFFLL);
printf("%%ld   0x8000000000000000LL %ld\n", 0x8000000000000000LL);
printf("%%lu   0x8000000000000000LL %lu\n", 0x8000000000000000LL);
#endif

exit(0);
}


