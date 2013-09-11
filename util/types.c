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

output on AMD64 and Intel dual Core 2

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

#define str2cmp(s1,s2) ( ( (*(unsigned char *)(s1) << 8) |  *((unsigned char *)(s1) + 1) ) - \
						 ( (*(unsigned char *)(s2) << 8) |  *((unsigned char *)(s2) + 1) )  )		 

int main(int argc, char * argv[])
{
int x = 1;

printf("\n");
printf("type      bytes\n");
printf("---------------\n");
printf("char        %lu\n", sizeof(char));
printf("char *      %lu\n", sizeof(char *));
printf("void *      %lu\n", sizeof(void *));
printf("short int   %lu\n", sizeof(short int));
printf("int         %lu\n", sizeof(int));
printf("long        %lu\n", sizeof(long));
printf("long int    %lu\n", sizeof(long int));
printf("long long   %lu\n", sizeof(long long));
printf("size_t      %lu\n", sizeof(size_t));
printf("float       %lu\n", sizeof(float));
printf("double      %lu\n", sizeof(double));
printf("long double %lu\n", sizeof(long double));
printf("wchar_t     %lu\n", sizeof(wchar_t));

printf("\n");
printf("format input              output\n");
printf("------------------------------------------------\n");
printf("%%d     0xffffffff               %d\n", 0xffffffff);
printf("%%u     0xffffffff               %u\n", 0xffffffff);
printf("%%d     0x7fffffff               %d\n", 0x7fffffff);
printf("%%u     0x7fffffff               %u\n", 0x7fffffff);
printf("%%d     0x80000000               %d\n", 0x80000000);
printf("%%u     0x80000000               %u\n", 0x80000000);
printf("%%lld     0x7fffffffffffffffLL   %lld\n", 0x7fffffffffffffffLL);
printf("%%llu     0x7fffffffffffffffLL   %llu\n", 0x7fffffffffffffffLL);
printf("%%llx     0xffffffffffffffffLL   %llx\n", 0xffffffffffffffffLL);
printf("%%llX     0x7fffffffffffffffLL   %llx\n", 0x7fffffffffffffffLL);
printf("%%llX     0x8000000000000000LL   %llx\n", 0x8000000000000000LL);
printf("%%llX   0xFFFFFFFFFFFFFFFFLL     %llx\n", 0xFFFFFFFFFFFFFFFFLL);
printf("%%llX   0x7FFFFFFFFFFFFFFFLL     %llx\n", 0x7FFFFFFFFFFFFFFFLL);
printf("%%llX   0x8000000000000000LL     %llx\n", 0x8000000000000000LL);
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

printf("\n");

if(*(char *)&x == 1)
	printf("CPU is little endian\n");
else
	printf("CPU is big endian\n");
	
#ifdef __LITTLE_ENDIAN__
printf("The constant __LITTLE_ENDIAN__ is defined by the compiler\n");
#endif

#ifdef __BIG_ENDIAN__
printf("The constant __BIG_ENDIAN__ is defined by the compiler\n");
#endif

exit(0);
}


