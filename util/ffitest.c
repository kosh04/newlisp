#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

/*
 testffi.c
 test FFI correct passing of parameter and return types

 this file is used in qa-specific-tests/qa-libffi

 compile using either:
     gcc -m32 ffitest.c -shared -o ffitest.dylib
 or
     gcc -m64 ffitest.c -shared -o ffitest.dylib


 all intx tupes must correctly handle sign-extension
 and unsigning
 e.g.: (ret_uint8 -1) => 255
       (ret_int8 255) => -1

    user API type  ffi type
    -------------  --------
    "byte"         &ffi_type_uint8
    "char"         &ffi_type_sint8
    "short int"    &ffi_type_sint16
    "unsigned short int"    &ffi_type_uint16
    "unsigned int" &ffi_type_uint32
    "int"          &ffi_type_sint32
#ifdef NEWLISP64
    "long"          &ffi_type_sint64
#else
    "long"          &ffi_type_sint32
#endif
    "long long"    &ffi_type_sint64
    "float"        &ffi_type_float
    "double"       &ffi_type_double
    "char*"        &ffi_type_pointer  input string or address, return string 
    "void*"        &ffi_type_pointer  input string or address, return address
#endif
*/

char ret_sint8(char number)
    {
    return(number);
    }

char ret_uint8(unsigned char number)
    {
    return(number);
    }

short int ret_sint16(int number)
    {
    return(number);
    }

short int ret_uint16(unsigned int number)
    {
    return(number);
    }

int ret_sint32(int number)
    {
    return(number);
    }

int ret_sint32_void(void)
    {
    return(-1);
    }

unsigned int ret_uint32(unsigned int number)
    {
    return(number);
    }

long long ret_sint64(long long number)
    {
    return(number);
    }

float ret_float(float number)
    {
    return(number);
    }

double ret_double(double number)
    {
    return(number);
    }

double add_double(double number1, double number2)
    {
    return(number1 + number2);
    }

char * ret_pointer(char * ptr)
    {
    char * upper;
    upper = ptr;
    while(*ptr) { *ptr = toupper(*ptr); ptr++; }
    return(upper);
    }

char * string_copy(char * from, char * to)
    {
    strcpy(to, from);
    return(to);
    }

/* test ffi closure API */

void (*function)(char * str, int inum, double fnum);

void register_callback(void * ptr)
    {
    function = ptr;
    }

void trigger_callback()
    {
    double dnum = 12345.67890;

    (*function)(">>>>> callback API ", 1234567890, dnum);
    }

/* test simple ffi */

void (*function_simple)(char * str, int inum);

void register_callback_simple(void * ptr)
    {
    function_simple = ptr;
    }

void trigger_callback_simple() /* only handles strings and integers */
    {
    long inum = 1234567890;

    (*function_simple)(">>>>> callback API ", inum);
    }

/* test struct API */

typedef struct clock
    {
    char hour;
    int min;
    short int sec;
    } clock;

clock addClock(clock in)
    {
    in.hour += 1;
    in.min += 1;
    in.sec += 1;
    return in;
    }

typedef struct foo
	{
	char first;
	int second;
	short int third;
	char * word;
	} foo;

foo * useFoo(int value)
	{
	foo * bar = malloc(sizeof(foo));
	bar->first = 1 + value;	
	bar->second = 2 + value;	
	bar->third = 3 + value;	
	bar->word = "hello world";
	return bar;
	}


/* miscellanous tests */

typedef struct 
{
double dat[2];
} gsl_complex;

/* structure return
(struct 'complex "double" "double")
(import LIB "gsl_complex_rect" "complex" "double" "double")
(gsl_complex_rect 3.0 4.0) ;=> (3 4)
*/

gsl_complex complex_test(gsl_complex gs)
{
return(gs);
}


/* mixed floats intd for simple ffi and 32bit
(import "ffitest.dylib" "mixed_int_float")
(get-string (mixed_int_float 123 (flt 123.456) 456 (flt 789.123)))
;=> "123 123.456001 456 789.122986\n"
*/

char * mixed_int_float(int x, float a, int y, float b)
{
static char buff[100];

sprintf(buff, "%d %f %d %f\n", x, a, y, b);

return(buff);
}

/* arrays */

int array_test(int n, int m, double * ptr[])
{
int i , j;

for(i = 0; i < n; i++)
    for(j = 0; j < m; j++)
        printf("%lf ", ptr[i][j]);

return(0);
}



/* eof */
