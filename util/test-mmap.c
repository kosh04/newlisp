#include <stdio.h>
#include <sys/mman.h>

int main(int argc, char ** argv)
{
void * address;
int pagesize;

pagesize = getpagesize();

printf("pagesize: %d\n", pagesize);

address = mmap( 0, pagesize, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0);

printf("address:0x%X\n", address);
}


