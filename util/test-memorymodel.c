#include <stdio.h>

int
main(int argc, char** argv) {
	short sc = sizeof(char) * 8;
	short ss = sizeof(short) * 8;
	short si = sizeof(int) * 8;
	short sl = sizeof(long) * 8;
	short sp = sizeof(void*) * 8;

	if (si == 32 && sl == 64 && sp == 64) { printf("LP64\n"); return 0; }
	if (si == 64 && sl == 64 && sp == 64) { printf("ILP64\n"); return 0; }
	if (si == 32 && sl == 32 && sp == 64) { printf("LLP64\n"); return 0; }
	if (si == 32 && sl == 32 && sp == 32) { printf("ILP32\n"); return 0; }
	if (si == 16 && sl == 32 && sp == 32) { printf("LP32\n"); return 0; }
	printf("UNKNOWN\n"); return 1;
}
