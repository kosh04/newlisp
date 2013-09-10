/* 
  sql.c - find offsets of important data fields in MySQL data structures 

  version 1.1, (c) Lutz Mueller 2004, released under the GNU GPL License
  version 1.2 added 2 more include files for 4.0

  compile using: gcc sql.c -o sql

  then do: ./sql
  to obtain a list of offsets in MySQL data structures for different
  versions and compile flavors of MySQL

  this program is only needed when designing new MySQL support functions
  in newLISP and the MySQL data structures have to be accessed directly

  the values produced by this program are used in the files mysql.lsp,
  mysql5.lsp and mysql51.lsp

  See also /usr/share/newlisp/mysql.lsp

*/


#define LINUX
/* #define MACOSX */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#ifdef MACOSX
#include "/usr/local/mysql/include/mysql.h"
#include "/usr/local/mysql/include/mysql_com.h"
#include "/usr/local/mysql/include/mysql_version.h"
#include "/usr/local/mysql/include/my_alloc.h"
#endif

#ifdef LINUX
#include "/usr/include/mysql/mysql.h"
#include "/usr/include/mysql/mysql_com.h"
#include "/usr/include/mysql/mysql_version.h"
#include "/usr/include/mysql/my_alloc.h"
#endif

MYSQL  * mysql;
MYSQL_RES  * mysql_res;


int main(int argc, char ** argv)
{
printf("sizeof(MYSQL)       %d\n", sizeof(MYSQL));
printf("sizeof(MYSQL_RES)   %d\n", sizeof(MYSQL_RES));
printf("sizeof(MYSQL_ROW)   %d\n", sizeof(MYSQL_ROW));
printf("sizeof(MYSQL_FIELD) %d\n", sizeof(MYSQL_FIELD));
printf("sizeof(NET) %d\n", sizeof(NET));
printf("sizeof(my_ulonglong) %d\n", sizeof(my_ulonglong));
printf("sizeof(my_bool) %d\n\n", sizeof(my_bool));

mysql = 0;
mysql_res = 0;

printf("offset mysql->affected_rows: %d\n", &mysql->affected_rows);
printf("offset mysql->insert_id: %d\n", &mysql->insert_id);
printf("offset mysql->net.last_error: %d\n", &mysql->net.last_error);

printf("offset mysql_res->row_count: %d\n", &mysql_res->row_count);
printf("offset mysql_res->field_count: %d\n", &mysql_res->field_count);
printf("offset mysql_res->eof: %d\n", &mysql_res->eof);


return 0;
}

/* eof */

