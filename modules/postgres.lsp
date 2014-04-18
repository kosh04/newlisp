;; @module postgres.lsp 
;; @description PostgreSQL interface (tested on PostgreSQL 8.3)
;; @version 1.02  - feature complete
;; @version 1.03  - doc formatting
;; @version 2.00  - replaced <tt>inc</tt> with <tt>++</tt>
;; @version 2.10  - new <tt>fnumber</tt> and <tt>fetch-value</tt>
;; @version 2.11  - new <tt>query</tt> with optional parameters 
;; @author Jeremy Cowgar 2006, Ted Walther 2009, Lutz Mueller 2010, Unya 2012
;;
;; <h3>Requirements</h3>
;; At the beginning of the program file include a 'load' statement for the module:
;; <pre>
;; (load "/usr/share/newlisp/modules/postgres.lsp")
;; ; or
;; (module "postgres.lsp") ; loads from (env "NEWLISPDIR") / modules
;; </pre>
;;
;; A version of 'libpq' for a specific platform is required:
;;  
;; on LINUX/UNIX: '/usr/local/lib/libpq.so.5.1' <br>
;; on Mac OS X:   '/usr/local/lib/libpq.dylib'
;;
;; This library is installed when using the Mac OS X install
;; package @link http://www.postgresql.org/download/macosx here
;;
;; This library might be in a different location on a particular
;; installation of PostgreSQL or have a different name.
;; Change accordingly in the code at the beginning.
;;
;; The PostgreSQL server itself may reside on a different machine
;; on the network. The library 'libpq' will communicate
;; with that server. The correct connection is created using
;; the 'PgSQL:connect' call.
;;
;; At the bottom of the module file 'postgres.lsp' a test routine 'test-pgsql'
;; is included to test for correct installation of PostgreSQL.  You call it
;; with the same arguments you would pass to ':connect'
;;
;; <h3>Functions available</h3>
;; <pre>
;;     PgSQL:connect ............. connect to a database
;;     PgSQL:query ............... execute a SQL statement
;;     PgSQL:num-rows ............ rows in result of query
;;     PgSQL:num-fields .......... columns in result of query
;;     PgSQL:fnumber ............. column number of query
;;     PgSQL:fetch-value ......... get value from the query result
;;     PgSQL:fetch-row ........... get row from the query result
;;     PgSQL:fetch-all ........... get all rows from the last query
;;     PgSQL:database ............ return all database names
;;     PgSQL:tables .............. return all tables names
;;     PgSQL:fields .............. return all fields in a table
;;     PgSQL:data-seek ........... position in result for fetching
;;     PgSQL:error ............... get error message
;;     PgSQL:affected-rows ....... number of affected rows from operation
;;     PgSQL:escape .............. escapes input string according to the SQL standard
;;     PgSQL:close-db ............ close database connection
;; </pre>
;;
;; <h3>Differences from the MySQL module</h3>
;;
;; The function ':inserted-id' isn&#039;t supported because PostgreSQL
;; doesn&#039;t support it.  Instead, use the 'RETURNING' clause in your 'INSERT'
;; statement, then use ':fetch-row' or ':fetch-all' to
;; find the value.  'INSERT&nbsp;RETURNING' is a PostreSQL idiom
;; documented @link http://www.postgresql.org/docs/8.3/static/sql-insert.html here.
;;
;; There is no ':init' function because it isn't needed by the underlying
;; library.  Just call ':connect'.
;;
;; <h3>A typical PgSQL session</h3>
;; The following code piece outlines a typical PgSQL session:
;;
;; @example
;; (module "postgres.lsp") ; load the module file
;;
;; (PgSQL:connect "192.168.1.10" "auser" "secret" "mydb") ; logon
;; (PgSQL:query "select ...;") ; SQL query
;; (PgSQL:query "insert ...;") ; SQL query
;;        ...
;; (PgSQL:close-db)

;; The database server is listening on IP 192.168.1.10. The program
;; connects with username '"auser"' password '"secret"' to a database with 
;; the name '"mydb"'. After connecting SQL statements are performed and
;; finally the program disconnects from the server.
;;
;; <h3>Bugs</h3>
;;
;; This module doesn't support connections through a Unix socket.

; make this module compatible with version less than 10.1.11
(when (< (sys-info -2) 10110)
	(constant (global '++) inc))

(context 'PgSQL)

(set 'files '(
	"/usr/local/lib/libpq.so.5.1" ; OpenBSD 4.6
	"/usr/lib/libpq.so" ; CentOS or other Linux
    "/usr/lib64/libpq.so" ; Linux 64bit
	"/usr/lib/libpq.so.5.1" ; Debian
	"/usr/local/pgsql/lib/libpq.dylib" ; Mac OS X
	"c:/Program Files/PostgreSQL/8.3/bin/libpq.dll" ; Win32
))

(set 'library (files (or
		     (find true (map file? files))
		     (throw-error "cannot find libpq library"))))

(import library "PQconnectdb" "cdecl")
(import library "PQstatus" "cdecl")
(import library "PQexec" "cdecl")
(import library "PQexecParams" "cdecl")
(import library "PQresultStatus" "cdecl")
(import library "PQresultErrorMessage" "cdecl")
(import library "PQgetvalue" "cdecl")
(import library "PQgetisnull" "cdecl")
(import library "PQcmdTuples" "cdecl")
(import library "PQntuples" "cdecl")
(import library "PQnfields" "cdecl")
(import library "PQfnumber" "cdecl")
(import library "PQclear" "cdecl")
(import library "PQfinish" "cdecl")

; On some wierd platforms NULL may equal something else, we'll worry about that when we bump into it.
(define NULL 0)
(define (NULL? n) (= 0 n))

(setq
  CONNECTION_OK 0
  CONNECTION_BAD 1
  PGRES_EMPTY_QUERY 0
  PGRES_COMMAND_OK 1
  PGRES_TUPLES_OK 2
  PGRES_COPY_OUT 3
  PGRES_COPY_IN 4
  PGRES_BAD_RESPONSE 5
  PGRES_NONFATAL_ERROR 6
  PGRES_FATAL_ERROR 7
)

;; @syntax (PgSQL:connect <str-server> <str-userID> <str-password> <str-db>)
;; @param <str-server> The host name or IP address or <tt>0</tt> for localhost.
;; @param <str-userID> The user ID for authentication.
;; @param <str-password> The password for authentication.
;; @param <str-db> The name of the database to connect to.
;; @return 'true' for success or 'nil' for failure.
;; Connects to a database on server and authenticates a user ID.

(define (connect host user passw dbname)
  (connectdb (string
	      (if (> (length host) 0) (string "host=" host) "")
	      (if (> (length user) 0) (string " user=" user) "")
	      (if (> (length passw) 0) (string " password=" passw) "")
	      (if (> (length dbname) 0) (string " dbname=" dbname) "")
	      )) )

;; @syntax (PgSQL:connectdb <str-conninfo>)
;; @param <str-conninfo> PostgreSQL Connection Parameters, To write an empty value or a value containing spaces, surround it with single quotes, e.g., keyword = 'a value'. keyword is 'host', 'hostaddr, 'port', 'dbname', 'user', 'password', 'connect_timeout', 'options', 'tty', 'sslmode', 'requiressl', 'krbsrvname', 'gsslib', 'service' in PostgreSQL 8.3.
;; @return 'true' for success or 'nil' for failure.
;; Connects to a database on server.

(define (connectdb conninfo)
    (close-db)
  (setq POSTGRES (PQconnectdb conninfo))
  (if (NULL? POSTGRES)
    nil
    (if (= (PQstatus POSTGRES) CONNECTION_OK)
      true
      (PQfinish POSTGRES)
      (setq POSTGRES nil))))

;; @syntax (PgSQL:query <str-sql> [<param> ...])
;; @param <str-sql> A valid SQL query string. If parameters are used, they are referred to in the command string as $1, $2, etc.
;; @param <param> Specifies the actual values of the parameters.
;; @return Returns a numeric status code
;; Sends a SQL query string to the database server for evaluation.
;; The return value will be one of the following: PGRES_EMPTY_QUERY,
;; PGRES_COMMAND_OK, PGRES_TUPLES_OK, PGRES_COPY_OUT, PGRES_COPY_IN,
;; PGRES_BAD_RESPONSE, PGRES_FATAL_ERROR
;;
;; From the libpq documentation:
;; <blockquote>
;;     If the result status is PGRES_TUPLES_OK, then the functions described
;;     below can be used to retrieve the rows returned by the query. Note that
;;     a SELECT command that happens to retrieve zero rows still shows
;;     PGRES_TUPLES_OK. PGRES_COMMAND_OK is for commands that can never return
;;     rows (INSERT, UPDATE, etc.). A response of PGRES_EMPTY_QUERY might
;;     indicate a bug in the client software. 
;; </blockquote>
;;

;; @example
;;
;; (PgSQL:query "select $1||$2" "abc" "def")
;; (PgSQL:fetch-all) ; -> (("abcdef"))
;;
;; (PgSQL:query "select $1 + $2" 10 20)
;; (PgSQL:fetch-all) ; -> (("30"))
;;
;; (PgSQL:query "select $1::timestamp + $2::interval" "2012-10-01 00:00:00" "123456 seconds")
;; (PgSQL:fetch-all) ; -> (("2012-10-02 10:17:36"))
;;
;; (PgSQL:query "create table tbl (a integer, b integer)")
;; (dotimes (i 10) (PgSQL:query "insert into tbl values ($1, $2)" i (* i 2)))
;; ;    a | b
;; ;   ---+----
;; ;    0 |  0
;; ;    1 |  2
;; ;    2 |  4
;; ;    ...
;; ;    9 | 18
;;
;; (PgSQL:query "select * from tbl where a=$1 or a=$2" 2 9)
;; (PgSQL:fetch-all) ; -> (("2" "4") ("9" "18"))

(define (query sql)
  (clear-result)
  (letn ((nParams (length (args)))
	 (params (map (lambda (argv) (string argv)) (args)))
	 (ptr-fmt (if (= (& (sys-info 9) 256) 0)
		      "lu" "Lu"))
	 (paramValues (if params
			 (pack (dup ptr-fmt nParams) params)
			 0)))
    (setq POSTGRES_RES
	  (PQexecParams POSTGRES sql nParams 0 paramValues 0 0 0))
    (if (not (NULL? POSTGRES_RES))
       (PQresultStatus POSTGRES_RES)
       PGRES_FATAL_ERROR)) )

;; @syntax (PgSQL:num-rows)
;; @return Number of rows from last query.

(define (num-rows)
  (PQntuples POSTGRES_RES))

;; @syntax (PgSQL:num-fields)
;; @return Number of columns from last query.

(define (num-fields)
  (PQnfields POSTGRES_RES))

;; @syntax (PgSQL:fnumber <str-column>)
;; @param <str-column> The column name.
;; @return the column number associated with the given column name. return nil when not found column name.

(define (fnumber name)
  (let (n (PQfnumber POSTGRES_RES name))
    (if (< n 0)
	nil
	n)))

;; @syntax (PgSQL:fetch-value <num-row> <col>)
;; @param <num-row> row number
;; @param <col> column number or column name string.
;; @return A single value
;; Fetches the single value in the row and column specified.  Used by the
;; ':fetch-row' and ':fetch-all' functions.  A field containing the 'NULL'
;; value will return the symbol 'NULL'
;;

(define (fetch-value row column)
  (if (string? column)
      (setq column (fnumber column)))
  (when column
    (let (s1 (PQgetvalue POSTGRES_RES row column))
      (if (NULL? s1)
	  nil
	  (let (s2 (get-string s1))
	    (if (= 1 (PQgetisnull POSTGRES_RES row column))
		'NULL s2))))) )

;; @syntax (PgSQL:fetch-row)
;; @return A list of field elements.
;; Fetches a row from a previous SQL 'PgSQL:query'  'select' statement.
;; Subsequent calls fetch row by row from the result table until the
;; end of the table is reached.

(define (fetch-row)
  (when (and (> (num-rows) 0) (> (num-rows) rowx))
    (let (row (map (fn (x) (fetch-value rowx x)) (sequence 0 (- (num-fields) 1))))
      (++ rowx)
      row)))

;; @syntax (PgSQL:fetch-all)
;; @return All rows/fields from the last query, or 'nil'
;; The whole result set from the query is returned at once as a list of row lists.

(define (fetch-all)
  (when (> (num-rows) 0)
    (data-seek 0)
    (map fetch-row (sequence 0 (- (num-rows) 1)))))

;; @syntax (PgSQL:databases)
;; @return A list of databases.
;; Performs the query 'SELECT datname FROM pg_database' which shows all the
;; database schemas hosted by the connected server.

(define (databases)
  (when (= PGRES_TUPLES_OK (query {SELECT datname FROM pg_database}))
    (map (fn (x) (x 0)) (fetch-all))))

;; @syntax (PgSQL:tables)
;; @return A list of tables in the database, or 'nil'
;; Performs the query 'SELECT table_name FROM information_schema.tables WHERE table_schema = &#039;public&#039;'

(define (tables)
  (when (= PGRES_TUPLES_OK (query {SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'}))
    (map (fn (x) (x 0)) (fetch-all))))

;; @syntax (PgSQL:fields <str-table>)
;; @param <str-table> The name of the table.
;; @return A list of fields in the table, or 'nil'
;; Unlike the equivalent function in the MySQL module, this function only shows
;; the names of all the fields in the given table.  It does not show the field
;; specification, which you would need to recreate the table.

(define (fields str-table)
  (when (= PGRES_TUPLES_OK (query (format {SELECT column_name FROM information_schema.columns WHERE table_name ='%s'} str-table)))
    (map (fn (x) (x 0)) (fetch-all))))

;; @syntax (PgSQL:data-seek <num-offset>)
;; @param <num-offset> The '0' based offset to position inside the data set.
;; @return Always 'true'. 
;; Sets a position in the result set which will be used by the next
;; 'PgSQL:fetch-row' call. If the offset is out of the allowed range for the
;; result set a subsequent fetch-row will return 'nil'.
 
(define (data-seek n) (setq rowx n) true)

;; @syntax (PgSQL:error)
;; @return A string containing the error message.
;; If there was no error, this function returns 'nil'.

(define (error)
  (let (s1 (PQresultErrorMessage POSTGRES_RES))
    (if (NULL? s1)
      nil
      (let (s2 (get-string s1))
	(if (= s "")
	  nil
	  s)))))

;; @syntax (PgSQL:affected-rows)
;; @return Number of rows affected by the last 'PgSQL:query' operation, or 'nil'
;; This function will only return a value following the execution of an INSERT,
;; UPDATE, DELETE, MOVE, FETCH, or COPY statement, or an EXECUTE of a prepared
;; query that contains an INSERT, UPDATE, or DELETE statement. It will return
;; 'nil' after all other queries.

(define (affected-rows)
    (let (s1 (PQcmdTuples POSTGRES_RES))
      (if (NULL? s1)
	nil
	(let (s2 (get-string s1))
	  (if (= s2 "")
	    nil
	    (int s))))))

;; @syntax (PgSQL:escape <str-sql>)
;; @return escaped string
;; This function will escape the ' character in <str-sql>, as per the SQL standard.
;; Depending on whether you
;; are using binary data or have configured Postgres to allow C escapes
;; you may need more advanced escaping than this function provides.

(define (escape)
  (replace {'} (apply string (args)) {''}))

(define (clear-result)
  (when (and POSTGRES_RES (not (NULL? POSTGRES_RES)))
    (PQclear POSTGRES_RES))
  (setq POSTGRES_RES nil rowx 0 colx 0))

;; @syntax (PgSQL:close-db)
;; @return Always 'true'.
;; Closes the database connection and frees associated resources.

(define (close-db)
  (clear-result)
  (when (and POSTGRES (not (NULL? POSTGRES)))
    (PQfinish POSTGRES))
  (setq POSTGRES nil)
  true)

(context MAIN)

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; test data base functions
;

(define (test-pgsql host user passw dbname)
  (unless (PgSQL:connect host user passw dbname)
    (println "PgSQL: couldn't connect") (exit 0))
  
  (println "databases:")
  (println (join (PgSQL:databases) ", "))
  (println)
  
  (PgSQL:query "CREATE SEQUENCE fruits_num_seq")
  (PgSQL:query "CREATE TABLE fruits (name varchar(2000), qty int, num int DEFAULT nextval('fruits_num_seq') NOT NULL)")
  
  (PgSQL:query "INSERT INTO fruits VALUES ('apples', 11)")
  (PgSQL:query "INSERT INTO fruits VALUES ('oranges', 22)")
  (PgSQL:query "INSERT INTO fruits VALUES ('bananas', 33)")

  (println "inserted into fruits:")
  (PgSQL:query "SELECT * FROM fruits")
  (println "\n" (PgSQL:affected-rows) " affected rows in query select")
  (dotimes (x (PgSQL:num-rows)) (println (PgSQL:fetch-row)))

  (println "num rows = " (PgSQL:num-rows) " num fields = " (PgSQL:num-fields))
  (println "fields = " (join (PgSQL:fields "fruits") ", "))
  (println)
  
  (println "tables:")
  (println (join (PgSQL:tables) ", "))
  (println)

  (PgSQL:query "SELECT * FROM fruits")
  (PgSQL:data-seek 2)
  (println "data-seek to offset 2:")
  (println (PgSQL:fetch-row))
  (println)
  
  (println "fetch-all:")
  (PgSQL:query "SELECT * FROM fruits")
  (println (PgSQL:fetch-all))

  (println "invalid data access (row 5, column 5): " (PgSQL:fetch-value 5 5))
  
  (PgSQL:query "DROP TABLE fruits")
  (PgSQL:query "DROP SEQUENCE fruits_num_seq")
  (PgSQL:close-db)
)

; eof
