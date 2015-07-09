;; @module postgres.lsp 
;; @description PostgreSQL interface (tested on PostgreSQL 9.4, should work on all supported versions)
;; @version 1.02  - feature complete
;; @version 1.03  - doc formatting
;; @version 2.00  - replaced <tt>inc</tt> with <tt>++</tt>
;; @version 2.10  - new <tt>fnumber</tt> and <tt>fetch-value</tt>
;; @version 2.11  - new <tt>query</tt> with optional parameters
;; @version 2.12  - add MacPorts path, Fix bugs (error), (affected-rows), (fields ...), test against PostgreSQL 9.4
;; @version 3.00  - add new postgreSQL imports, use pg_config to find libpq header, improve test coverage
;; @author Jeremy Cowgar 2006, Ted Walther 2009, Lutz Mueller 2010, Unya 2012, Neil Tiffin 2015
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
;; on LINUX/UNIX: 'libpq.so' <br>
;; on Mac OS X:   'libpq.dylib' <br>
;; on Windows:    'libpq.dll'
;;
;; This library is installed when using the install
;; package @link http://www.postgresql.org/download/ .
;; Unix, Linux, and Mac each have package installers that can be used
;; to install the PostgreSQL client (libpq) & server.
;;
;; Libpq might be in a different location on a particular
;; installation of PostgreSQL or have a different extension.
;; This module attempts to find libpq using pg_config.  If you can execute
;; pg_config at the command line and see its results then everything should just
;; work.  If not, then
;; you may have to edit the manual search code below to find your specific libpq.
;;
;; If you are using PostgreSQL from a Linux distribution you will need to install the
;; development headers for libpq, usually called libpq-dev.
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
;;   Connect
;;     PgSQL:connect ............. connect to a database
;;     PgSQL:close-db ............ close database connection
;;     PgSQL:error-conn .......... get connection error message *
;;
;;   Query
;;     PgSQL:query ............... execute a SQL statement
;;
;;   Query Results
;;     PgSQL:affected-rows ....... number of affected rows from operation
;;     PgSQL:data-seek ........... position in result for fetching
;;     PgSQL:error ............... get query error message
;;     PgSQL:fetch-all ........... get all rows from the last query
;;     PgSQL:fetch-row ........... get row from the query result
;;     PgSQL:fetch-value ......... get value from the query result
;;     PgSQL:fnumber ............. column number of query
;;     PgSQL:num-fields .......... columns in result of query
;;     PgSQL:num-rows ............ rows in result of query
;;
;;   Info
;;     PgSQL:database ............ return all database names
;;     PgSQL:fields .............. return all fields in a table
;;     PgSQL:tables .............. return all tables names
;;
;;   Misc
;;     PgSQL:escape .............. escapes single quote in input string 
;;     PgSQL:escape-literal ...... escapes literal for PostgreSQL *
;;     PgSQL:escape-identifier ... escapes identifier for PostgreSQL *
;;
;; * API may be specific to PostgreSQL
;; </pre>
;;
;; <h3>Differences from the MySQL module</h3>
;;
;; The function ':inserted-id' isn&#039;t supported because PostgreSQL
;; doesn&#039;t support it.  Instead, use the 'RETURNING' clause in your 'INSERT'
;; statement, then use ':fetch-row' or ':fetch-all' to
;; find the value.  'INSERT&nbsp;RETURNING' is a PostreSQL idiom
;; documented @link http://www.postgresql.org/docs/9.4/static/sql-insert.html here.
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
;; If the database server is running locally then "localhost" may be used for
;; the host name.
;;
;; <h3>Bugs</h3>
;;
;; This module doesn't support connections through a Unix socket.
;;
;; <h3>Implementation Notes</h3>
;; As of 19 March 2015.
;; On Windows only works with x86 (32bit) PostgreSQL install.
;; pg_config must be in the path.
;; Tested on OSX 10.10.2, Linux SMP Debian 3.16.7-ckt7-1, Windows 8.1 [version 6.3.9600]

; make this module compatible with version less than 10.1.11
(when (< (sys-info -2) 10110)
	(constant (global '++) inc))

(context 'PgSQL)

; get pg_config if available  
(set 'pg_lib_dir (exec "pg_config --libdir"))

(if pg_lib_dir
  (set 'files
    (list 
      (append (first pg_lib_dir) "/libpq.dylib")  ; shared Mac OS X libs
      (append (first pg_lib_dir) "/libpq.so")   ; loadable elf libs Posix Unix Linux
      (append (first pg_lib_dir) "/libpq.dll")  ; Windows lib
  ))
  (set 'files '(
    "/usr/local/lib/libpq.so.5.1" ; OpenBSD 4.6
    "/usr/lib/libpq.so" ; CentOS or other Linux
    "/usr/lib64/libpq.so" ; Linux 64bit
    "/usr/lib/libpq.so.5.1" ; Debian
    "/usr/local/pgsql/lib/libpq.dylib" ; Mac OS X
    "c:/Program Files/PostgreSQL/8.3/bin/libpq.dll" ; Win32
  )))

; find the library file
(set 'library (files (or
  (find true (map file? files))
  (throw-error "Cannot find libpq library!"))))

; done with these
(delete 'pg_config)
(delete 'pg_lib_dir)
(delete 'files)

; import functions and throw error if not found
(define (pg_import fun_name)
  (import library fun_name "cdecl"))

; import functions and print warning if not found
(define (pg_import_warn fun_name)
  (unless (catch (import library fun_name "cdecl") 'pg_load_error)
    (println "libpq import WARNING: " pg_load_error)))

; import functions available in libpq for 8.4 or earlier
; These are core functions and will throw an error if not found.
(map pg_import (list 
	"PQcancel"
	"PQclear"
	"PQcmdStatus"
	"PQcmdTuples"
	"PQconnectdb"
	;"PQconnectionNeedsPassword"	; PostgreSQL 8.3
	;"PQconnectionUsedPassword"		; PostgreSQL 8.3
	"PQconsumeInput"
	"PQdb"
	"PQdescribePortal"				; PostgreSQL 8.2
	"PQdescribePrepared"			; PostgreSQL 8.2
	;"PQencryptPassword"				; PostgreSQL 8.2
	"PQerrorMessage"
	"PQexec"
	"PQexecParams"
	"PQexecPrepared"					; PostgreSQL 7.4
	"PQfinish"
	"PQflush"
	"PQfname"
	"PQfnumber"
	"PQfreeCancel"
	"PQfreemem"								; PostgreSQL 7.4
	"PQgetCancel"
	"PQgetisnull"
	"PQgetvalue"
	"PQhost"
	"PQinstanceData"					; PostgreSQL 8.4
	"PQisBusy"
	"PQisnonblocking"
	"PQisthreadsafe"					; PostgreSQL 8.2
	"PQnfields"
	"PQnotifies"
	"PQntuples"
	"PQoptions"
	"PQpass"
	"PQport"
	"PQprepare"								; PostgreSQL 8.0
	"PQprotocolVersion"
	"PQregisterEventProc"			; PostgreSQL 8.4
	"PQresultErrorMessage"
	"PQresultInstanceData"		; PostgreSQL 8.4
	"PQresultSetInstanceData"	; PostgreSQL 8.4
	"PQresultStatus"
	"PQresStatus"
	"PQsendQueryPrepared"			; PostgreSQL 7.4
	"PQserverVersion"					; PostgreSQL 8.0
	"PQsetInstanceData"				; PostgreSQL 8.4
	"PQsetnonblocking"
	"PQsetNoticeProcessor"
	"PQsetNoticeReceiver"
	"PQstatus"
	"PQuser"
))

; Try to import new libpq functions and print warning (do not throw error) if not found.
; if any of these functions are used then the code in this module should verify
; that the respective function was actually loaded before use.
(map pg_import_warn (list 
	"PQconninfo"					; PostgreSQL 9.3
	"PQescapeIdentifier"	; PostgreSQL 9.1
	"PQescapeLiteral"			; PostgreSQL 9.1
	"PQlibVersion"				; PostgreSQL 9.0
	"PQping"							; PostgreSQL 9.0
	"PQpingParams"				; PostgreSQL 9.0
	"PQsetSingleRowMode"	; PostgreSQL 9.2
))

(delete 'pg_import_warn)
(delete 'pg_import)


; On some wierd platforms NULL may equal something else, we'll worry about that when we bump into it.
(define NULL 0)
(define (NULL? n) (= 0 n))

(setq
  CONNECTION_OK 0
  PGRES_FATAL_ERROR 7)

(set 'PQPING_STATUS '(
	"PQPING_OK"
	"PQPING_REJECT"
	"PQPING_NO_RESPONSE"
	"PQPING_NO_ATTEMPT"))

; module variables
(setq PG_CONN nil)			; pg connection structure
(setq PG_RESULT nil)		; pg query result structure
(setq PG_ROWX 0)				; current row
(setq PG_COLX 0)				; current column

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
;; @param <str-conninfo> PostgreSQL Connection Parameters, To write an empty value or a value containing spaces, surround it with single quotes, e.g., keyword = 'a value'. keyword is 'host', 'hostaddr, 'port', 'dbname', 'user', 'password', 'connect_timeout', 'client_encoding', 'options', 'application_name', 'fallback_application_name', 'keepalives', 'keepalives_idle', 'keepalives_interval', 'keepalives_count', 'tty', 'sslmode', 'sslcompression', 'sslcert', 'sslkey', 'sslrootcert', 'sslcrl', 'requirepeer', 'krbsrvname', 'gsslib', 'service' in PostgreSQL 9.4.
;; @return 'true' for success or nil on failure.
;; Connects to a database on server.

(define (connectdb conninfo)
  (close-db)
  (setq PG_CONN (PQconnectdb conninfo))
  (if (NULL? PG_CONN)
    nil
    (if (= (PQstatus PG_CONN) CONNECTION_OK)
      true
      (PQfinish PG_CONN)
      (setq PG_CONN nil))))


;; @syntax (PgSQL:query <str-sql> [<param> ...])
;; @param <str-sql> A valid SQL query string. If parameters are used, they are referred to in the command string as $1, $2, etc.
;; @param <param> Specifies the actual values of the parameters.
;; @return Returns a numeric status code
;; Sends a SQL query string to the database server for evaluation.
;; The return value will be an integer representing one of the following enumerated types: PGRES_EMPTY_QUERY,
;; PGRES_COMMAND_OK, PGRES_TUPLES_OK, PGRES_COPY_OUT, PGRES_COPY_IN,
;; PGRES_BAD_RESPONSE, PGRES_FATAL_ERROR.
;;
;; The numeric status code can be converted to a string using (PgSQL:result-str <status-code>).  
;; The numeric status code should not be used directly.
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
    (setq PG_RESULT
	  (PQexecParams PG_CONN sql nParams 0 paramValues 0 0 0))
    (if (not (NULL? PG_RESULT))
       (PQresultStatus PG_RESULT)
       PGRES_FATAL_ERROR)) )

;; @syntax (PgSQL:num-rows)
;; @return Number of rows from last query.

(define (num-rows)
  (PQntuples PG_RESULT))

;; @syntax (PgSQL:num-fields)
;; @return Number of columns from last query.

(define (num-fields)
  (PQnfields PG_RESULT))

;; @syntax (PgSQL:fnumber <str-column>)
;; @param <str-column> The column name.
;; @return the column number associated with the given column name. return nil when not found column name.

(define (fnumber name)
  (let (n (PQfnumber PG_RESULT name))
    (if (< n 0)
			nil
			n)))

;; @syntax (PgSQL:fname <int-column>)
;; @param <int-column> The integer column number.
;; @return the column name associated with the given column number. return nil when not found column name.

(define (fname column)
	(let (s1 (PQfname PG_RESULT (int column)))
		(if (= s1 0)
			nil
			(let (s2 (get-string s1))
				(if (= s2 "")
					nil
					s2)))))

;; @syntax (PgSQL:fetch-value <num-row> <col>)
;; @param <num-row> row number
;; @param <col> column number or column name string.
;; @return A single value
;; Fetches the single value in the row and column specified.  Used by the
;; ':fetch-row' and ':fetch-all' functions.  A field containing the 'NULL'
;; value will return the symbol 'NULL'

(define (fetch-value row column)
  (if (string? column)
      (setq column (fnumber column)))
  (when column
    (let (s1 (PQgetvalue PG_RESULT row column))
      (if (NULL? s1)
	  nil
	  (let (s2 (get-string s1))
	    (if (= 1 (PQgetisnull PG_RESULT row column))
		'NULL s2))))) )

;; @syntax (PgSQL:fetch-row)
;; @return A list of field elements.
;; Fetches a row from a previous SQL 'PgSQL:query'  'select' statement.
;; Subsequent calls fetch row by row from the result table until the
;; end of the table is reached.

(define (fetch-row)
  (when (and (> (num-rows) 0) (> (num-rows) PG_ROWX))
    (let (row (map (fn (x) (fetch-value PG_ROWX x)) (sequence 0 (- (num-fields) 1))))
      (++ PG_ROWX)
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
  (when (= "PGRES_TUPLES_OK" (result-status-str (query {SELECT datname FROM pg_database})))
    (map (fn (x) (x 0)) (fetch-all))))

;; @syntax (PgSQL:tables)
;; @return A list of tables in the database, or 'nil'
;; Performs the query 'SELECT table_name FROM information_schema.tables WHERE table_schema = &#039;public&#039;'

(define (tables)
  (when (= "PGRES_TUPLES_OK" (result-status-str (query {SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'})))
    (map (fn (x) (x 0)) (fetch-all))))

;; @syntax (PgSQL:fields <str-table>)
;; @param <str-table> The name of the table.
;; @return A list of fields in the table, or 'nil'
;; Unlike the equivalent function in the MySQL module, this function only shows
;; the names of all the fields in the given table.  It does not show the field
;; specification, which you would need to recreate the table.

(define (fields str-table)
  (let (sql (format {SELECT column_name FROM information_schema.columns WHERE table_name = %s} (escape-literal str-table)))
    (when (= "PGRES_TUPLES_OK" (result-status-str (query sql)))
      (map (fn (x) (x 0)) (fetch-all)))))

;; @syntax (PgSQL:data-seek <num-offset>)
;; @param <num-offset> The '0' based offset to position inside the data set.
;; @return Always 'true'. 
;; Sets a position in the result set which will be used by the next
;; 'PgSQL:fetch-row' call. If the offset is out of the allowed range for the
;; result set a subsequent fetch-row will return 'nil'.

(define (data-seek n) (setq PG_ROWX n) true)

;; @syntax (PgSQL:error)
;; @return A string containing the query result error message.
;; If there was no error, this function returns 'nil'.

(define (error)
  (if (= PG_RESULT nil)
    (throw-error "Results are not available.")
    (let (s1 (PQresultErrorMessage PG_RESULT))
      (if (NULL? s1)
        nil
        (let (s2 (get-string s1))
          (if (= s2 "")
            nil
            s2))))))

;; @syntax (PgSQL:affected-rows)
;; @return Number of rows affected by the last 'PgSQL:query' operation, or 'nil'
;; This function will only return a value following the execution of an INSERT,
;; UPDATE, DELETE, MOVE, FETCH, or COPY statement, or an EXECUTE of a prepared
;; query that contains an INSERT, UPDATE, or DELETE statement. It will return
;; 'nil' after all other queries.

(define (affected-rows)
  (if (= PG_RESULT nil)
    nil
		(let (s1 (PQcmdTuples PG_RESULT))
			(if (NULL? s1)
				nil
				(let (s2 (get-string s1))
					(if (= s2 "")
						nil
						(int s2)))))))

;; @syntax (PgSQL:error-conn)
;; @return error message string
;; Returns the error message most recently generated by an operation on the connection.

(define (error-conn)
  (if (= PG_CONN nil)
    (throw-error "Connection is not available.")
    (let (s1 (PQerrorMessage PG_CONN))
      (if (NULL? s1)
        nil
        (let (s2 (get-string s1))
          (if (= s2 "")
            nil
            s2))))))

;; @syntax (PgSQL:escape <str-sql>)
;; @return escaped string
;; This function only escapes the ' character in <str-sql>, as per the SQL standard.
;; Depending on whether you
;; are using binary data or have configured Postgres to allow C escapes
;; you may need more advanced escaping than this function provides.

(define (escape)
  (replace {'} (apply string (args)) {''}))

;; @syntax (PgSQL:escape-literal <str>)
;; @param <str> string to be escaped
;; @return escaped string
;; This function escapes a string for use within an SQL command. This is
;; useful when inserting data values as literal constants in SQL commands. Certain
;; characters (such as quotes and backslashes) must be escaped to prevent them from
;; being interpreted specially by the SQL parser. escape-literal performs this
;; operation.  This function was added in PostgreSQL 9.1 and will throw an error if
;; you are using an older libpq.

(define (escape-literal x)
  (if (primitive? PQescapeLiteral)
		(if (= PG_CONN nil)
			(throw-error "Must be connected to a database.")
			(let (pg_raw_result (PQescapeLiteral PG_CONN x (length (get-string x))))
			(if (= pg_raw_result nil)
				(throw-error (error-conn))
				(let (pg_result (get-string pg_raw_result))
					(PQfreemem pg_raw_result)
					pg_result))))
  	(throw-error "PQescapeLiteral not available.")))

;; @syntax (PgSQL:escape-identifier <str>)
;; @param <str> string to be escaped
;; @return escaped string
;; This function escapes a string for use as an SQL identifier, such as a
;; table, column, or function name. This is useful when a user-supplied identifier
;; might contain special characters that would otherwise not be interpreted as part
;; of the identifier by the SQL parser, or when the identifier might contain upper
;; case characters whose case should be preserved. This function was added in 
;; PostgreSQL 9.1 and will throw an error if you are using an older libpq.

(define (escape-identifier x)
  (if (primitive? PQescapeIdentifier)
		(if (= PG_CONN nil)
			(throw-error "Must be connected to a database.")
			(let (pg_raw_result (PQescapeIdentifier PG_CONN x (length (get-string x))))
			(if (= pg_raw_result nil)
				(throw-error (error-conn))
				(let (pg_result (get-string pg_raw_result))
					(PQfreemem pg_raw_result)
					pg_result))))
  	(throw-error "PQescapeIdentifier not available.")))

(define (clear-result)
  (when (and PG_RESULT (not (NULL? PG_RESULT)))
    (PQclear PG_RESULT))
  (setq PG_RESULT nil PG_ROWX 0 PG_COLX 0))

;; @syntax (PgSQL:close-db)
;; @return Always 'true'.
;; Closes the database connection and frees associated resources.

(define (close-db)
  (clear-result)
  (when (and PG_CONN (not (NULL? PG_CONN)))
    (PQfinish PG_CONN))
  (setq PG_CONN nil)
  true)

;; @syntax (PgSQL:host)
;; @return connected host name string
(define (host)
  (if (= PG_CONN nil)
    (throw-error "Connection is not established.")
    (let (s1 (PQhost PG_CONN))
      (if (NULL? s1)
        nil
        (let (s2 (get-string s1))
          (if (= s2 "")
            nil
            s2))))))

;; @syntax (PgSQL:port)
;; @return connected port name string
(define (port)
  (if (= PG_CONN nil)
    (throw-error "Connection is not established.")
    (let (s1 (PQport PG_CONN))
      (if (NULL? s1)
        nil
        (let (s2 (get-string s1))
          (if (= s2 "")
            nil
            s2))))))

;; @syntax (PgSQL:db)
;; @return connected database name string
(define (db)
  (if (= PG_CONN nil)
    (throw-error "Connection is not established.")
    (let (s1 (PQdb PG_CONN))
      (if (NULL? s1)
        nil
        (let (s2 (get-string s1))
          (if (= s2 "")
            nil
            s2))))))

;; @syntax (PgSQL:options)
;; @return connected options string
(define (options)
  (if (= PG_CONN nil)
    (throw-error "Connection is not established.")
    (let (s1 (PQoptions PG_CONN))
      (if (NULL? s1)
        nil
        (let (s2 (get-string s1))
          (if (= s2 "")
            nil
            s2))))))

;; @syntax (PgSQL:user)
;; @return connected user name string
(define (user)
  (if (= PG_CONN nil)
    (throw-error "Connection is not established.")
    (let (s1 (PQuser PG_CONN))
      (if (NULL? s1)
        nil
        (let (s2 (get-string s1))
          (if (= s2 "")
            nil
            s2))))))

;; @syntax (PgSQL:result-str <int-status-code>)
;; @param <int-status-code> An integer query result status code
;; @return string for the given integer status code
;; The return value will be one of the following strings "PGRES_EMPTY_QUERY",
;; "PGRES_COMMAND_OK", "PGRES_TUPLES_OK", "PGRES_COPY_OUT", "PGRES_COPY_IN",
;; "PGRES_BAD_RESPONSE", "PGRES_FATAL_ERROR".

(define (result-status-str status-code)
    (let (s1 (PQresStatus status-code))
      (if (NULL? s1)
        nil
        (let (s2 (get-string s1))
          (if (= s2 "")
            nil
            s2)))))
	
; verify that the constant PGRES_FATAL_ERROR has the correct integer value
(if (!= "PGRES_FATAL_ERROR" (result-status-str PGRES_FATAL_ERROR) )
	(throw-error 
	  (append "PGRES_FATAL_ERROR defined as " 
	    (string PGRES_FATAL_ERROR) 
	    ", but PostgreSQL has that defined as "
	    (result-status-str PGRES_FATAL_ERROR) )))

;; @syntax (PgSQL:lib-version)
;; @return libpq version as string (e.g. "9.1.1")
(define (lib-version)
  (if (= PQlibVersion nil)
    "Not Available - Pre 9.0"
    (begin
      (regex {(\d+?)(\d\d)(\d\d)$} (string (PQlibVersion)))
      (append $1 "." $2 "." $3))))

;; @syntax (PgSQL:protocol-version)
;; @return protocol version "2", or "3" as string, "0" bad connection.
;; Interrogates the frontend/backend protocol being used.
(define (protocol-version)
  (if (= PG_CONN nil)
    (throw-error "Connection is not established.")
    (string (PQprotocolVersion PG_CONN))))

;; @syntax (PgSQL:server-version)
;; @return backend server version as string (e.g. "9.1.1")
(define (server-version)
  (if (= PG_CONN nil)
    (throw-error "Connection is not established.")
    (begin
      (regex {(\d+?)(\d\d)(\d\d)$} (string (PQserverVersion PG_CONN)))
      (append $1 "." $2 "." $3))))
	
(context MAIN)


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; test data base functions
;
; Check PgSQL Test Coverage 
;   Function NOT Tested: PgSQL:clear-result
;   Percent PgSQL Coverage: 96.9

(define (test-pgsql host user passw dbname)
  (unless (PgSQL:connect host user passw dbname)
    (println "PgSQL: couldn't connect 1st try.")
    (exit 0))

  ; clean up in case the previous test failed
  (PgSQL:query "DROP TABLE fruits")

  (unless (PgSQL:connectdb (format {host='%s' user='%s' password='%s' dbname='%s'} (PgSQL:escape host) (PgSQL:escape user) (PgSQL:escape passw) (PgSQL:escape dbname)))
    (println "PgSQL: couldn't connect 2nd try.")
    (exit 0))

  (if (not (PgSQL:NULL? 0))
  	(throw-error "NULL not valid."))
  (if (PgSQL:NULL? 5)
  	(throw-error "NULL not valid."))
  
  (println "\nTest Connection Info Retrieval:")
  (println "    database: " (PgSQL:db))
  (println "        user: " (PgSQL:user))
  (println "        host: " (PgSQL:host))
  (println "        port: " (PgSQL:port))
  (println "     options: " (PgSQL:options))
  (println "   libpq ver: " (PgSQL:lib-version))
  (println "  server ver: " (PgSQL:server-version))
  (println "protocol ver: " (PgSQL:protocol-version))

  (println "\nTest List Databases:")
  (println (join (PgSQL:databases) ", "))
  
  (println "\nTest Create Test Data")
  (PgSQL:query "CREATE TABLE fruits (name varchar, qty int, num serial NOT NULL)")
  
  (set 'test_data (list (list "apples" 11) (list "oranges" 22) (list "bananas" 33) (list "pears" 44)))  
  (println test_data)
  
  (dolist (x test_data)
  	(println "Insert result: " (PgSQL:result-status-str (PgSQL:query "INSERT INTO fruits VALUES ($1, $2)" (nth 0 x) (nth 1 x))) " - " (nth 0 x) ", " (nth 1 x))
  	(let (myCount (PgSQL:affected-rows))
			(if (or (nil? myCount) (zero? myCount))
				(throw-error "Insert did not work."))))
  
  (println "\nTest Inserted into fruits:")
  (setq q "SELECT * FROM fruits ORDER BY name;")
  (setq q-status (PgSQL:query q))
  (println "         select query: " q)

  (println "  select query result: " (PgSQL:result-status-str q-status))
	(if (!= "PGRES_TUPLES_OK" (PgSQL:result-status-str q-status))
		(throw-error "query result not ok."))
		
  (println "        rows returned: " (PgSQL:num-rows))
  (if (!= 4 (PgSQL:num-rows))
  	(throw-error "Wrong number of rows." ))
  	
  (println "     columns returned: " (PgSQL:num-fields))
  (if (!= 3 (PgSQL:num-fields))
  	(throw-error "Wrong number of columns." ))

  (println "  'qty' column number: " (PgSQL:fnumber "qty"))
	(if (!= 1 (PgSQL:fnumber "qty"))
		(throw-error "column number not correct."))
	
  ; gather column names from result
  (set 'column_names (let (x -1)
  										(collect (PgSQL:fname (inc x)) (PgSQL:num-fields))))							
  (println "column names returned: " (join column_names ", "))

  (dotimes (x (PgSQL:num-rows)) (println "                  row: " (PgSQL:fetch-row)))
  (println "   table column names: " (join (PgSQL:fields "fruits") ", "))


  (println "\nTest List Tables:")
  (println (join (PgSQL:tables) ", "))

  (println "\nTest fetch-row and data-seek to offset 2:")
  (PgSQL:query "SELECT * FROM fruits")
  (PgSQL:data-seek 2)
  (println (PgSQL:fetch-row))
  
  (println "\nTest fetch-all:")
  (PgSQL:query "SELECT * FROM fruits")
  (println (PgSQL:fetch-all))

  (println "\nTest Fetching Out of Range Values:")
  (println "invalid data access (row 5, column 5): " (PgSQL:fetch-value 5 5))
  
  ; clean up after DB tests
  (PgSQL:query "DROP TABLE fruits")
  
  (println "\nTest PG Error Retrieval:")
  (PgSQL:query "SELECT * FROM fruittyyyy")
  (println "Should show result error on next line.\n" (PgSQL:error))
  (println "Should show connection error (same as above) on next line.\n" (PgSQL:error-conn))
  
  (println "\nTest Text Escaping:")
  (println "   Check escape literal: " (PgSQL:escape-literal "group's"))
  (println "Check escape identifier: " (PgSQL:escape-identifier "group's"))

  (println "\nTest Query PG Result Conversion to String:")
  (println "result 0: " (PgSQL:result-status-str 0))
  (println "result 1: " (PgSQL:result-status-str 1))
  (println "result 2: " (PgSQL:result-status-str 2))
  (println "result 3: " (PgSQL:result-status-str 3))
  (println "result 4: " (PgSQL:result-status-str 4))
  (println "result 5: " (PgSQL:result-status-str 5))
  (println "result 6: " (PgSQL:result-status-str 6))
  (println "result 7: " (PgSQL:result-status-str 7))
  (println "result 8: " (PgSQL:result-status-str 8))
  (println "result 9: " (PgSQL:result-status-str 9))

  (PgSQL:close-db)
  
	(println)
  (println "Tests Completed.")
	(println)
)

; eof
