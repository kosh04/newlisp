;; @module mysql51.lsp 
;; @description MySQL v.5.1 interface
;; @version 2.61 - addition for mysql_escape_string (Jeff Ober)
;; @version 2.62 - fix for mysql_escape_string (Tim Johnson)
;; @author Lutz Mueller 2003-2006, Gordon Fischer 2005, Jeff Ober 2007
;;
;; This MySQL 5.1 interface module has been tested on version 5.1.21
;; of mysql from @link http://www.mysql.com www.mysql.com
;;
;; This implementation supports a maximum of 2,147,483,647
;; rows in a database table. Now automatically adjusts row indexes to
;; endian type of host CPU, but higher 32 bits are treated as 0 for now.
;;
;; <h3>Requirements</h3>
;; At the beginning of the program file include a 'load' statment for the module:
;; <pre>
;; (load "/usr/share/newlisp/mysql5.lsp")
;; </pre>
;;
;; A version of 'libmysqlclient' for a specific platform is required:
;;  
;; on LINUX/UNIX: '/usr/local/mysql/libmysqlclient.so' <br>
;; on Mac OS X:   '/usr/local/mysql/libmysqlclient.dylib'
;;
;; To compile MySQL with client libraries use:
;;
;; './configure --prefix=/usr/local --enable-shared'
;;
;; This library might be in a different location on a particular
;; installation of MySQL or have a different name.
;; Change accordingly in the code at the beginning.
;;
;; The MySQL server itself may reside on a different machine
;; on the network. The library 'libmysqlclient' will communicate
;; with that server. The correct connection is created using
;; the 'MySQL:connect' call.
;;
;; At the bottom of the module file 'mysql51r.lsp' a test routine 'test-mysql'
;; is included to test for correct installation of MySQL.
;;
;; In the 'MySQL:connect' call of that test routine the correct parameters 
;; for the MySQL server location and user and password have to be inserted.
;;
;; <h3>Adapting mysql.lsp to other versions of MySQL</h3>
;; Some of the functions like 'mysql_num_rows()' cannot be imported
;; because they are really macros extracting data from structures
;; like 'MYSQL' or 'MYSQL_RES'. See the file 'mysql.h' in your MySQL distribution.
;;
;; The file 'sql.c' in the newLISP distribution contains a program
;; calculating the offsets of the most important fields in these
;; structures. These offsets are used here to retrieve values for
;; the number of rows in a result set, etc. Using these offsets
;; and the information found in 'mysql.h' and 'mysql_com.h', other
;; functions can be imported and wrappers built around them.
;; In this case one needs to install the developer's version of
;; MySQL to get the header files mentioned.
;;
;; <h3>Functions available</h3>
;; <pre>
;;     MySQL:init ................ get a database handle MYSQL
;;     MySQL:connect ............. connect to a database
;;     MySQL:query ............... execute a SQL statement
;;     MySQL:num-rows ............ rows in result of query
;;     MySQL:num-fields .......... columns in result of query
;;     MySQL:fetch-row ........... get row from the query result
;;     MySQL:fetch-all ........... get all rows from the last query
;;     MySQL:database ............ return all database names
;;     MySQL:tables .............. return all tables names
;;     MySQL:fields .............. return all fields in a table
;;     MySQL:data-seek ........... position in result for fetching
;;     MySQL:affected-rows ....... number of affected rows from operation
;;     MySQL:inserted-id ......... last value of auto increment id operation
;;     MySQL:escape .............. escapes SQL input string using mysql_real_escape_string
;;     MySQL:error ............... get error message
;;     MySQL:close-db ............ close database connection
;; </pre>
;;
;; <h3>A typical MySQL session</h3>
;; The following code piece outlines a typical MySQL session:
;; @example
;; (load "mysql51.lsp) ; load the module file
;;
;; (MySQL:init)       ; initialize
;; (MySQL:connect "192.168.1.10" "auser" "secret" "mydb") ; logon
;; (MySQL:query "select ...;") ; SQL query
;; (MySQL:query "insert ...;") ; SQL query
;;        ...
;; (MySQL:close-db)

;; The database server is listening on IP 192.168.1.10. The program
;; connects with username '"auser"' password '"secret"' to a database with 
;; the name '"mydb"'. After connecting SQL statements are performed and
;; finally the program disconnects from the server.

(context 'MySQL)

(set 'files '(
	"/usr/lib/libmysqlclient.so" ; Linux, UNIX
	"/usr/local/mysql/lib/libmysqlclient.dylib" ; MacOS X
))

(set 'libmysqlclient (files (or 
				(find true (map file? files)) 
				(begin (println "cannot find libmysqlclient library") (exit)))))

(import libmysqlclient "mysql_init")
(import libmysqlclient "mysql_real_connect")
(import libmysqlclient "mysql_get_host_info")
(import libmysqlclient "mysql_real_escape_string")
(import libmysqlclient "mysql_query")
(import libmysqlclient "mysql_real_query")
(import libmysqlclient "mysql_store_result")
(import libmysqlclient "mysql_free_result")
(import libmysqlclient "mysql_data_seek")
(import libmysqlclient "mysql_fetch_row")
(import libmysqlclient "mysql_close")
(import libmysqlclient "mysql_fetch_field_direct")
(import libmysqlclient "mysql_insert_id")

; following constant offsets into 'C' data structures are different on each mayor MySQL
; version compile and run util/sql.c from the distribution to obtain these numbers

; check endianess of the host CPU 
(set 'big-endian (= (pack ">ld" 1) (pack "ld" 1)))

(constant 'NUM_ROWS_OFFSET (if big-endian 4 0))
(constant 'NUM_FIELDS_OFFSET 72)
(constant 'ERROR_OFFSET 95)
(constant 'INSERT_ID_OFFSET (if big-endian 708 704))
(constant 'AFFECTED_ROWS_OFFSET (if big-endian 700 696))

;; @syntax (MySQL:init)
;; @return 'true' on success, 'nil' on failure.

(define (init)
  (set 'MYSQL (mysql_init 0))
  (if (= MYSQL 0) (set 'MYSQL nil))
  (not (= MYSQL nil)))

;; @syntax (MySQL:connect <str-server> <str-userID> <str-password> <str-db>)
;; @param <str-server> The host name or IP address or <tt>0</tt> for localhost.
;; @param <str-userID> The user ID for authentication.
;; @param <str-password> The password for authentication.
;; @param <str-db> The name of the database to connect to.
;; @return 'true' for success or 'nil' for failure.
;;
;; Connects to a database on server and authenticates a user ID.
;; '(MySQL:init)' must have been called previously.

(define (connect host user passw database)
  (not(= (mysql_real_connect MYSQL host user passw database 0 0 0) 0)))

;; @syntax (MySQL:query <str-sql>)
;; @param <str-sql> A valid SQL query string.
;; @return For 'insert' queries rerturns the inserted ID else 'true' 
;; for success or 'nil' for failure.
;;
;; Sends a SQL query string to the database server for evaluation.

(define (MySQL:query sql)
  (if MYSQL_RES (mysql_free_result MYSQL_RES))
  (set 'result  (= (mysql_query MYSQL sql) 0))
  (set 'MYSQL_RES (mysql_store_result MYSQL)) 
  (if (= MYSQL_RES 0) (set 'MYSQL_RES nil))
  (if (and result (find "insert into" sql 1)) (set 'result (inserted-id)))
  result)

;; @syntax (MySQL:num-rows)
;; @return Number of rows from last query.

(define (num-rows)
  (if MYSQL_RES (get-int (int (+ MYSQL_RES NUM_ROWS_OFFSET)))))

;; @syntax (MySQL:num-fields)
;; @return Number of columns from last query.

(define (num-fields)
  (if MYSQL_RES (get-int (int (+ MYSQL_RES NUM_FIELDS_OFFSET)))))


; format the result based on the field type.
;

(define (keep-type res_ptr field_addr column_num, data)
  (set 'type_ptr (mysql_fetch_field_direct res_ptr (int column_num)))
  ; The field type is the 20th field of the MySQL_FIELD structure
  ; since fields 1-19 are all 4 byte fields we get the enum value
  ; like so
  (set 'data (get-int (int (+ type_ptr (* 19 4)))))
  ; Consult 'enum_field_types' in mysql_com.h for values
  (if (= data 1) ;; boolean
        (get-string field_addr)
      (= data 3) ;; integer
        (int (get-string field_addr))
      (= data 12) ;; datetime
        (apply date-value (map int (parse (get-string field_addr) "[-: ]" 0)))
      (= data 4) ;; float
        (float (get-string field_addr))
      ; else (will handle TEXT type 252)
      (get-string field_addr)
  )
)

;; @syntax (MySQL:fetch-row)
;; @return A list of field elements.
;;
;; Fetches a row from a previous SQL 'MySQL:query'  'select' statement.
;; Subsequent calls fetch row by row from the result table until the
;; end of the table is reached.

(define (fetch-row)
  (if MYSQL_RES
    (set 'rdata (mysql_fetch_row MYSQL_RES))
    (set 'rdata 0))
  (if (!= rdata 0)
    (begin
      (set 'row '())
      (dotimes (field (num-fields))
            (set 'field_addr (get-int (int (+ rdata (* field 4)))))
            (if (= field_addr 0)
              (push nil row -1) ;; what to do when the field contains NULL
              (push (keep-type MYSQL_RES field_addr field) row -1)))
    row))) ; not necessary starting v 9.9.5, because push returns the list

;; @syntax (MySQL:fetch-all)
;; @return All rows/fields from the last query.
;;
;; The whole result set from the query is returned at once as a list of row lists.

(define (fetch-all , all)
  (dotimes (x (num-rows)) (push (fetch-row) all))
  (reverse all)) ; can be written shorter starting version 9.9.5
                 ; because push returns the list modified

;; @syntax (MySQL:databases)
;; @return A list of databases.
;;
;; Performs a 'show databases;' query.

(define (databases)
  (query "show databases;")
  (fetch-all))

;; @syntax (MySQL:table)
;; @return A list of tables in the database.
;;
;; Performs a 'show tables;' query.

(define (tables)
  (query "show tables;")
  (fetch-all))

;; @syntax (MySQL:fields <str-table>)
;; @param <str-table> The name of the table.
;; @return A list of field description lists.
;;
;; For each field name in the table a list of specifications
;; for that field is returned. The list starts with the name
;; for the field followed by the type size/precision and
;; other optional field descriptions.

(define (fields table)
  (query (append "show fields from " table ";"))
  (fetch-all))
  
;; @syntax (MySQL:data-seek <num-offset>)
;; @param <num-offset> The <tt>0</tt> based offset to position inside the data set.
;; @return Always 'true'. 
;; 
;; Positions in the result set at a zero based offset
;; for a subsequent 'MySQL:fetch-row' call. If the offset
;; is out of the allowed range for the result set a subsequent
;; fetch-row will return 'nil'.
 
(define (data-seek offset)
  (if MYSQL_RES
    (if big-endian
    	(mysql_data_seek MYSQL_RES  0 (int offset))
        (mysql_data_seek MYSQL_RES (int offset) 0)))
  true
)

;; @syntax (MySQL:error)
;; @return Text info about the last error which occured.

(define (error)
  (if MYSQL (get-string (+ MYSQL ERROR_OFFSET))))


;; @syntax (MySQL:affected-rows)
;; @return Number of affected rows by the last 'MySQL:query' operation.

(define (affected-rows)
  (if MYSQL
    (get-int (int (+ MYSQL AFFECTED_ROWS_OFFSET)))))

;; @syntax (MySQL:inserted-id)
;; @return Last insert ID from an auto increment field.

(define (inserted-id)
;  (if MYSQL (mysql_insert_id MYSQL))
  (if MYSQL (get-int (int (+ MYSQL INSERT_ID_OFFSET)))))

;; @syntax (MySQL:escape <str-sql>)
;; @return escaped string
;;
;; This function will escape special characters in <str-sql>, so that it 
;; is safe to place it in a MySQL query.
(define (escape value , safe-value) 
  (set 'safe-value (dup " " (+ 1 (* 2 (length value)))))
  (MySQL:mysql_real_escape_string MySQL:MYSQL safe-value value (length value)) 
  safe-value)

;; @syntax (MySQL:close-db)
;; @return Always 'true'.
;;
;; Closes database access. For new database acess, both 'MySQL:init' and 
;; 'MySQL:connect' functions have to be called.

(define (close-db)
  (if MYSQL_RES (mysql_free_result MYSQL_RES))
  (if MYSQL (mysql_close MYSQL))
  (set 'MYSQL nil)
  (set 'MYSQL_RES nil)
  true)

(context 'MAIN)

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; test data base functions
;

(define (test-mysql)
  (MySQL:init)
  (MySQL:connect 0 "" 0 "test")
  
  (println "databases:")
  (MySQL:query "show databases;")

  (dotimes (x (MySQL:num-rows)) (println (MySQL:fetch-row)))
  (println)
  
  (MySQL:query "create table fruits (name TEXT(2000), 
                                     qty INT(3),
                                     num INT(4) AUTO_INCREMENT UNIQUE);")
  
  (MySQL:query "insert into fruits values ('apples', 11, null);")
  (println "inserted-id: " (MySQL:inserted-id))
  (MySQL:query "insert into fruits values ('oranges', 22, null);")
  (println "inserted-id: " (MySQL:inserted-id))
  (MySQL:query "insert into fruits values ('bananas', 33, null);")
  (println "inserted-id: " (MySQL:inserted-id))

  (println "inserted into fruits:")
  (MySQL:query "select * from fruits;")
  (println "\n" (MySQL:affected-rows) " affected rows in query select")
  (dotimes (x (MySQL:num-rows)) (println (MySQL:fetch-row)))

  (println "no rows = " (MySQL:num-rows) " no fields = " (MySQL:num-fields))
  (println "fields = " (MySQL:fields "fruits"))
  (println)
  
  (println "tables:")
  (MySQL:query "show tables;")
  (dotimes (x (MySQL:num-rows)) (println (MySQL:fetch-row)))
  (println)
  
  (MySQL:query "select * from fruits;")
  (MySQL:data-seek 2)

  (println "data-seek to offset 2:")
  (println (MySQL:fetch-row))
  (println)
  
  (println "fetch-all:")
  (println (MySQL:query "select * from fruits;"))
  (println (MySQL:fetch-all))   
  
  (MySQL:query "drop table fruits;")
  (MySQL:close-db)
)

; eof
