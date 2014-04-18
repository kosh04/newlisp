;; @module mysql.lsp 
;; @description MySQL v.5.x interface (tested on MySQL 5.0 and 5.1)
;; @version 2.61 - addition for mysql_escape_string (Jeff Ober)
;; @version 2.62 - fix for mysql_escape_string (Tim Johnson)
;; @version 3.0  - module now independent of C-structure offsets
;; @version 3.1  - module now senses if running 64-bit version of newLISP
;; @version 3.2  - a fix when fetch-all has an empty result
;; @version 3.3  - typo in fetch-all didn't delete old fetches
;; @version 3.4  - documentaion error for load path
;; @version 3.41  - library load path for Fedora Linux
;; @version 3.42  - library load path upgraded for OpenBSD 4.9
;; @version 3.43  - library load path upgraded for CentOS 6.x 
;; @version 3.44  - library load path upgraded for CentOS 6.x 
;; @version 3.45  - library load path upgraded for UBUNTU Linux 12.04
;; @author Lutz Mueller 2003-2010, Gordon Fischer 2005, Jeff Ober 2007
;;
;; This MySQL 5.x interface module has been tested on versions 5.0 and 5.1
;; of mysql from @link http://www.mysql.com www.mysql.com
;;
;; An alternate implementation of a MySQL module with more features 
;; is available at @link http://static.artfulcode.net/newlisp/index.html ArtfulCode.
;;
;; <h3>Requirements</h3>
;; At the beginning of the program file include a 'load' statement for the module:
;; <pre>
;; (load "/usr/share/newlisp/modules/mysql.lsp")
;; ; or shorter
;; (module "mysql.lsp")
;; </pre>
;;
;; A version of 'libmysqlclient' for a specific platform is required:
;;  
;; on LINUX/UNIX: '/usr/local/mysql/lib/libmysqlclient.so' <br>
;; on Mac OS X:   '/usr/local/mysql/lib/libmysqlclient.dylib'
;;
;; This library is installed when using the Mac OS X x86 installer .dmg package
;; from @link http://www.mysql.com http://www.mysql.com
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
;; At the bottom of the module file 'mysql51.lsp' a test routine 'test-mysql'
;; is included to test for correct installation of MySQL.
;;
;; In the 'MySQL:connect' call of that test routine the correct parameters 
;; for the MySQL server location and user and password have to be inserted.
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
;; <h3>A typical MySQL session</h3>
;; The following code piece outlines a typical MySQL session:
;; @example
;; (module "mysql.lsp) ; load the module file
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

; fetch-row and keep-type functions depend on this
(set 'NEWLISP64 (not (zero? (& (sys-info -1) 256))))

(set 'files '(
    "/usr/local/lib/libmysqlclient.so.20.0" ; OpenBSD 4.9
    "/usr/lib/libmysqlclient.so" ; Linux, UNIX
    "/usr/lib/mysql/libmysqlclient.so" ; Linux Fedora
    "/usr/lib64/mysql/libmysqlclient.so" ; Linux Fedora CentOS 6.x
    "/usr/lib/x86_64-linux-gnu/libmysqlclient.so" ; Ubuntu 12.04 LTS
    "/usr/local/mysql/lib/libmysqlclient.so" ; Linux, UNIX
    "/usr/local/mysql/lib/libmysqlclient.dylib" ; MacOS X
    "/usr/lib/libmysqlclient.dylib" ; MacOS X
))

(set 'library (files (or 
		       (find true (map file? files)) 
		       (throw-error "cannot find libmysqlclient library"))))

(import library "mysql_init")
(import library "mysql_real_connect")
(import library "mysql_get_host_info")
(import library "mysql_real_escape_string")
(import library "mysql_query")
(import library "mysql_real_query")
(import library "mysql_store_result")
(import library "mysql_free_result")
(import library "mysql_data_seek")
(import library "mysql_fetch_row")
(import library "mysql_close")
(import library "mysql_fetch_field_direct")
(import library "mysql_insert_id")
(import library "mysql_num_rows")
(import library "mysql_num_fields")
(import library "mysql_affected_rows")
(import library "mysql_error")

; check endianess of the host CPU 
(set 'big-endian (= (pack ">ld" 1) (pack "ld" 1)))

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
;; Connects to a database on server and authenticates a user ID.
;; '(MySQL:init)' must have been called previously.

(define (connect host user passw database)
  (not(= (mysql_real_connect MYSQL host user passw database 0 0 0) 0)))

;; @syntax (MySQL:query <str-sql>)
;; @param <str-sql> A valid SQL query string.
;; @return For 'insert' queries rerturns the inserted ID else 'true' for success or 'nil' for failure.
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
  (if MYSQL_RES (mysql_num_rows MYSQL_RES)))

;; @syntax (MySQL:num-fields)
;; @return Number of columns from last query.

(define (num-fields)
  (if MYSQL_RES (mysql_num_fields MYSQL_RES)))


; format the result based on the field type.
;

(define (keep-type res_ptr field_addr column_num, data)
  (set 'type_ptr (mysql_fetch_field_direct res_ptr (int column_num)))
  ; The field type is the 20th field of the MySQL_FIELD structure
  ; since fields 1-19 are all 4 byte fields we get the enum value
  ; like so
  (if NEWLISP64
  	(set 'data (get-long (int (+ type_ptr (* 19 8)))))
  	(set 'data (get-int (int (+ type_ptr (* 19 4))))) )
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
            (if NEWLISP64
              (set 'field_addr (get-long (int (+ rdata (* field 8)))))
              (set 'field_addr (get-int (int (+ rdata (* field 4))))) )
            (if (= field_addr 0)
              (push nil row -1) ;; what to do when the field contains NULL
              (push (keep-type MYSQL_RES field_addr field) row -1)))
    row))) ; not necessary starting v 9.9.5, because push returns the list

;; @syntax (MySQL:fetch-all)
;; @return All rows/fields from the last query.
;; The whole result set from the query is returned at once as a list of row lists.

(define (fetch-all , (all '()))
  (dotimes (x (num-rows)) (push (fetch-row) all -1)))

;; @syntax (MySQL:databases)
;; @return A list of databases.
;; Performs a 'show databases;' query.

(define (databases)
  (query "show databases;")
  (fetch-all))

;; @syntax (MySQL:tables)
;; @return A list of tables in the database.
;; Performs a 'show tables;' query.

(define (tables)
  (query "show tables;")
  (fetch-all))

;; @syntax (MySQL:fields <str-table>)
;; @param <str-table> The name of the table.
;; @return A list of field description lists.
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
  (if MYSQL (get-string (mysql_error MYSQL)))) 

;; @syntax (MySQL:affected-rows)
;; @return Number of affected rows by the last 'MySQL:query' operation.

(define (affected-rows)
  (if MYSQL (mysql_affected_rows MYSQL)))

;; @syntax (MySQL:inserted-id)
;; @return Last insert ID from an auto increment field.

(define (inserted-id)
  (if MYSQL (mysql_insert_id MYSQL)))

;; @syntax (MySQL:escape <str-sql>)
;; @return escaped string
;; This function will escape special characters in <str-sql>, so that it 
;; is safe to place it in a MySQL query.
(define (escape value , safe-value) 
  (set 'safe-value (dup " " (+ 1 (* 2 (length value)))))
  (MySQL:mysql_real_escape_string MySQL:MYSQL safe-value value (length value)) 
  safe-value)

;; @syntax (MySQL:close-db)
;; @return Always 'true'.
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
  (unless (MySQL:connect 0 "" 0 "test")
    (println "Could not connect to MySQL")
	(exit))
  
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
