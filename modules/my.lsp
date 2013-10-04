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
;; @version 3.43 - add multi-db instance support
;; @author Roy Gu 2013, Lutz Mueller 2003-2010, Gordon Fischer 2005, Jeff Ober 2007
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
;; (set 'link (MySQL:init))       ; initialize
;; (MySQL:connect "192.168.1.10" "auser" "secret" "mydb" 3306 link) ; logon
;; (set 'res (MySQL:query "select ...;" link)) ; SQL query
;; (set 'insertid (MySQL:query "insert ...;" link)) ; SQL query
;;        ...
;; (MySQL:close-db link)

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
    "/usr/local/mysql/lib/libmysqlclient.so" ; Linux, UNIX
    "/usr/local/mysql/lib/libmysqlclient.dylib" ; MacOS X
    "/usr/lib/libmysqlclient.dylib" ; MacOS X
        "/home/work/lib/libmysqlclient.so.18" ; My Ubuntu Pc
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
;; @return mysql connection link id on success, 'nil' on failure.

(define (init)
  (let (dblink (mysql_init 0))
    (if (= dblink 0) nil dblink)))

;; @syntax (MySQL:connect <str-server> <str-userID> <str-password> <str-db> <int-port> <int-dblink>)
;; @param <str-server> The host name or IP address or <tt>0</tt> for localhost.
;; @param <str-userID> The user ID for authentication.
;; @param <str-password> The password for authentication.
;; @param <str-db> The name of the database to connect to.
;; @param <int-port> The port of the database to connect to.
;; @param <int-dblink> The connection link id of the database.
;; @return 'true' for success or 'nil' for failure.
;; Connects to a database on server and authenticates a user ID.
;; '(MySQL:init)' must have been called previously.

(define (connect host user passw database port dblink)
  (not (= (mysql_real_connect dblink host user passw database port 0 0) 0)))

;; @syntax (MySQL:query <str-sql> <int-dblink>)
;; @param <str-sql> A valid SQL query string.
;; @param <int-dblink> The connection link id of the database.
;; @return For 'insert' queries rerturns the inserted ID else 'true' for success or 'nil' for failure.
;; Sends a SQL query string to the database server for evaluation.

(define (query sql dblink)
  (let (result (= (mysql_real_query dblink sql (+ 1 (length sql))) 0) res (mysql_store_result dblink))
    (if (= res 0) (set 'res true))
    (if (and result (find "insert into" sql 1)) (inserted-id dblink) res)))

;; @syntax (MySQL:num-rows <int-mysql_res>)
;; @param <int-mysql_res> The query result of database.
;; @return Number of rows from last query.

(define (num-rows (mysql_res nil))
  (if mysql_res (mysql_num_rows mysql_res)))

;; @syntax (MySQL:num-fields <int-mysql_res>)
;; @param <int-mysql_res> The query result of database.
;; @return Number of columns from last query.

(define (num-fields (mysql_res nil))
  (if mysql_res (mysql_num_fields mysql_res)))


;; format the result based on the field type.

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

;; @syntax (MySQL:fetch-row <int-mysql_res>)
;; @param <int-mysql_res> The query result of database.
;; @return A list of field elements.
;; Fetches a row from a previous SQL 'MySQL:query'  'select' statement.
;; Subsequent calls fetch row by row from the result table until the
;; end of the table is reached.

(define (fetch-row (mysql_res nil))
  (if mysql_res
    (set 'rdata (mysql_fetch_row mysql_res))
    (set 'rdata 0))
  (if (!= rdata 0)
    (begin
      (set 'row '())
      (dotimes (field (num-fields mysql_res))
            (if NEWLISP64
              (set 'field_addr (get-long (int (+ rdata (* field 8)))))
              (set 'field_addr (get-int (int (+ rdata (* field 4))))) )
            (if (= field_addr 0)
              (push nil row -1) ;; what to do when the field contains NULL
              (push (keep-type mysql_res field_addr field) row -1)))
    row))) ; not necessary starting v 9.9.5, because push returns the list

;; @syntax (MySQL:fetch-all <int-mysql_res>)
;; @param <int-mysql_res> The query result of database.
;; @return All rows/fields from the last query.
;; The whole result set from the query is returned at once as a list of row lists.

(define (fetch-all mysql_res, (all '()))
  (dotimes (x (num-rows mysql_res)) (push (fetch-row mysql_res) all -1)))

;; @syntax (MySQL:databases <int-dblink>)
;; @param <int-dblink> The connection link id of the database.
;; @return A list of databases.
;; Performs a 'show databases;' query.

(define (databases dblink)
  (let (res (query "show databases;" dblink))
  (fetch-all res)))

;; @syntax (MySQL:tables <int-dblink>)
;; @param <int-dblink> The connection link id of the database.
;; @return A list of tables in the database.
;; Performs a 'show tables;' query.

(define (tables dblink)
  (let (res (query "show tables;" dblink))
  (fetch-all res)))
  

;; @syntax (MySQL:fields <str-table> <int-dblink>)
;; @param <str-table> The name of the table.
;; @param <int-dblink> The connection link id of the database.
;; @return A list of field description lists.
;; For each field name in the table a list of specifications
;; for that field is returned. The list starts with the name
;; for the field followed by the type size/precision and
;; other optional field descriptions.

(define (fields table dblink)
  (let (res (query (append "show fields from " table ";") dblink))
    (fetch-all res)))
  
;; @syntax (MySQL:data-seek <num-offset> <int-mysql_res>)
;; @param <num-offset> The <tt>0</tt> based offset to position inside the data set.
;; @return Always 'true'. 
;; Positions in the result set at a zero based offset
;; for a subsequent 'MySQL:fetch-row' call. If the offset
;; is out of the allowed range for the result set a subsequent
;; fetch-row will return 'nil'.
 
(define (data-seek offset (mysql_res nil))
  (if mysql_res
    (if big-endian
        (mysql_data_seek mysql_res  0 (int offset))
        (mysql_data_seek mysql_res (int offset) 0)))
  true
)

;; @syntax (MySQL:error <int-dblink>)
;; @param <int-dblink> The connection link id of the database.
;; @return Text info about the last error which occured.

(define (error dblink)
  (if dblink (get-string (mysql_error dblink))))

;; @syntax (MySQL:affected-rows)
;; @return Number of affected rows by the last 'MySQL:query' operation.

(define (affected-rows dblink)
  (if dblink (mysql_affected_rows dblink)))

;; @syntax (MySQL:inserted-id)
;; @return Last insert ID from an auto increment field.

(define (inserted-id dblink)
  (if dblink (mysql_insert_id dblink)))

;; @syntax (MySQL:escape <str-sql>)
;; @return escaped string
;; This function will escape special characters in <str-sql>, so that it 
;; is safe to place it in a MySQL query.
(define (escape value dblink , safe-value) 
  (set 'safe-value (dup " " (+ 1 (* 2 (length value)))))
  (MySQL:mysql_real_escape_string dblink safe-value value (length value)) 
  safe-value)

;; @syntax (MySQL:close-db)
;; @return Always 'true'.
;; Closes database access. For new database acess, both 'MySQL:init' and 
;; 'MySQL:connect' functions have to be called.

(define (close-db dblink)
  (if dblink (mysql_close dblink))
  (set 'dblink nil)
  true)

(context 'MAIN)

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; test data base functions
;

(define (test-mysql)
  (set 'link (MySQL:init))
  (unless (MySQL:connect "10.23.241.39" "root" "root" "test" 7702 link)
    (println "Could not connect to MySQL")
    (exit))
  
  (println "databases:")
  (set 'db-res (MySQL:query "show databases;" link))

  (dotimes (x (MySQL:num-rows db-res)) (println (MySQL:fetch-row db-res)))
  (println)
  
  (MySQL:query "create table fruits (name TEXT(2000), 
                                     qty INT(3),
                                     num INT(4) AUTO_INCREMENT UNIQUE);" link)
  
  (MySQL:query "insert into fruits values ('apples', 11, null);" link)
  (println "inserted-id: " (MySQL:inserted-id link))
  (MySQL:query "insert into fruits values ('oranges', 22, null);" link)
  (println "inserted-id: " (MySQL:inserted-id link))
  (MySQL:query "insert into fruits values ('bananas', 33, null);" link)
  (println "inserted-id: " (MySQL:inserted-id link))

  (println "inserted into fruits:")
  (set 'fruit-res (MySQL:query "select * from fruits;" link))
  (println "\n" (MySQL:affected-rows link) " affected rows in query select")
  (dotimes (x (MySQL:num-rows fruit-res)) (println (MySQL:fetch-row fruit-res)))

  (println "no rows = " (MySQL:num-rows fruit-res) " no fields = " (MySQL:num-fields fruit-res))
  (println "fields = " (MySQL:fields "fruits" link))
  (println)
  
  (println "tables:")
  (set 'tbl-res (MySQL:query "show tables;" link))
  (dotimes (x (MySQL:num-rows tbl-res)) (println (MySQL:fetch-row tbl-res)))
  (println)
  
  (set 'fruit-res (MySQL:query "select * from fruits;" link))
  (MySQL:data-seek 2 fruit-res)

  (println "data-seek to offset 2:")
  (println (MySQL:fetch-row fruit-res))
  (println)
  
  (println "fetch-all:")
  (set 'fruit-res (MySQL:query "select * from fruits;" link))
  (println (MySQL:fetch-all fruit-res))
  
  (MySQL:query "drop table fruits;" link)
  (MySQL:close-db link)
)

; eof

