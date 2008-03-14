;; @module mysql.lsp
;; @description MySQL v.4 interface
;; @version v2.31 - addition for mysql_escape_string (Jeff)
;; @author Lutz Mueller 2003, Gordon Fischer 2005, Jeff 2007
;;
;; newLISP connection to the MySQL v 4.1 database
;;
;; Current tested MySQL version is 4.1.14
;;
;; See also @link http://www.mysql.com www.mysql.com
;;
;; This file is tested with MySQL 4.0/4.1  and will not work on
;; previous versions 3.22 and 3.23 which need different offsets
;; into the MYSQL result data structure.
;;
;; <h3>Implemented functions</h3>
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
;;     MySQL:escape .............. escapes SQL input string using mysql_escape_string
;;     MySQL:error ............... get error message
;;     MySQL:close-db ............ close database connection
;; </pre>
;;
;; For details on usage see the documentation for the mysql5.lsp module.

(context 'MySQL)

(set 'libmysqlclient "/usr/lib/libmysqlclient.so.14")

(import libmysqlclient "mysql_init")
(import libmysqlclient "mysql_real_connect")
(import libmysqlclient "mysql_get_host_info")
(import libmysqlclient "mysql_escape_string")
(import libmysqlclient "mysql_query")
(import libmysqlclient "mysql_real_query")
(import libmysqlclient "mysql_store_result")
(import libmysqlclient "mysql_free_result")
(import libmysqlclient "mysql_data_seek")
(import libmysqlclient "mysql_fetch_row")
(import libmysqlclient "mysql_close")
(import libmysqlclient "mysql_fetch_field_direct")
(import libmysqlclient "mysql_insert_id")

; initialize database access, get an access handle MYSQL
;
(define (init)
  (set 'MYSQL (mysql_init 0))
  (if (= MYSQL 0) (set 'MYSQL nil))
  (not (= MYSQL nil)))

; connect to a specific database
;
(define (connect host user passw database)
  (not(= (mysql_real_connect MYSQL host user passw database 0 0 0) 0)))

; send a SQL string for evaluation
;
(define (query sql)
  (if MYSQL_RES (mysql_free_result MYSQL_RES))
  (set 'result  (= (mysql_query MYSQL sql) 0))
  (set 'MYSQL_RES (mysql_store_result MYSQL)) 
  (if (= MYSQL_RES 0) (set 'MYSQL_RES nil))
  (if (and result (find "insert into" sql 1)) (set 'result (inserted-id)))
  result)

; number of rows from last query
;
(define (num-rows)
  (if MYSQL_RES (get-int MYSQL_RES)))

; number of columns from last query
;
(define (num-fields)
  (if MYSQL_RES (get-int (int (+ MYSQL_RES 60)))))


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
      ; else
      (get-string field_addr)
  )
)

; fetch a row from a query, subsequent calls go trough the table
; until the bottom is reached
;
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
    row)))

; return all rows/columns from the last query
;
(define (fetch-all)
  (set 'all '())
  (dotimes (x (num-rows)) (push (fetch-row) all))
  (reverse all))

; get a list of databases
;
(define (databases)
  (query "show databases;")
  (fetch-all))

; get a list of tables
;
(define (tables)
  (query "show tables;")
  (fetch-all))

; get a list of fields
;
(define (fields table)
  (query (append "show fields from " table ";"))
  (fetch-all))
  

; position to row in result set
;
(define (data-seek offset)
  (if MYSQL_RES
    (begin
    (mysql_data_seek MYSQL_RES offset)
    true)))

; Escapes input value using mysql_escape_string
(define (escape value , safe-value) 
  (set 'safe-value (dup " " (+ 1 (length value)))) 
  (MySQL:mysql_escape_string safe-value value (length value)) 
  safe-value)

; text info about error
;
(define (error)
  (if MYSQL (get-string (+ MYSQL 60))))

; number of affected rows by last MySQL:query operation
;
(define (affected-rows)
  (if MYSQL
    (get-int (int (+ MYSQL 368)))))

; last insert_id from auto increment field
;
(define (inserted-id)
  (if MYSQL (mysql_insert_id MYSQL)))

; close database access
;
(define (close-db)
  (if MYSQL_RES (mysql_free_result MYSQL_RES))
  (if MYSQL (mysql_close MYSQL))
  (set 'MYSQL nil)
  (set 'MYSQL_RES nil)
  true)

(context 'MAIN)

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  
  (MySQL:query "create table fruits (name CHAR(20), 
                                     qty INT(3),
                                     num INT(4) AUTO_INCREMENT UNIQUE);")
  
  (MySQL:query "insert into fruits values ('apples', 11, null);")
  (println "inserted-id: " (MySQL:inserted-id))
  (MySQL:query "insert into fruits values ('oranges', 22, null);")
  (println "inserted-id: " (MySQL:inserted-id))
  (MySQL:query "insert into fruits values ('bananas', 33, null);")
  (println "inserted-id: " (MySQL:inserted-id))
  
  (println "\n" (MySQL:affected-rows) " affected rows in query")

  (println "inserted into fruits:")
  (MySQL:query "select * from fruits;")
  (dotimes (x (MySQL:num-rows)) (println (MySQL:fetch-row)))

  (println "rows = " (MySQL:num-rows) " fields = " (MySQL:num-fields))
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
