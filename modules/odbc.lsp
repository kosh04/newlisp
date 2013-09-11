;; @module odbc.lsp 
;; @description ODBC database interface
;; @version 1.7 - comments redone for automatic documentation
;; @version 1.8 - doc changes
;; @author Lutz Mueller, 2003-2010
;;
;; <h2>OCBC Interface functions</h2>
;; This module has only been tested on Win32 but should work on UNIX too
;; with few modifications. At the beginning of the program file include
;; a 'load' statement for the module:
;; <pre>
;; (load "c:/Program Files/newlisp/modules/odbc.lsp")
;; ; or shorter
;; (module "odbc.lsp")
;; </pre>
;; Some of the code assumes Intel (low -> high) little-endian byte order.
;;
;; See the end of file for a test function 'test-odbc', which demonstrates the 
;; usage of the module and can be used to test a correct ODBC installation and 
;; data source setup.
;; <h2>Requirements</h2>
;; On Win32 platforms required 'odbc32.dll' is part of the OS's installations. 
;; There is no UNIX function import tested or adapted for this ODBC module.
;; <h2>Function overview</h2>
;; <pre>
;;  (ODBC:connect data-source-name-str user-name-str password-str) ; connect to a data source
;;  (ODBC:query sql-str)          ; perform a SQL statement
;;  (ODBC:num-cols)               ; number of columns in a query result set from 'select'
;;  (ODBC:column-atts col)        ; retrieve columns attributes
;;  (ODBC:fetch-row)              ; fetch a row of data after a sql query with 'select'
;;  (ODBC:affected-rows)          ; number of rows affected by a sql query: 'delete', 'update' etc.
;;  (ODBC:tables)                 ; return a list of tables in the current database
;;  (ODBC:columns table-name)     ; return an array of column attributes in table-name
;;  (ODBC:close-db)               ; close database connection
;; </pre>

(context 'ODBC)

; ----------------- import functions from DLL -------------------


; set to the appropiate library on Unix or Win32
(define ODBC-library "odbc32.dll")

; Constants used, make sure these constants are Ok on your Operating System or Platform.
; Note, that (define var value) is the same as as saying (set 'var value), it is here more
; of a visual distinction, documenting that values are constants and shouldn't be changed.
; Most of these are defned in sql.h, sqltypes.h and sqlext.h of your platform.
; The following definitions come from c:\Borland\BCC\Include

(define SQL_HANDLE_ENV          1)
(define SQL_HANDLE_DBC          2)
(define SQL_HANDLE_STMT         3)
(define SQL_HANDLE_DESC         4)

(define SQL_NULL_HANDLE         0)

(define SQL_SUCCESS             0)
(define SQL_SUCCESS_WITH_INFO   1)

(define SQL_OV_ODBC3            3)
(define SQL_ATTR_ODBC_VERSION	200)

(define SQL_LOGIN_TIMEOUT     103)

(define SQL_NTS                -3)

(define SQL_CHAR                1)
(define SQL_C_CHAR       SQL_CHAR)


; Import functions
; there are many more, which are not used here, goto microsoft.com and unixodbc.org for
; more information on ODBC SQLxxx API


(set 'funcs '(
	"SQLAllocHandle"
	"SQLSetEnvAttr"
	"SQLFreeHandle"
	"SQLSetConnectAttr"
	"SQLConnect"
	"SQLDisconnect"
	"SQLGetDiagRec"
	"SQLExecDirect"
	"SQLNumResultCols"
	"SQLRowCount"
	"SQLBindCol"
	"SQLFetch"
	"SQLDescribeCol"
	"SQLTables"
	"SQLColumns"))

(dolist (fun funcs)
	(import ODBC-library fun))

; ------------------------------- reserve space for global pointers ----------------------------

(set 'ptr-odbc-env "    ")     ; pointer to environment handle
(set 'ptr-odbc-conn "    ")    ; pointer to connection handle
(set 'ptr-result-cols "    ")  ; pointer to number of columns in result
(set 'ptr-odbc-stmt "    ")    ; pointer to handle for sql statement
(set 'ptr-result-rows "    ")  ; pointer to number of affected rows from sql statement

(set 'odbc-stmt nil)           ; statement handle
(set 'odbc-conn nil)           ; connection handle
(set 'result-cols 0)           ; contains the number of rows resulting from a 'select' qery

; -------------------------------------- AUXILIARY ROUTINES ------------------------------------

; check result code

(define (is-error-result)
	;result is 16bit, disregard upper 16 bits
	(set 'odbc-result (& 0xFFFF odbc-result))
	(and (!= odbc-result SQL_SUCCESS) (!= odbc-result SQL_SUCCESS_WITH_INFO)))

; initialize and make connection

(define (init)
	(and
		; get environment handle
		(set 'odbc-result (SQLAllocHandle SQL_HANDLE_ENV SQL_NULL_HANDLE ptr-odbc-env))

		(if (is-error-result)
			(begin
				(println "Error allocating env handle")
				nil) true)

		(set 'odbc-env (get-int ptr-odbc-env))

		; register version
		(set 'odbc-result (SQLSetEnvAttr odbc-env SQL_ATTR_ODBC_VERSION SQL_OV_ODBC3 0))

		(if (is-error-result)
			(begin
				(println "Error setting ODBC environment")
				(SQLFreeHandle SQL_HANDLE_ENV odbc-env)
				nil) true))
	)

; get diagnostic record 
;
; retrieve error info after last failed ODBC request
;
; type is one of the following:
;
; SQL_HANDLE_ENV, SQL_HANDLE_DBC, SQL_HANDLE_STMT, SQL_HANDLE_DESC 
;

(define (error type)
	(set 'diag-status "          ")
	(set 'diag-err  "    ")
	(set 'diag-mlen "    ")
	(set 'diag-message "                                                                ")
	(SQLGetDiagRec type odbc-conn 1 diag-status diag-err diag-message 64 diag-mlen)
	(string diag-message " " diag-status (get-int diag-err)))

; bind all columns to string output 
;
; before fetching rows string variables are configured with sufficient long string buffers
; for the 'fetch' statement.
;

(set 'vars '(var0  var1  var2  var3  var4  var5  var6  var7  var8  var9 
             var10 var11 var12 var13 var14 var15 var16 var17 var18 var19 
             var20 var21 var22 var23 var24 var25 var26 var27 var28 var29
             var30 var32 var32 var33 var34 var35 var36 var37 var38 var39
             var40 var41 var42 var43 var44 var45 var46 var47 var48 var49
             var50 var51 var52 var53 var54 var55 var56 var57 var58 var59
             var60 var51 var62 var63 var64))


(define (bind-columns)
	(set 'ptr-result-err "    ")
	(for (v 1 result-cols)
		(set 'w (+ (last (column-atts v)) 1))
		(set (nth v vars) (format (string "%" w "s") ""))
		(SQLBindCol odbc-stmt (int v) SQL_C_CHAR (eval (nth v vars)) w ptr-result-err))
	
	true)


;====================================  USER ROUTINES ========================================


;; @syntax (ODBC:connect <str-data-source> <str-user> <str-password>)
;; @param <str-data-source> The ODBC dara source.
;; @param <str-user> The user name.
;; @param <str-password> The password of the user.
;; @return 'true' on success, 'nil' on failure.
;; Connect to a data-source with a user name and password.
;; The data-source name must be configured first via ODBC
;; administrative tools, i.e. a control applet on Win32.
;;
;; @example 
;; (ODBC:connect "mydatabase" "johndoe" "secret")

(define (ODBC:connect data-source user password)

	(and
		(init)

		; allocate connection handle
		(set 'odbc-result (SQLAllocHandle SQL_HANDLE_DBC odbc-env ptr-odbc-conn))

		(if (is-error-result)
			(begin
				(println "Error allocating conn handle")
				(SQLFreeHandle SQL_HANDLE_ENV odbc-env)
				nil) true)

		(set 'odbc-conn (get-int ptr-odbc-conn))

		; set timeout for connection
		(SQLSetConnectAttr odbc-conn SQL_LOGIN_TIMEOUT 5 0)

		; connect to a data source
		(set 'odbc-result (SQLConnect odbc-conn data-source SQL_NTS
                                                      user SQL_NTS
                                                      password SQL_NTS))

		(if (is-error-result)
			(begin
				(println "Could not connect")
				(SQLFreeHandle SQL_HANDLE_DBC odbc-conn)
				(SQLFreeHandle SQL_HANDLE_ENV odbc-env)
				nil) true))
	)


;; @syntax (ODBC:query <str-sql>)
;; @param <str-sql> The SQL statement string.
;; @return 'true' on success, 'nil' on failure.
;; Send and SQL string for database manipulation
;;
;; @example
;; (query "select * from someTable")
;; (query "delete from addresses")
;; (query "insert into fruits values ('apples', 11)")

(define (ODBC:query sql-string)
	(and 
		; is stmt handle exists free it
		(if odbc-stmt (begin 
			(SQLFreeHandle SQL_HANDLE_STMT odbc-stmt)
			(set 'odbc-stmt nil)
			true)	true)

		; allocate statement handle
		(set 'odbc-result (SQLAllocHandle SQL_HANDLE_STMT odbc-conn ptr-odbc-stmt))

		(if (is-error-result)
			(begin
				(println "could not allocate statement handle")
				nil)
			(set 'odbc-stmt (get-int ptr-odbc-stmt)))

		; do the query
		(set 'odbc-result (SQLExecDirect odbc-stmt sql-string SQL_NTS))
		(if (is-error-result)
			(begin
				(println "query failed")
				nil)
			true)

		; find number of columns in result set
		(SQLNumResultCols odbc-stmt ptr-result-cols)
		(set 'result-cols (& 0xFFFF (get-int ptr-result-cols)))

		; bind colums to string vars for fetching
		(if (not (= result-cols 0)) (bind-columns) true)
		true
		)
		
	)


;; @syntax (ODBC:num-cols)
;; @return Number of columns in the result set.

(define (num-cols) result-cols)


;; @syntax (ODBC:columns-atts <num-col>)
;; @param <num-col> The number of the column, starting witth 1 for the first.
;; @return A list of attributes for a column in a result set.
;; Returns a list with the columname SQL, data type number and required column size
;; when displaying in a string. For the data type number and SQL data type see
;; the file 'sql.h' on your platform OS, i.e. 'SQL_VARCHAR', 'SQL_INTEGER' etc.
;;
;; before using 'ODBC:column-atts' a query has to be performed.
;;
;; @example
;; (ODBC:column-atts 1)  => ("name" 12 20)

;; The first column has the header '"name"' with data type 'SQL_VARCHAR' (12)
;; and a maximum display width of 20 characters.

(define (column-atts col)
	(set 'col-name-out "                                ")
	(set 'ptr-name-len "    ")
	(set 'ptr-data-type "    ")
	(set 'ptr-col-size "    ")
	(set 'ptr-dec-dig "    ")
	(set 'ptr-nullable "    ")

	(set 'odbc-result (& 0xFFFF (SQLDescribeCol odbc-stmt (int col)
                                                col-name-out 32
                                                ptr-name-len
                                                ptr-data-type
                                                ptr-col-size
                                                ptr-dec-dig
                                                ptr-nullable)))
	(list col-name-out (& 0xFFFF (get-int ptr-data-type)) (get-int ptr-col-size)))



;; @syntax (ODBC:fetch-row)
;; @return A list of items of a result set row.
;; Fetches a row of data after a previously executed 'ODBC:query'. Each data is formatted as
;; a string, and can be converted using newLISP conversion functions 
;; like: 'int', 'float' or 'string'.
;;
;; If data types are unknown then 'ODBC:column-atts' can be used to retrieve the data type
;; number.
;;
;; @example
;; (ODBC:fetch-row) => ("apples" "11")

(define (fetch-row , row)
	(bind-columns)
	(set 'odbc-result (& 0xFFFF (SQLFetch odbc-stmt)))
	(if (is-error-result) 
		nil
		(begin
			(for (x result-cols 1) (push (eval (nth x vars)) row))
			row))) ; not necessary starting 9.9.5 because push returns the list


;; @syntax (ODBC:affected-rows)
;; @return Number of rows affected by the last SQL statement.
;; Returns the number of rows affected by an 'insert', 'update' or 'delete', 'ODBX:query'
;; operation. After a 'select' operation the number -1 will be returned.

(define (affected-rows)	
	(set 'odbc-result (& 0xFFFF (SQLRowCount odbc-stmt ptr-result-rows)))
	(if (is-error-result) 0	(get-int ptr-result-rows)))


;; @syntax (ODBC:tables)
;; @return A list of tables in the current database connection.

(define (tables)
    (if (and
        ; is stmt handle exists free it
        (if odbc-stmt (begin
            (SQLFreeHandle SQL_HANDLE_STMT odbc-stmt)
            (set 'odbc-stmt nil)
            true)   true)

        ; allocate statement handle
        (set 'odbc-result (SQLAllocHandle SQL_HANDLE_STMT odbc-conn ptr-odbc-stmt))
        (if (is-error-result)
            (begin
                (println "could not allocate statement handle")
                nil)
            (set 'odbc-stmt (get-int ptr-odbc-stmt)))

        ; do the query
        (set 'odbc-result (SQLTables odbc-stmt 0 SQL_NTS 0 SQL_NTS "%" SQL_NTS 0 SQL_NTS))
        (if (is-error-result)
            (begin
                (println "query failed")
                nil)
            true)

        ;; find number of columns in result set
        (SQLNumResultCols odbc-stmt ptr-result-cols)
        (set 'result-cols (& 0xFFFF (get-int ptr-result-cols)))

        ;; bind colums to string vars for fetching
        (if (not (= result-cols 0)) (bind-columns) true)

        (begin
           (set 'names nil)
           (while (set 'row (ODBC:fetch-row))
               (push (nth 2 row) names -1))
           true)
        ) names)
    )

;; @syntax (ODBC:columns <str-table-name>)
;; @param <str-table-name> The name of the table.
;; @return A list of list of columns and their attributes.

(define (ODBC:columns table)
    (if (and
        ; is stmt handle exists free it
        (if odbc-stmt (begin
            (SQLFreeHandle SQL_HANDLE_STMT odbc-stmt)
            (set 'odbc-stmt nil)
            true)   true)

        ; allocate statement handle
        (set 'odbc-result (SQLAllocHandle SQL_HANDLE_STMT odbc-conn ptr-odbc-stmt))

        (if (is-error-result)
            (begin
                (println "could not allocate statement handle")
                nil)
            (set 'odbc-stmt (get-int ptr-odbc-stmt)))

        ; do the query
        (set 'odbc-result (SQLColumns odbc-stmt 0 SQL_NTS 0 SQL_NTS
                          table SQL_NTS 0 SQL_NTS))
        (if (is-error-result)
            (begin
                (println "query failed")
                nil)
            true)

        ; find number of columns in result set
        (SQLNumResultCols odbc-stmt ptr-result-cols)
        (set 'result-cols (& 0xFFFF (get-int ptr-result-cols)))

        ; bind colums to string vars for fetching
        (if (not (= result-cols 0)) (bind-columns) true)

        (begin
           (set 'names nil)
           (while (set 'col (ODBC:fetch-row))
               (set 'attr (list (nth 3 col) (nth 5 col) (nth 6 col) (nth 8 col)))
               (push attr names -1))
           true)
        ) names)
    )


;; @syntax (ODBC:close-db)
;; @return 'true' on success, 'nil' on failure.
;; Closes a database connection.

(define (close-db)
	(if odbc-stmt (SQLFreeHandle SQL_HANDLE_STMT odbc-stmt))
	(set 'odbc-stmt nil)
	(if odbc-conn (begin
		(SQLDisconnect odbc-conn)
		(SQLFreeHandle SQL_HANDLE_DBC odbc-conn)
		(set 'odbc-conn nil)))
	true)
    

(context 'MAIN)
;=================================== test =================================================
;
; Note: before performing this test a database with name 'test'
; and data source name 'test' should be created. The data base
; should contain a table described by the following SQL statement:
;
;      create table fruits (name CHAR(20), qty INT(3))
;
; For this configure an Access database: 'test-db' with table 'fruits' 
; and a text field 'name' width 20 and field 'qty' as type integer. 
; Make the 'User Data Source' connection with the ODBC control applet 
; in control-panel/administrative-tools for the MS Access *.mdb driver
; and pick as a data source name and database location the test-db.mdb i
; created. 
;
; On some systems the table can also be created with an SQL statement
;     (ODBC:query "create ....")
; On MS-Acces this will not work and the table has to be created
; manually.
;
; A sample of test-db.mdb can be found at: 
;     http://newlisp.org/downloads/Other/
;
; example:
;          (test-odbc)
;



(define (test-odbc)

	; Note, on MS-Access must create table fruits manually first
	; else you could do:
	;   (ODBC:query "create table fruits (name CHAR(20), qty INT(3))")
	; for "aUser" and "secret" you may just put empty strings ""
	; i.e. (ODBC:connect "test" "" "")
	; when on Windows on the same machine

	(if (not (ODBC:connect "test-db" "" "")) (exit))

	(println "connected ...")

	(ODBC:query "insert into fruits values ('apples', 11)")
	(ODBC:query "insert into fruits values ('oranges', 22)")
	(ODBC:query "insert into fruits values ('bananas', 33)")

	(println "inserted 3 records")

	(ODBC:query "select * from fruits")

	(println "performed a query")

	(println (ODBC:num-cols) " columns in result set")
	(println "fetching rows ...")
	(while (set 'row (ODBC:fetch-row)) 
		(set 'row (map trim row))
		(println row))
	(println)


	(ODBC:query "delete from fruits")
	(println "rows deleted: " (ODBC:affected-rows))
	
	(println "\nclosing database")
	(ODBC:close-db)
	)



; eof ;
