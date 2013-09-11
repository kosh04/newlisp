;; @module sqlite3.lsp 
;; @description SQLite3 database interface routines
;; @version 1.6 - comments redone for automatic documentation
;; @version 1.7 - a fix getting types when null values are present (thanks Dmitry)
;; @version 1.8 - a fix to make 64-Bit integers work (thanks Dmitry)
;; @version 1.9 - new library detection routine
;; @version 2.0 - added documentation for close
;; @version 2.1 - use default functor for query
;; @author Lutz Mueller, 2004-2008
;;
;; <h2>Module for SQLite3 database bindings</h2>
;; To use this module include the following 'load' statement at the
;; beginning of the program file:
;; <pre>
;; (load "/usr/share/newlisp/modules/sqlite3.lsp")
;; </pre>
;; SQLite version 3.0 introduced a new database format and is incompatible
;; whith the previous 2.1 to 2.8 format. Old SQLite 2.x based databases can
;; be converted  using the old and new sqlite client application:
;;
;;    sqlite OLD.DB .dump | sqlite3 NEW.DB
;;
;; While in sqlite 2.8 all returned fields where of string type, SQLite3
;; returns, text, integer or float. Blobs are returned as text and NULLs
;; are returned as nil.
;;
;; See also the documentation at @link http://sqlite.org sqlite.org
;;
;; <h2>Requirements:</h2> 
;; One of the libraries sqlite3.dll for Win32 or libsqlite3.so for UNIX like
;; operating systems is required from http://www.sqlite.org.
;;
;; SQLite is an <in-process> database. The library contains the whole database
;; system. An extra database server is not required. SQLite also has limited
;; mutiuser capabilities for accessing a common database from several programs
;; at the same time. See the documentaion at @link http://sqlite.org sqlite.org 
;; for details.
;;
;; The following is a short example how to use SQLite3:
;;
;; @example
;; (sql3:open "MYDB")      ; opens/creates a database returns a handle (ignore)
;;                         ; or 'nil' on failure
;;
;; (sql3:sql "select * from mytable;")	; make a SQL query, return result
;;
;; (sql3:error)            ; return error text
;;
;; (sql3:close)            ; close the database

;; Function calls returning 'nil' signal that an error has occurred. The
;; function 'sql3:error' can then be used to get details about the error
;; as a text string.
;;
;; At the bottom of the source file 'sqlite3.lsp' a test routine called
;; 'test-sqlite3' can be found to test for correct installation of SQLite.

(context 'sql3)

; set library to path-name of the library on your platform OS
;
(set 'files (list
	"/usr/local/lib/libsqlite3.so" ; Linyx and BSDs
	"/usr/lib/libsqlite3.0.dylib" ; Mac OSX Darwin
	"/usr/local/lib/libsqlite3.so" ; Solaris
	(string (env "PROGRAMFILES") "/sqlite3/sqlite3.dll") ; Win32/MinGW
))


(set 'library (files (or 
				(find true (map file? files)) 
				(begin (println "cannot find sqlite3 library") (exit)))))

(import library "sqlite3_open" "cdecl")
(import library "sqlite3_close" "cdecl")
(import library "sqlite3_prepare" "cdecl")
(import library "sqlite3_step" "cdecl")
(import library "sqlite3_column_count" "cdecl")
(import library "sqlite3_column_name" "cdecl")
(import library "sqlite3_column_type" "cdecl")
(import library "sqlite3_column_int" "cdecl")
(import library "sqlite3_column_double" "cdecl")
(import library "sqlite3_column_text" "cdecl")
(import library "sqlite3_column_blob" "cdecl")
(import library "sqlite3_column_bytes" "cdecl")
(import library "sqlite3_finalize" "cdecl")
(import library "sqlite3_get_table" "cdecl")
(import library "sqlite3_last_insert_rowid" "cdecl")
(import library "sqlite3_changes" "cdecl")
(import library "sqlite3_busy_timeout" "cdecl")
(import library "sqlite3_errmsg" "cdecl")


; gloablly used vars and constants

(define db nil)                  ; database handle
(define dbp "\000\000\000\000")  ; ptr to database handle
(define error-message nil)        ; error message
(define col-names '())           ; list of column headers
(define col-types '())           ; list of column types
(define pstm "\000\000\000\000") ; ptr to compiled sql

(constant 'SQLITE_OK 0)
(constant 'SQLITE_ROW 100)
(constant 'SQLITE_DONE 101)

(constant 'SQLITE_TYPES '(
	0 
	SQLITE_INTEGER 
	SQLITE_FLOAT 
	SQLITE_TEXT 
	SQLITE_BLOB 
	SQLITE_NULL))

;; @syntax (sql3:open <str-db-name>)
;; @param <str-db-name> The name of the database.
;; @return A database handle (discard), or 'nil' on failure.
;; Opens or creates a database. If the database does exist it gets opened, 
;; else a new database with the name given is created.
;; If trying to open a database that already has been opened 'nil' is returned
;; and an error text can be retrieved using 'sql3:error'.

(define (sql3:open db-name)
  ; only open if not alrady done
  (if (not db)
    (begin
      (set 'result (sqlite3_open db-name dbp))
      (if (!= result SQLITE_OK)
        (set 'db nil)
        (set 'db (get-int dbp))))
    (begin
      (set 'error-message "A database is already open")
      nil))
)

;; @syntax (sql3:close)
;; @return Returns 'true' on success;
;; Closes the currently open database.

(define (sql3:close) 		;;  overwrite the close in MAIN
	(if db (begin
		(sqlite3_close db)
		(set 'db nil)
		true)))

;; @syntax (sql3:sql <str-sql>)
;; @param <str-sql> The SQL statement.
;;
;; Executes the SQL statement in <str-sql>. For 'select' statements an array 
;; of the result set is returned or '()' for the empty set. For other statements
;; 'true' is returned for a  successful outcome. On failure 'nil' is returened 
;; and 'sql3:error' can be used to retrieve the error text.

(define (sql sql-str)
	(set 'result nil 'done nil 'error-message nil)
	(set 'sqarray '());
	(set 'col-names '());
	(set 'col-types '());

	; set up parameters for sqlite3_prepare() call
	(set 'ppstm "\000\000\000\000") ; pointer to statement ptr
	(set 'pptail "\000\000\000\000") ; pointer to statement tail

	; compile the sql statment
	(if db (set 'result (sqlite3_prepare db sql-str -1 ppstm pptail)))

	; set up parameters for sqlite3_step() call
	(set 'pstm (get-int ppstm))

	; execute the compiled statement
	(if (= result SQLITE_OK) 
		(while (not done) 
			;; execute statement until done/101 or 
			(set 'result (sqlite3_step pstm))
			(set 'num-cols (sqlite3_column_count pstm))
			(if (empty? col-names) (set 'col-names (get-names pstm num-cols)))
			(set 'col-types (get-types pstm num-cols))
			(if (= result SQLITE_ROW)
				(push (get-values pstm num-cols) sqarray -1)
				(set 'done true) ;; received done/101 or error
			))
		
                )
	; if done/101 finalize
	(if (= result SQLITE_DONE) 
		(begin
			(set 'result (sqlite3_finalize pstm))
			; for 'select' statements return the array else 'true'
			(if (> num-cols 0) sqarray true))
		(if (= result 0) true (set-error))))


(define (get-values pstm cols) 
	(set 'row '())
	(dotimes (idx cols)
		(set 'i (int idx)) ; all loop vars are float
		(case (nth idx col-types idx)
;			(SQLITE_INTEGER 
;				(push (sqlite3_column_int pstm i) row -1))
;			fixed for 64-bit, thanks Dmitry
			(SQLITE_INTEGER 
				(set 'pstr (sqlite3_column_text pstm i)) 
				(if (= pstr 0) 
					(push nil row -1) 
					(push (int (get-string pstr)) row -1))) 
			(SQLITE_FLOAT 
				(set 'pstr (sqlite3_column_text pstm i))
				(if (= pstr 0)
					(push nil row -1)
					(push (float (get-string pstr)) row -1)))
			(SQLITE_TEXT 
				(set 'pstr (sqlite3_column_text pstm i))
				(if (= pstr 0)
					(push "" row -1)
					(push (get-string pstr) row -1)))
			(SQLITE_BLOB 
				(set 'pstr (sqlite3_column_blob pstm i))
				(set 'len (sqlite3_column_bytes pstm i))
				(set 'buff (dup "\000" len))
				(if (= pstr 0)
					(push "" row -1)
					(begin
						(cpymem pstr buff len)
						(push buff row -1))))
			(SQLITE_NULL 
				(push nil row -1))))
	row)

(define (get-names pstm cols) 
	(set 'row '())
	(dotimes (idx cols)
		(set 'i (int idx)) ; all loop vars are float
		(set 'ps (sqlite3_column_name pstm i))
		(if (= ps 0)	       ;; check for null pointer to result
			(push "" row -1)
			(push (get-string ps) row -1)))
	row)

(define (get-types pstm cols)
	(set 'row '())
	(dotimes (idx cols)
		(set 'i (int idx)) ; all loop vars are float
		(push (nth (sqlite3_column_type pstm i) SQLITE_TYPES) row -1))
	row)

(define sql3:sql3 sql)

;; @syntax (sql3:rowid)
;; @return The last row id from last 'insert'.
;; Gets the id of the last row inserted.

(define (rowid)
	(if db (sqlite3_last_insert_rowid db)))

;; @syntax (sql3:tables)
;; @return A list of tables in the database.

(define (tables)
	(if db (begin
		(set 'lst (sql "select tbl_name from sqlite_master")) ))
		(if lst (set 'lst (first (transpose lst)))))


;; @syntax (sql3:columns <str-tabel-name>)
;; @return A list of column names for a table.

(define (columns aTable)
        (if (list? (sql (append "select * from " aTable " where 0;")))
                col-names))


;; @syntax (sql3:changes)
;; @return The Number of rows changed/affected by the last SQL statement.

(define (changes)
	(if db (sqlite3_changes db)))



;; @syntax (sql3:timeout <num-milli-seconds>)
;; @return 'true' on success or 'nil' on failure.
;; Sets busy timeout in milliseconds.

(define (timeout ms)
	(if db (zero? (sqlite3_busy_timeout db (int ms)))))



;; @syntax (sql3:error)
;; @return The error text of the last error occured in 'sql3:sql'.

(define (error) error-message)
	
(define (set-error)
	(set 'result (sqlite3_errmsg db))
	(if (= result 0) 
		(set 'error-message nil)
		(set 'error-message (get-string result))
		nil
		)
)


(context 'MAIN)

; -------------------------------------------------------------------------
;
; test the database routines
;
; if there is an old "TEST" db from an earlier sqlite 2.8 delete it first
;
(define (test-sqlite3)
	(if (sql3:open "TEST") 
		(println "database opened/created,  ... Ok")
		(println "problem opening/creating database"))

	(if (sql3:sql "create table fruits (name CHAR(20), qty INT(3), price FLOAT(10), blobtext BLOB);")
		(println "created table fruits,  ... Ok")
		(println "problem creating table fruits"))

	(if (sql3:sql "insert into fruits values ('apples', 11, 1.234, X'41424300010101');")
		(println "inserted, last row id: " (sql3:rowid) ",  ... Ok")
		(println "problem inserting row"))

	(if (sql3:sql "insert into fruits values ('oranges', 22, 2.345, X'42434400020202');")
		(println "inserted, last row id: " (sql3:rowid) ",  ... Ok")
		(println "problem inserting row"))

	(if (sql3:sql "insert into fruits values ('bananas', 33, 3.456, X'44454600030303');")
		(println "inserted, last row id: " (sql3:rowid) ",  ... Ok")
		(println "problem inserting row"))

	(if (sql3:sql 
		"insert into fruits values ('grapes', 123456789012345678, 7.89, X'47484900040404');")
		(println "inserted, last row id: " (sql3:rowid) ",  ... Ok")
		(println "problem inserting row"))

	(set 'sqarray (sql3:sql "select * from fruits;"))

	(if sqarray
		(begin
			(println "selected rows: ") 
			(map println sqarray)
			(println "column names: ")
			(map println sql3:col-names)
			(println "... Ok"))
		(println "problem with select"))

	(if (sql3:sql "delete from fruits where 1;")
		(println "deleted, rows affected: " (sql3:changes) ",  ... Ok")
		(println "problem deleting rows"))

	(if (list? (sql3:tables) )
		(println "tables: " (sql3:tables) ", ... Ok")
		(println "problem in sql3:tables"))

	(if (list? (sql3:columns "fruits") )
		(println "columns: " (sql3:columns "fruits") ", ... Ok")
		(println "problem in sql3:columns"))
	

	(if (sql3 "drop table fruits;")
		(println "table fruits dropped,  ... Ok")
		(println "problem dropping table fruits"))


	(sql3:close)
)


; eof ;
