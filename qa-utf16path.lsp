;; qa-utf16path - check WIN_32 file and directory routines on 
;; UTF-16 encoded paths

(if (not (= ostype "Win32"))
		(println "This test is lengthy and normally only used on" 
				" the Win32 UTF8 enabled versions.")
)

(set 'Yuki "\230\162\182\230\181\166\231\148\177\232\168\152")
(set 'InvalidUTF8 "\xC0\xC1")

(set 'UnicodeStr Yuki)

(set 'h 1) ; pre allocate mem so changes should be minimal
(set 'sRand 
	(join (map char (map (curry + 32)
		(rand 
			(- 127 32)
			(+ 10 90);(rand 100))
		)
	)
)))
(set 'buff sRand)


(define-macro (assert any)
	(local (result)
		(if (not (set 'result (eval any)))
			(throw-error (string "Expression failed! " any))
		)
		result
	)
)

(println "Check memory - hit <enter>.")
(read-line)

(println "Step 1")

(println (time 
	(begin 
		(assert (make-dir UnicodeStr))
		(assert (remove-dir UnicodeStr))
	)
	1000
))

(println "Step 2")

(assert (make-dir UnicodeStr))
(println (time (assert (directory? UnicodeStr)) 100000))
(println (time (assert (file? UnicodeStr)) 100000))
(println (time (begin
	(assert (directory))
	(assert (directory "." UnicodeStr))
	(assert (= 1 (length (directory "." UnicodeStr))))
	(assert (real-path UnicodeStr))
	(assert (change-dir UnicodeStr))
	(assert (real-path))
	(assert (directory))
	(assert (change-dir ".."))
) 5000))
(assert (remove-dir UnicodeStr))

(println "Step 3")

		
(println (time
	(begin
		(assert (write-file UnicodeStr sRand))
		(assert (= (read-file UnicodeStr) sRand))
		(assert (append-file UnicodeStr (reverse sRand)))
		(assert (delete-file UnicodeStr))
	)
5000 ))

(println "Step 4")

(assert (write-file UnicodeStr sRand))

(println (time (assert (not (directory? UnicodeStr))) 100000))
(println (time (assert (file? UnicodeStr)) 100000))

(println (time (assert (file-info UnicodeStr)) 100000))

(println "Step 5")

(println (time (begin
	(assert (set 'h (open UnicodeStr "w")))
	(assert (write-buffer h sRand))
	(assert (close h))
	(assert (set 'h (open UnicodeStr "a")))
	(assert (write-buffer h (reverse sRand)))
	(assert (close h))
	(assert (set 'h (open UnicodeStr "r")))
	(assert (read-buffer h buff 1024))
	(assert (close h))
	(assert (rename-file UnicodeStr "foo"))
	(assert (rename-file "foo" UnicodeStr))
	(assert (rename-file UnicodeStr UnicodeStr))
	(assert (not (rename-file UnicodeStr InvalidUTF8)))
	(assert (not (rename-file InvalidUTF8 InvalidUTF8)))
	(assert (not (rename-file InvalidUTF8 UnicodeStr)))
) 10000))

(assert (delete-file UnicodeStr))

(println "Step 6")

(println (time (save UnicodeStr) 10000))

(assert (delete-file UnicodeStr))

