#!/usr/bin/env newlisp

# prodcons.lsp -  Producer/consumer
#
# this program only runs on Linux/UNIX
#
# usage of 'fork', 'wait-pid', 'semaphore' and 'share'

(when (= ostype "Windows")
		(println "this will not run on Windows")
		(exit))

(constant 'wait -1 'sig 1 'release 0)

(define (consumer n)
	(set 'i 0)
	(while (< i n)
		(semaphore cons-sem wait)
		(print (set 'i (share data)) "\r")
		(semaphore prod-sem sig))  
	(exit))
		
(define (producer n)
	(for (i 1 n)
		(semaphore prod-sem wait)
		(print "-> " (share data i) " ->  ")
		(semaphore cons-sem sig))   
	(exit))


(define (run n)
	(set 'data (share))	
	(share data 0)

	(set 'prod-sem (semaphore)) ; get semaphores
	(set 'cons-sem (semaphore))

	(set 'prod-pid (fork (producer n))) ; start threads
	(set 'cons-pid (fork (consumer n)))
	(semaphore prod-sem sig) ; get producer started

	(wait-pid prod-pid) ; wait for threads to finish
	(wait-pid cons-pid) ; 
	(semaphore cons-sem release) ; release semaphores
	(semaphore prod-sem release))

(set 'N (or (int (main-args -1)) 100000))
(set 'duration (time (run N)))

(println (div duration N) " ms per exchange")

(exit)

