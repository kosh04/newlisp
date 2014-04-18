;; @module gmp.lsp
;; @description GNU MP Bignum Library interface
;; @version 1.52 updated for new install locations and GMP website
;; @version 1.53 took out redefinition of normal int operations to floats
;; @version 1.6 added library path for OpenBSD and tested for 64-bit newLISP
;; @version 1.7 doc changes
;; @version 1.8 doc changes
;; @version 1.9 fixes for 64-bit in <,>,>=,<=,factor didn't sign extend 32->64
;; @version 2.0 more fixes for 64-bit, allocated space for handles was to small
;; @version 2.1 changed deprecated <tt>name</tt> to <tt>term</tt>
;; @version 2.2 out-comment 64-bit lib on Mac OSX, newLISP is shipped as 32-bit
;; @version 2.2 document built-in big integer arithmetik in newLISP
;; @author Lutz Mueller, 2007-2011
;; <h3>The GNU MP Bignum Library</h3>
;;
;; Since version 10.4.8, newLISP has built-in support for unlimited precision big-integer
;; arithmetik supporting the integer operators 
;; <tt>= ,- , *, /, %, ++, --, &lt;, &lt;= , &gt;, &gt;=, !=</tt>
;; and the functions <tt>abs</tt>, <tt>even?</tt>, <tt>odd</tt>, <tt>sgn</tt>  and <tt>zero?</tt>. 
;; As this module has some functions not available built into newLISP, it is still packaged with
;; the distribution.
;;
;; This modules interfaces to libgmp which can be obtained from @link http://gmplib.org/ http://gmplib.org/ .
;;
;; Source code for libgmp.so (UNIX) or lbgmp.dll (Win32) or libgmp.dylib (Mac OS X) 
;; is available at this site. After installing the library the correct path-name should
;; be added in the <tt>(set 'files ... )</tt> statement around line 130 in this module.
;;
;; When compiling libgmp for Mac OS X on Intel CPUs and for 32-bit newLISP use:
;; <pre>
;;     ./configure CFLAGS="-m32" ABI=32
;;     make
;;     sudo make install
;; </pre>
;; This interface module presets the maximum precision to 1024.
;; The precision can be changed to any other value by changing the 
;; definition of 'MAX_PRECISION' in the source of this module.
;;
;; All arguments to the GMP functions in this module must be given as strings,
;; an error will be thrown otherwise. When starting with a 0 the number is 
;; assumed to be in octal format when starting with 0x in hexadecimal format.
;;
;; This file only imports a few functions from the many available in GNU GMP.
;; See the GMP manual for more functions.
;;
;; Note, that since version 8.9.7 newLISP does all integer arithmetik with
;; 64 bits giving up to 19 digits of precision. For precisions less or equal
;; 19 digits newLISP's built-in 64-bit integer arithmetik is much faster.
;;
;; <h3>Usage</h3>
;; At the beginning of the programfile include a 'load' statement for the module:
;; <pre>
;; (load "/usr/share/newlisp/modules/gmp.lsp")
;; ; or shorter
;; (module "gmp.lsp")
;; </pre>
;; @example
;; (GMP:+ "123456789012345678901234567890" "123456789012345678901234567890") 
;; => "246913578024691357802469135780"

;; <h3>Adding more functions to the library</h3>
;; When adding functions be aware that inside the GMP context
;;  +,-,*,/,=,&lt;,&gt;,&lt;=,&gt;=  are overwritten for multiple precision and the 
;; original operators  would have would have to be prefixed with MAIN when used, 
;; inside the 'gmp.lsp' module.

;; <br/><br/>
;; <center><h2>Integer arithmetik</h2></center>
;; @syntax (GMP:+ <arg1> <arg2>)
;; add two integers in <arg1> and <arg2>
;; @syntax (GMP:- <arg1> <arg2>)
;; subtract <arg2> from <arg1>
;; @syntax (GMP:* <arg1> <arg2>)
;; multiply <arg1> by <arg2>
;; @syntax (GMP:/ <arg1> <arg2>)
;; divide <arg1> by <arg2>, round towards '0' (zero)
;; @syntax (GMP:% <arg1> <arg2>)
;; calc rest of division <arg1>/<arg2>
;; @syntax (GMP:** <arg1> <arg2>)
;; calc power(<arg1>, <arg2>)
;; @syntax (GMP:= <arg1> <arg2>)
;; test for <arg1> equal <arg2>
;; @syntax (GMP:< <arg1> <arg2>)
;; test for <arg1> smaller <arg2>
;; @syntax (GMP:> <arg1> <arg2>)
;; test for <arg1> bigger <arg2>
;; @syntax (GMP:<= <arg1> <arg2>)]
;; test for <arg1> smaller or equal <arg2>
;; @syntax (GMP:>= <arg1> <arg2>)
;; test for <arg1> bigger or equal <arg2>
;;
;; <center><h2>Bit operations</h2></center>
;; @syntax (GMP:& <arg1> <arg2>)
;; bitwise <and> of <arg1>, <arg2>
;; @syntax (GMP:| <arg1> <arg2>)
;; bitwise inclusive <or> of <arg1>, <arg2>
;; @syntax (GMP:^ <arg1> <arg2>)
;; bitwise exclusive <or> of <arg1>, <arg2>
;; @syntax (GMP:~ <arg>)
;; bitwise complement of <arg>
;;
;; <center><h2>Number theory</h2></center>
;; @syntax (GMP:prime? <arg>)
;; check if <arg> is prime
;; @syntax (GMP:next-prime <arg>)
;; calc closes prime greater than <arg>
;; @syntax (GMP:factor <arg>)
;; calc a list of prime factors for <arg>
;; @syntax (GMP:gcd <arg1> <arg2>)
;; greatest common divisor of <arg1> and <arg2>
;; @syntax (GMP:bin <arg1> <arg2>)
;; calc binomial (<arg1> <arg2>)
;; @syntax (GMP:fac <arg>)
;; arg! factorial(<arg>) 
;; @syntax (GMP:fib <arg>)
;; fibonacci(arg)
;;
;; <center><h2>Random numbers</h2></center>
;; @syntax (GMP:seed <arg>)
;; seed the random generator
;; @syntax (GMP:rand <arg>)
;; generate random numbers between 0 and arg - 1
;;

;(constant '+ add '- sub '* mul '/ div)

(if (< (sys-info -2) 10111)
	(constant (global 'term) name))

(context 'GMP)

; convert from int32 to int64 with sign extension
; in both newLISP 32-bit and 64-bit integers are 64-bit
(define (int64 x) (get-int (pack "ld" x)))

; maximum digits, can be set to any value higher if required
; when choosing a different number the functions GMP:fac and GMP:fib
; have to be re-calibrated for their maximum value not to overflow
; the result space
;
(define MAX_PRECISION 1024)

(define gmp-type-error "String expected in GMP module.")
(define gmp-size-error "Argument too big in GMP module")
(define gmp-divzero-error "Division by zero in GMP module")

(set 'files '(
;	"/usr/local/lib/libgmp.dylib" ; Mac OSX for 64-bit newLISP
    "/usr/lib/libgmp.dylib" ;Mac OSX
    "/usr/lib/libgmp.3.dylib" ;Mac OSX
    "/opt/local/lib/libgmp.3.dylib" ;Mac OSX
    "/opt/local/lib/libgmp.dylib" ;Mac OSX
    "/usr/local/lib/libgmp.dylib" ;Mac OSX
    "/usr/local/lib/libgmp.so.8.0" ; OpenBSD 4.6
    "/usr/lib/libgmp.so.3" ; Linux, BSDs
    "/usr/lib64/libgmp.so"
    "/usr/local/lib/libgmp.so" ; Linux, BSDs
    "/WINDOWS/system32/libgmp-3.dll" ; Win32 DLL path on current drive
    "libgmp-3.dll" ; Win32 in path for current directory
))

(set 'library (files (or
		       (find true (map file? files))
		       (throw-error "cannot find GMP library"))))
  
; integer arithmetik    
(import library "__gmpz_init")
(import library "__gmpz_add")
(import library "__gmpz_add_ui")
(import library "__gmpz_sub")
(import library "__gmpz_mul")
(import library "__gmpz_tdiv_q")
(import library "__gmpz_tdiv_r")
(import library "__gmpz_cmp")
(import library "__gmpz_cmp_si")
(import library "__gmpz_set_si")
(import library "__gmpz_divisible_p")
(import library "__gmpz_pow_ui")
(import library "__gmpz_clear")

; bit operators
(import library "__gmpz_and")
(import library "__gmpz_ior")
(import library "__gmpz_xor")
(import library "__gmpz_com")

; number theory
(import library "__gmpz_probab_prime_p")
(import library "__gmpz_nextprime")
(import library "__gmpz_gcd")
(import library "__gmpz_bin_ui")
(import library "__gmpz_fac_ui")
(import library "__gmpz_fib_ui")

; random numbers
(import library "__gmpz_urandomm")
(import library "__gmp_randseed")

; conversion functions
(import library "__gmpz_set_str")
(import library "__gmpz_get_str")

; auxiliary functions
(import library "__gmpz_sizeinbase")
(import library "__gmp_randinit_default")
(import library "__gmp_randseed_ui")

; reserve handles
(define op1 (dup "\000" 16)) ; 12 on 32-bit 16 on 64-bit
(define op2 (dup "\000" 16))
(define rop (dup "\000" 16))

(define randstate (dup "\000" 32)) ; 20 on 32-bit 32 on 64-bit

; init handles
(__gmpz_init op1)
(__gmpz_init op2)
(__gmpz_init rop)

; handles to speed up factor
(define mp-n (dup "\000" 16))
(define mp-d (dup "\000" 16))
(define mp-k (dup "\000" 16))
(__gmpz_init mp-n)
(__gmpz_init mp-d)
(__gmpz_init mp-k)


; init randstate
(__gmp_randinit_default randstate)
(__gmp_randseed_ui randstate (time-of-day))

; init / reserve result string
(define rops (dup "\000" (MAIN:+ 2 MAX_PRECISION)))

; add two integers
;
(define (GMP:+ p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (__gmpz_add rop op1 op2)
  (if (MAIN:> (__gmpz_sizeinbase rop 10) MAX_PRECISION)
  	(throw-error gmp-size-error))
  (get-string (__gmpz_get_str rops 10 rop))
)

; subtract two integers
;
(define (GMP:- p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (__gmpz_sub rop op1 op2)
  (if (MAIN:> (__gmpz_sizeinbase rop 10) MAX_PRECISION)
  	(throw-error gmp-size-error))
  (get-string (__gmpz_get_str rops 10 rop))
)

; multiply two integers
;
(define (GMP:* p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (dotimes (i 1000000)
    (__gmpz_mul rop op1 op2))
  (if (MAIN:> (__gmpz_sizeinbase rop 10) MAX_PRECISION)
  	(throw-error gmp-size-error))
  (get-string (__gmpz_get_str rops 10 rop))
)

; divide two integers
; return floor value of result
(define (GMP:/ p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (if (MAIN:= (__gmpz_cmp_si op2 0) 0) (throw-error gmp-divzero-error))
  (dotimes (i 1000000)
  (__gmpz_tdiv_q rop op1 op2))
  (get-string (__gmpz_get_str rops 10 rop))
)

; modulo two integers
; return rest value of division result
(define (GMP:% p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (if (MAIN:= (__gmpz_cmp_si op2 0) 0) (throw-error gmp-divzero-error))
  (__gmpz_tdiv_r rop op1 op2)
  (get-string (__gmpz_get_str rops 10 rop))
)

; exponentiation function
; return power(p1 p2)
;
(define (GMP:** p1 p2 , pexp)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (set 'pexp (int p2 0))
  (__gmpz_pow_ui rop op1 pexp)
  (if (MAIN:> (__gmpz_sizeinbase rop 10) MAX_PRECISION)
  	(throw-error gmp-size-error))
  (get-string (__gmpz_get_str rops 10 rop))
)
  
; test of two integers are equal, return true if equal
; otherwise nil
;
(define (GMP:= p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (MAIN:= (__gmpz_cmp op1 op2) 0)
)
  
; test is p1 is smaller than p2 
; return true or nil
;
(define (GMP:< p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (MAIN:< (int64 (__gmpz_cmp op1 op2)) 0)
)

; test is p1 is smaller than p2
; return true or nil
;
(define (GMP:> p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (MAIN:> (int64 (__gmpz_cmp op1 op2)) 0)
)

; test is p1 is smaller or eaual than p2
; return true or nil
;
(define (GMP:<= p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (MAIN:<= (int64 (__gmpz_cmp op1 op2)) 0)
)

; test is p1 is bigger or equal than p2
; return true or nil
;
(define (GMP:>= p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (MAIN:>= (int64 (__gmpz_cmp op1 op2)) 0)
)

; bitwise and two integers
;
(define (GMP:& p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (__gmpz_and rop op1 op2)
  (get-string (__gmpz_get_str rops 10 rop))
)

; bitwise inclusive or two integers
;
(define (GMP:| p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (__gmpz_ior rop op1 op2)
  (get-string (__gmpz_get_str rops 10 rop))
)

; bitwise exclusive or two integers
;
(define (GMP:^ p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (__gmpz_xor rop op1 op2)
  (get-string (__gmpz_get_str rops 10 rop))
)

; bitwise complement of one integer
;
(define (GMP:~ p1)
  (if (not (string? p1)) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_com rop op1)
  (get-string (__gmpz_get_str rops 10 rop))
)

; check if a prime
; returns true if a prime
; returns nil if not a primes or probably not a prime
;
(define (GMP:prime? p1 , r)
  (if (not (string? p1)) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (set 'r (__gmpz_probab_prime_p op1 10))
  (if
    (MAIN:= r 0) nil
    (MAIN:= r 1) (MAIN:= (next-prime (- p1 "1")) p1)
    (MAIN:= r 2) true)
)

; get the next prime higher then arg
;
(define (GMP:next-prime p1)
  (if (not (string? p1)) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_nextprime rop op1)
  (get-string (__gmpz_get_str rops 10 rop))
)

; factorize n
;
(define (GMP:factor n)
  (if (not (string? n)) (throw-error gmp-type-error))
  (set 'factors nil)
  (set 'prevfact nil)
  (__gmpz_set_str mp-n n 0)
  (if (MAIN:> (int64 (__gmpz_cmp_si mp-n 2)) 0)
    (begin
      (__gmpz_set_si mp-d 2)
      (__gmpz_set_si mp-k 0)

      (while (MAIN:!= 0 (__gmpz_divisible_p mp-n mp-d))
        (__gmpz_tdiv_q mp-n mp-n mp-d)
        (__gmpz_add_ui mp-k mp-k 1)
      )
      
      (if (MAIN:> (int64 (__gmpz_cmp_si mp-k 0)) 0)
        (push-factor mp-d mp-k))
      
      (__gmpz_set_si mp-d 3)
      (__gmpz_mul op1 mp-d mp-d)
      (while (MAIN:<= (int64 (__gmpz_cmp op1  mp-n)) 0)
        (__gmpz_set_si mp-k 0)        
        (while (MAIN:!= 0 (__gmpz_divisible_p mp-n mp-d))
          (__gmpz_tdiv_q mp-n mp-n mp-d)
          (__gmpz_add_ui mp-k mp-k 1) )
        (if (MAIN:> (int64 (__gmpz_cmp_si mp-k 0)) 0) (push-factor mp-d mp-k))
        (__gmpz_add_ui mp-d mp-d 2)
        (__gmpz_mul op1 mp-d mp-d) )
    ) 
  ) 
    
  (if (MAIN:> (int64 (__gmpz_cmp_si mp-n 1)))
    (if prevfact
      (begin
        (___gmpz_set_si op1 1) 
    	(push-factor mp-n op1))
      (begin
        (set 'n (get-string (__gmpz_get_str rops 10 mp-n)))
        (push n factors -1)))) 
  factors ; not necessary starting v.9.9.5 because push returns list
)

(define (push-factor f k)
  (set 'f (get-string (__gmpz_get_str rops 10 f)))
  (set 'k (get-string (__gmpz_get_str rops 10 k)))
  (if (not prevfact)
    (begin
      (push f factors -1)
      (set 'k (- k "1"))))
      
  (while (> k "0")
    (push f factors -1)
    (set 'k (- k "1")) )
)    
      	

; get the greates common divisor
;
(define (GMP:gcd p1 p2)
  (if (or (not (string? p1)) (not (string? p2))) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_set_str op2 p2 0)
  (__gmpz_gcd rop op1 op2)
  (get-string (__gmpz_get_str rops 10 rop))
)


; get binomial of <arg1> <arg2>
;
(define (GMP:bin n k)
  (if (not (string? n)) (throw-error gmp-type-error))
  (__gmpz_set_str op1 n 0)
  (set 'k (int k 0))
  (__gmpz_bin_ui rop op1 k)
  (if (MAIN:> (__gmpz_sizeinbase rop 10) MAX_PRECISION)
  	(throw-error gmp-size-error))
  (get-string (__gmpz_get_str rops 10 rop))
)
  
; get factorial of arg
; args may not be bigger than 458
; for 1024 digits precision in the result
;
(define (GMP:fac p1 , n)
  (if (not (string? p1)) (throw-error gmp-type-error))
  (set 'n (int p1))
  (__gmpz_fac_ui rop n)
  (if (MAIN:> (__gmpz_sizeinbase rop 10) MAX_PRECISION)
  	(throw-error gmp-size-error))
  (get-string (__gmpz_get_str rops 10 rop))
)

; get fibonacci of arg
; arg may not be bigger than   for a 1024 digits result
;
(define (GMP:fib p1 , n)
  (if (not (string? p1)) (throw-error gmp-type-error))
  (set 'n (int p1))
  (__gmpz_fib_ui rop n)
  (if (MAIN:> (__gmpz_sizeinbase rop 10) MAX_PRECISION)
  	(throw-error gmp-size-error))
  (get-string (__gmpz_get_str rops 10 rop))
)

; get a random number between 0 and arg - 1
;
(define (GMP:rand p1)
  (if (not (string? p1)) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmpz_urandomm rop randstate op1)
  (get-string (__gmpz_get_str rops 10 rop))
) 

; seed the random generator
;
(define (GMP:seed p1)
  (if (not (string? p1)) (throw-error gmp-type-error))
  (__gmpz_set_str op1 p1 0)
  (__gmp_randseed randstate op1)  
  true
)

(define (check func <arg1> <arg2> result)
   (if (MAIN:= (apply func (list <arg1> <arg2>)) result)
   	(println "GMP:" (term func) "\t-> Ok")
   	(println "Problem in GMP:" (term func)))
)

(context 'MAIN)

; this test assumes that libgmp itself is correct, only the
; newLISP implementation is tested
;
(define (test-GMP)
  ; INTEGER ARITHMETIK
  (GMP:check 'GMP:+ "123" "456" "579")
  (GMP:check 'GMP:- "100" "99" "1")
  (GMP:check 'GMP:* "123" "456" "56088")
  (GMP:check 'GMP:/ "56088" "456" "123")
  (GMP:check 'GMP:% "56088" "456" "0")
  (GMP:check 'GMP:** "2" "10" "1024")
  (GMP:check 'GMP:= "999999" "999999" true)
  (GMP:check 'GMP:< "999999" "1000000" true)
  (GMP:check 'GMP:> "999999" "1000000" nil)
  (GMP:check 'GMP:<= "999999"  "1000000" true)
  (GMP:check 'GMP:>= "999999" "1000000" nil)

  ; BIT OPERATTIONS
  (GMP:check 'GMP:& "0xAAAA" "0xFF00" "43520")
  (GMP:check 'GMP:| "0xAAAA" "0x5555" "65535")
  (GMP:check 'GMP:^ "0xAAAA" "0xAAAA" "0")
  (GMP:check 'GMP:~ "0xFFFF" nil "-65536")

  ; NUMBER THEORY
  (GMP:check 'GMP:prime? "127" nil true)
  (GMP:check 'GMP:next-prime "127" nil "131")
  (GMP:check 'GMP:factor "123" nil '("3" "41"))
  (GMP:check 'GMP:gcd "20" "8" "4")
  (GMP:check 'GMP:bin "10" "2" "45")
  (GMP:check 'GMP:fac "5" nil "120")
  (GMP:check 'GMP:fib "30" nil "832040")

  ; RANDOM NUMBERS
  (GMP:check 'GMP:seed "12345" nil true)
  ;(GMP:check 'GMP:rand "1000000" nil "18235") ; check manually
)

; eof
