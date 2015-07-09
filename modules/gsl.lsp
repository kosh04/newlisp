;; @module gsl.lsp 
;; @description Selected functions from the GNU Scientific Library
;; @version 1.0 - initial release. Minimum newLISP version is 10.4.0.
;; @version 1.1 - added check for extended ffi enabled version.
;; @version 1.2 - changed ostype Win32 to Windows
;; @author Lutz Mueller 2012

;; <h2>Module GSL For The GNU Scientific Library</h2>
;; The @link http://www.gnu.org/software/gsl/ GSL GNU Scientific Library
;; implements over a 1000 functions from different subject areas. In this 
;; release of the 'gsl.lsp' module only a few linear algebra functions
;; are implemented.

;; To use this module, the main GSL library 'libgsl' and  a supporting
;; library 'libgslcblas' must be installed. 
;;
;; This module requires newLISP version 10.4.0 as a minimum. For 64-bit
;; newLISP on Mac OSX, Linux and other Unix, 10.4.2 is the minimum. 
;; 
;; Precompiled 32-bit libraries for the binary distributions of newLISP 
;; versions are available in the
;; @link http://www.nuevatec.com/GSL/ http://www.nuevatec.com/GSL/
;; directory. See the 'INSTALL.txt' file in that directory for instructions how
;; to install the library files.
;;
;; The module contains <tt>(test-gsl)</tt> to test all implemented functions.

;; @syntax (gsl:CholeskyD <matrix-A>)
;; @param <matrix-A> A square matrix of m row vectors with n = m column elements each.
;; @return The matrix L .
;; The function performs a Cholesky decomposition of <matrix-A>. The matrix A must
;; be symmetric and positive definite square.
;;
;; A = L * Lt
;;
;; Lt is the transposition of L. 
;;
;; @example
;; (gsl:CholeskyD '((4 2 -2) (2 10 2) (-2 2 5)))
;;
;; gsl:Cholesky-L => 
;;
;; (  ( 2 0 0 )
;;    ( 1 3 0 )
;;    (-1 1 1.732050808)  )
;;

;; @syntax (gsl:Cholesky-solve <matrix-A> <vector-b>)
;; @param <matrix-A> A square matrix of m row vectors with n = m column elements each.
;; @params <vector-b> A vector of n elements.
;; @return Vector x containing a solution for Ax = b .
;;
;; @example
;; (gsl:Cholesky-solve '((4 2 -2) (2 10 2) (-2 2 5)) '(1 2 3)) 
;; 
;; => (0.8333333333 -0.1666666667 1)
;;

;; @syntax (gsl:QRD <matrix-A>)
;; @param <matrix-A> A list of m row vector lists with n column elements each.
;; @return Vector <em>tau</em> of k = min(n, m) elements.
;; The function does QR decomposition of a matrix A.
;;
;; A = Q * R
;;
;; The function works for both, square and rectangular matrices.
;; The number of rows m in A must be equal to or greater than the number of columns n.
;; The orthogonal m * m matrix Q can be found in the variable 'gsl:QR-Q'.
;; The m * n matrix R can be found in the variable 'gsl:QR-R'.
;;
;; @example
;; (gsl:QRD '((12 -51 4) (6 167 -68) (-4 24 -41)) ) => (1.857142857 1.993846154 0)
;;
;; gsl:QR-Q => 
;;
;; (  ( 0.8571428571 -0.3942857143 -0.3314285714 )
;;    ( 0.4285714286  0.9028571429  0.03428571429)
;;    (-0.2857142857  0.1714285714 -0.9428571429 )  )
;;
;; gsl:QR-R => 
;;
;; (  (14  21 -14 )
;;    ( 0 175 -70 )
;;    ( 0   0  35 )  )

;; @syntax (gsl:QR-solve <matrix-A> <vector-b>)
;; @param <matrix-A> A matrix of m row vectors with n column elements each.
;; @params <vector-b> A vector of n elements.
;; @return Vector x containing a solution for Ax = b .
;; Matrix A is either square or overdetermined with m > n, more rows than columns.
;; When m > n, then the variable 'gsl:QR-residual' contains a vector of residuals.
;; For a square matrix A this vector contains 0's.
;;
;; @example
;; (gsl:QR-solve '((12 -51 4) (6 167 -68) (-4 24 -41)) '(1 2 3))
;; 
;; => (0.009387755102 -0.02432653061 -0.08832653061)
;;
;; (gsl:QR-solve '((1 2) (3 4) (5 6) (7 8)) '(1 2 3 4))
;; => (9.690821045e-16 0.5)
;;
;; gsl:QR-residual 
;; => (2.512815377e-17 1.103917396e-16 -2.961679405e-16 1.606480472e-16)

;; @syntax (gsl:SVD <matrix-A>)
;; @param <matrix-A> A matrix of m row vectors with n column elements each.
;; @return A vector of diagonal elements from the S matrix.
;; The function does a SVD (Singular Value Decomposition) of the <matrix-A> into
;; its components U, S and V. Matrix U is orthogonal m*n. S is a diagonal
;; square matrix of n singular values. V is a n*n square orthogonal matrix.
;; The function works for both, square and rectangular matrices.
;;
;; A = U * S * Vt 
;;
;; Vt is the transposition of V.
;;
;; The number of rows m in A must be equal to or greater than the number of columns n.
;; The m*n matrix U can be found in the variable 'gsl:SVD-U'.
;; The diagonal elements of matrix S can be found in the vector variable 'gsl:SVD-S'.
;; The n*n matrix V can be found in the variable 'gsl:SVD-V'.
;;
;; @example
;; (gsl:SVD '((1 2) (3 4) (5 6) (7 8))) => (14.2690955 0.6268282324)
;;
;; gsl:SVD-U =>
;; (  (-0.1524832333 -0.8226474722 )
;;    (-0.3499183718 -0.4213752877 )
;;    (-0.5473535103 -0.02010310314)
;;    (-0.7447886488  0.3811690814 )  )
;;  
;; gsl:SVD-S => 
;;
;; (14.2690955 0.6268282324)
;;
;; gsl:SVD-V => 
;;
;; (  (-0.641423028   0.7671873951)
;;    (-0.7671873951 -0.641423028 )  )

;; @syntax (gsl:SVD-solve <matrix-A> <vector-b>)
;; @param <matrix-A> A matrix of m row vectors with n column elements each.
;; @params <vector-b> A vector of n elements
;; @return a vector x containing a solution for Ax = b .
;; The number of rows m in A must be equal to or greater than the number n of columns.
;;
;; @example
;; (gsl:SV-solve '((1 2) (3 4) (5 6) (7 8)) '(1 2 3 4))
;; 
;; => (5.551115123e-16 0.5 0 0)
;; 

; on Mac OSX when using 32-bit newLISP from the normal binary Mac OSX installer
; use the following to make the libraries:
;
; ./configure CFLAGS="-O2 -m32"
; make
; sudo make install
;
; install needs the root password
;

(when (!= 1024 (& 1024 (sys-info -1)))
    (println "This module needs newLISP compiled for extended ffi.")
    (println "Must exit")
    (exit))

; the following assumes the libararies installed in the system library path
(set 'LIB 
	(if 
		(= ostype "Windows") "libgsl-0.dll" ; 32-bit or 64-bit
		(= ostype "OSX")   "libgsl.dylib" ; 32-bit or 64-bit
		(= ostype "Linux") "/usr/local/lib/libgsl.so" ; 32-bit or 64-bit
	))

; load libgslcblas which contans functions referenced by libgsl
; the symbol cblas_sdsdot is not needed but newLISP versions before
; 10.4.2 can not use the 'import' statement without a function name
; libgslcblas is mainly needed internally by libgsl.
; On windows the library is automatically loaded by libgsl-0.dll.
(if 
    (= ostype "OSX") (import "libgslcblas.dylib" "cblas_sdsdot")
    (= ostype "Linux") (import "/usr/local/lib/libgslcblas.so" "cblas_sdsdot")
)
    
; structs are defined but only needed for debugging, instead use "void*"
(struct 'complex "double" "double") ; complex numbers
(struct 'block "long" "void*") ; vectors and matrices allocation block, size data-ptr
(struct 'vector "long" "long" "void*" "block" "int") ; size stride data block owner
(struct 'matrix "long" "long" "long" "void*" "block" "int") ; size1 size2 tda data block owner 

(import LIB "gsl_set_error_handler_off" "void*") ; turn of error handler
(import LIB "gsl_strerror" "char*" "int") ; get error text

; pointers are only used passed around internally
; for debugging (unpack vector vptr) can always be used
(import LIB "gsl_vector_alloc" "void*" "long") 
(import LIB "gsl_vector_calloc" "void*" "long") 
(import LIB "gsl_vector_free" "void" "void*") 
(import LIB "gsl_vector_get" "double" "void*" "long")
(import LIB "gsl_vector_set" "void" "void*" "long" "double")

; in matrix functions "void*" can be used instead of struct "matrix"
; because pointers are only used passed around internally
; for debugging (unpack matrix Xptr) can always be used
(import LIB "gsl_matrix_alloc" "void*" "long" "long")
(import LIB "gsl_matrix_calloc" "void*" "long" "long")
(import LIB "gsl_matrix_free" "void" "void*") 
(import LIB "gsl_matrix_get" "double" "void*" "long" "long")
(import LIB "gsl_matrix_set" "void" "void*" "long" "long" "double")
(import LIB "gsl_matrix_scale" "int" "void*" "double")

; linear algebra functions
(import LIB "gsl_linalg_cholesky_decomp" "int" "void*")
(import LIB "gsl_linalg_cholesky_solve" "int" "void*" "void*" "void*")
(import LIB "gsl_linalg_QR_decomp" "int" "void*" "void*")
(import LIB "gsl_linalg_QR_unpack" "int" "void*" "void*" "void*" "void*")
(import LIB "gsl_linalg_QR_solve" "int" "void*" "void*" "void*" "void*")
(import LIB "gsl_linalg_QR_lssolve" "int" "void*" "void*" "void*" "void*" "void*")
(import LIB "gsl_linalg_SV_decomp" "int" "void*" "void*" "void*" "void*")
(import LIB "gsl_linalg_SV_solve" "int" "void*" "void*" "void*" "void*" "void*")

; turn of error handler
; instead check return values from gsl_xxx_xxx functions
; and do 'throw-error' retrieving error text with 'gsl_strerror'
(gsl_set_error_handler_off) 

; helper functions for translating C-arrays and vectors to lists and back

(define (get-vector-from-list x)
    (when (array? x) (set 'x (array-list x)))
    (letn (m (length x) xptr (gsl_vector_calloc m))
        (dotimes (i m) (gsl_vector_set xptr i (pop x)))
        (list xptr m))
)
        
(define (get-matrix-from-list X)
    (when (array? X) (set 'X (array-list X)))
    (let (m (length X) n (length (X 0)) Xptr 0)
        (set 'Xptr (gsl_matrix_calloc m n))
        (dotimes (i m) (set 'row (pop X)) ; row
            (dotimes (j n) (gsl_matrix_set Xptr i j (pop row)))) ; col
        (list Xptr m n))
)

(define (get-list-from-vector xr)
	(let (xptr (xr 0) n (xr 1) result nil)
		(dotimes (i n)
			(push (gsl_vector_get xptr i) result -1))
	result
	)
)

(define (get-list-from-matrix Xr)
    (let (row nil col nil result nil Xptr (Xr 0) m (Xr 1) n (Xr 2))
        (dotimes (i m) 
            (set 'row nil)
            (dotimes (j n)
                (push (gsl_matrix_get Xptr i j) row -1))
            (push row result -1))
    result
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; API functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (gsl:CholeskyD A)
    (letn   (
            Ar (get-matrix-from-list A)
            Aptr (Ar 0)
            m (Ar 1)
            n (Ar 2)
            result 0
            )
        (set 'result (gsl_linalg_cholesky_decomp Aptr))
        (unless (zero? result)
            (throw-error (gsl_strerror result)))
        ; zero the triangle above diagonal
        (for (i 0 (- m 2)) 
            (for (j (+ i 1) (- n 1)) 
                (gsl_matrix_set Aptr i j 0.0)))
        (set 'result (get-list-from-matrix Ar))
        (gsl_matrix_free Aptr)
    result) ; matix L of A = LLt
)            

(define (gsl:Cholesky-solve A b)
    (letn   (
            Ar (get-matrix-from-list A)
            Aptr (Ar 0)
            m (Ar 1)
            n (Ar 2)
            br (get-vector-from-list b)
            bptr (br 0)
            xptr (gsl_vector_calloc n)
            result 0
            )
        (set 'result (gsl_linalg_cholesky_decomp Aptr))
        (unless (zero? result)
            (throw-error (gsl_strerror result)))
        (set 'result (gsl_linalg_cholesky_solve Aptr bptr xptr))
        (unless (zero? result)
            (throw-error (gsl_strerror result)))
        (set 'result (get-list-from-vector (list xptr n)))
        (gsl_matrix_free Aptr)
        (gsl_vector_free bptr)
        (gsl_vector_free xptr)
        result)
)

(define (gsl:QRD A flag)
    (letn   (
            Ar (get-matrix-from-list A)
            Aptr (Ar 0)
            m (Ar 1)
            n (Ar 2)
            nt (min m n)
            Tptr (gsl_vector_calloc nt)
            Qptr (gsl_matrix_calloc m m)
            Rptr (gsl_matrix_calloc m n)
            result 0
            )
        (set 'result (gsl_linalg_QR_decomp Aptr Tptr))
        (unless (zero? result)
            (throw-error (gsl_strerror result)))
        ; Aptr contains QR
        (set 'result (gsl_linalg_QR_unpack Aptr Tptr Qptr Rptr))
        (unless (zero? result)
            (throw-error (gsl_strerror result)))
        (set 'gsl:QR-tau (get-list-from-vector (list Tptr nt)))
        ; Q and R have reverse signs compared with published
        ; examples but values are the same and verify A = QR.
        ; reverse signs for Q and R unless flag set true
        (unless flag
            (gsl_matrix_scale Qptr -1)
            (gsl_matrix_scale Rptr -1)
            ; avoid -0 in lower triangular
            (for (i 1 (- m 1)) (for (j 0 (- i 1)) 
                (gsl_matrix_set Rptr i j 0))))
        (set 'gsl:QR-Q (get-list-from-matrix (list Qptr m m)))
        (set 'gsl:QR-R (get-list-from-matrix (list Rptr m n)))
        (gsl_matrix_free Aptr)
        (gsl_vector_free Tptr)
        (gsl_matrix_free Qptr)
        (gsl_matrix_free Rptr)
        gsl:QR-tau); vector tau 
)

(define (gsl:QR-solve A b)
    (letn   (
            Ar (get-matrix-from-list A)
            Aptr (Ar 0)
            m (Ar 1)
            n (Ar 2)
            nt (min m n)
            tptr (gsl_vector_calloc nt)
            br (get-vector-from-list b)
            bptr (br 0)
            xptr (gsl_vector_calloc n)
            resptr (gsl_vector_calloc m)
            result 0
            )
        (set 'result (gsl_linalg_QR_decomp Aptr tptr))
        (unless (zero? result)
            (throw-error (gsl_strerror result)))
        ; Aptr contains QR
        (if (> m n) ; more rows than cols
            (set 'result (gsl_linalg_QR_lssolve Aptr tptr bptr xptr resptr))
            (set 'result (gsl_linalg_QR_solve Aptr tptr bptr xptr)) )
        (unless (zero? result)
            (throw-error (gsl_strerror result)))
        (set 'result (get-list-from-vector (list xptr n)))
        (set 'gsl:QR-residual (get-list-from-vector (list resptr m)))
        (gsl_matrix_free Aptr)
        (gsl_vector_free tptr)
        (gsl_vector_free bptr)
        (gsl_vector_free xptr)
        (gsl_vector_free resptr)
        result)
)

(define (gsl:SVD A)
    (letn   (
            Ar (get-matrix-from-list A)
            Aptr (Ar 0)
            m (Ar 1)
            n (Ar 2)
            Sptr (gsl_vector_calloc n)
            Vptr (gsl_matrix_calloc n n)
            work (gsl_vector_calloc n)
            result 0)
        (set 'result (gsl_linalg_SV_decomp Aptr Vptr Sptr work))
        (unless (zero? result)
            (throw-error (gsl_strerror result)))
		(set 'gsl:SVD-U (get-list-from-matrix Ar))
		(set 'gsl:SVD-S (get-list-from-vector (list Sptr n)))
		(set 'gsl:SVD-V (get-list-from-matrix (list Vptr n n)))
        (gsl_matrix_free Aptr)
        (gsl_vector_free Sptr)
        (gsl_matrix_free Vptr)
        (gsl_vector_free work)
        gsl:SVD-S) ; vector of diagonals of S of A = USVt
)

(define (gsl:SV-solve A b)
    (letn   (
            Ar (get-matrix-from-list A)
            Aptr (Ar 0)
            m (Ar 1)
            n (Ar 2)
            Sptr (gsl_vector_calloc n)
            Vptr (gsl_matrix_calloc n n)
            work (gsl_vector_calloc n) 
            xptr (gsl_vector_calloc n) 
            br (get-vector-from-list b)
            bptr (br 0)
            result 0)
        (set 'result (gsl_linalg_SV_decomp Aptr Vptr Sptr work))
        (unless (zero? result)
            (throw-error (gsl_strerror result)))
        (set 'result (gsl_linalg_SV_solve Aptr Vptr Sptr bptr xptr))
        (unless (zero? result)
            (throw-error (gsl_strerror result)))
        (set 'result (get-list-from-vector (list xptr n)))
        (gsl_matrix_free Aptr)
        (gsl_vector_free Sptr)
        (gsl_matrix_free Vptr)
        (gsl_vector_free work)
        (gsl_vector_free bptr)
        (gsl_vector_free xptr)
        result)
)        

; User helper functions

(define (gsl:diagonal x flag)
    (when (array? x) (set 'x (array-list x)))
    (letn (len (length x) A (array len len (dup 0 len)))
        (dotimes (i len) (setf (A i i) (pop x)))
        (unless flag A (array-list A)))
)

(context MAIN)

;;;;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-gsl)
    ; Cholesky decomp
    (println)
    (println "========== Cholesky example A = L * Lt")
    (println "(gsl:CholeskyD '((4 2 -2) (2 10 2) (-2 2 5)))" )
    (set 'L (gsl:CholeskyD '((4 2 -1) (2 10 2) (-2 2 5))))
    (println)
    (println "L -> ") (map println L)
    (println)
    (println "verify LLt ->") (map println (multiply L (transpose L)))
    (println)
    (println "(gsl:Cholesky-solve '((4 2 -2) (2 10 2) (-2 2 5)) '(1 2 3))")
    (set 'x (gsl:Cholesky-solve '((4 2 -2) (2 10 2) (-2 2 5)) '(1 2 3)))
    (println)
    (println "x -> " x)
    (println "Ax -> " (multiply '((4 2 -2) (2 10 2) (-2 2 5)) (transpose (list x))))

    ; QR decomp example
    (println)
    (println "========== QR example A = Q * R")
    (println "(gsl:QRD '((12 -51 4) (6 167 -68) (-4 24 -41)) ) tau vector ===> " 
        (gsl:QRD '((12 -51 4) (6 167 -68) (-4 24 -41)) ))

    (println)
    (println "Q -> ") (map println gsl:QR-Q)
    (println)
    (println "R -> ") (map println gsl:QR-R)
    (println)
    (println "verify QR -> ") (map println (multiply gsl:QR-Q gsl:QR-R))
    (println)
    (println "(gsl:QR-solve '((12 -51 4) (6 167 -68) (-4 24 -41)) '(1 2 3))")
    (set 'x (gsl:QR-solve '((12 -51 4) (6 167 -68) (-4 24 -41)) '(1 2 3)))
    (println)
    (println "x -> " x)
    (println "Ax -> " (multiply '((12 -51 4) (6 167 -68) (-4 24 -41)) (transpose (list x))))
    (println "residual -> " gsl:QR-residual)
    (println)
    (println "(gsl:QR-solve '((1 2) (3 4) (5 6) (7 8)) '(1 2 3 4))")
    (set 'x (gsl:QR-solve '((1 2) (3 4) (5 6) (7 8)) '(1 2 3 4)))
    (println)
    (println "x -> " x)
    (println "Ax -> " (multiply '((1 2) (3 4) (5 6) (7 8)) (transpose (list x))))
    (println "residual -> " gsl:QR-residual)

    ; SV decomp example
    (println)
    (println "========== SVD example A = U * S * Vt")
    (println "(gsl:SVD '((1 2) (3 4) (5 6) (7 8))) => " (gsl:SVD '((1 2) (3 4) (5 6) (7 8))))
    (println)
    (println "U -> ") (map println gsl:SVD-U)        
    (println)
    (println "diagonal of S -> ") (println gsl:SVD-S)        
    (println)
    (println "V -> ") (map println gsl:SVD-V)        
    (println)
    (println "verify USVt -> ") (map println
        (multiply (multiply gsl:SVD-U (gsl:diagonal gsl:SVD-S)) (transpose gsl:SVD-V)))
    (println)
    (println "(gsl:SV-solve '((1 2) (3 4) (5 6) (7 8)) '(1 2 3 4))")
    (set 'x (gsl:SV-solve '((1 2) (3 4) (5 6) (7 8)) '(1 2 3 4)))
    (println)
    (println "x -> " x)
    (println "Ax -> " (multiply '((1 2) (3 4) (5 6) (7 8)) (transpose (list x))))
    (println)
    (println "(gsl:SV-solve '((12 -51 4) (6 167 -68) (-4 24 -41)) '(1 2 3))")
    (set 'x (gsl:SV-solve '((12 -51 4) (6 167 -68) (-4 24 -41)) '(1 2 3)))
    (println)
    (println "x -> " x)
    (println "Ax -> " (multiply '((12 -51 4) (6 167 -68) (-4 24 -41)) (transpose (list x))))
    true
)

;(test-gsl) (exit)

; eof ;
