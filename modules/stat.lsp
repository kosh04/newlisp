;; @module stat.lsp
;; @description Basic statistics and plotting library
;; @version 2.0 - fixed plot an plotXY routines for gnuplot
;; @version 2.1 - doc changes
;; @version 2.2 - frequency vector in stat:power was shifted left by 1
;; @version 2.3 - $TEMP should be global
;; @version 2.4 - changed deprecated <tt>name</tt> to <tt>term</tt>
;; @author Lutz Mueller, 2001-2010
;; <h2>Functions for statistics and plotting with GNU plot</h2>
;; To use this module it has to be loaded at the beginning of the
;; program file:
;; <pre>
;; (load (append (env "NEWLISPDIR") "/modules/stat.lsp"))
;; ; or shorter
;; (module "stat.lsp")
;; </pre>
;; All functions work on integers and floats or a mix of both. <lists> are normal
;; LISP lists. <matrices> are lists of lists, one list for each row in the
;; two dimensional data matrix. See the function 'stat:matrix' on how to make matrices
;; from lists.
;;
;; This file also shows how to use some of the built in matrix math functions
;; like 'multiply', 'transpose', 'invert' and 'fft'. 
;;
;; Note, that the plot functions need 'gnuplot' installed, a free graphing 
;; package available for most operating systems.
;; See: @link http://www.gnuplot.info/ http://www.gnuplot.info/
;;
;; If no Gnuplot program is found by the module all non-plot routines are still
;; usable.
;;
;;
;; <h2>Summary of functions</h2><br>
;; <h3>Plot functions (requires 'gnuplot')</h3>
;; <pre>
;; stat:plot     - plots one ore more vectors (lists)
;; stat:plotXY   - plots an XY graph from to vectors
;; </pre>
;; The plot functions rely on a $HOME/tmp directory on Mac OS X and other Unix
;; or <tt>c:\tmp</tt> or <tt>c:\tmp</tt> on Windows. When loading the module,
;; a message will be printed to stdout if either the gnuplot executable or a 
;; temporary directory cannot be found.
;;
;; The plot functions are designed for interactive use with no special options,
;; titles on the top or the X and Y axes. Only a legend is printed in the top
;; right corner to identify different data ranges. On Win32 plot functions 
;; should only be used when using newLISP in a command shell, not inside the 
;; newLISP-GS application.
;;
;; To produce more refined graphics the file <tt>tmp/plot</tt> can be edited
;; to produce a different output. The Gnuplot command-file <tt>plot</tt> will
;; look for data files in the same directory. One file for each data range.
;; <h3>General uni- and bi- variate statistics</h3>
;; <pre>
;; stat:sum      - returns the sum of a vector of numbers
;; stat:mean     - returns the arithmetik mean of a vector of numbers
;; stat:var      - returns the estimated variance of numbers in a vector sample
;; stat:sdev     - returns the estimated standard deviation of numbers in a vector
;; stat:sum-sq   - returns the sum of squares of a data vector
;; stat:sum-xy   - returns the sum of products of a two data vectors
;; stat:corr     - returns the correlation coefficient between two vectors
;; stat:cov      - returns the covariance of two number vectors
;; stat:sum-d2   - returns the sum of squared differences of a vector from its mean
;; stat:sum-d2xy - returns sum of squared diffferences of two vectors
;; stat:reqression - calculates the intecept and slope of a regression estimate
;; stat:fit      - return the fitted line using regression coefficients
;; stat:f-prob   - return the probability of an F-ratio of two variances
;; stat:moments  - calulates 1st to 3rd moments from a vector of numbers
;; </pre>
;; <h3>Multi variate statistics</h3>
;; <pre>
;; stat:multiple-reg  - calculates a multiple regression
;; stat:cov-matrix    - calculates a covariance matrix
;; stat:corr-matrix   - calculates a correlation matrix
;; </pre>
;; <h3>Time series</h3>
;; <pre>
;; stat:smooth   - smoothes a vector of numbers
;; stat:lag      - calcultes a difference list with specified lag
;; stat:cumulate - cumulate a data vector
;; stat:power    - calculate the power spectrum of a time series
;; </pre>
;; <h3>Matrix and list utilities</h3>
;; <pre>
;; stat:matrix       - make a matrix from column vectors
;; stat:diagonal     - make a diagonal matrix
;; stat:get-diagonal - return the diagonal of a matrix in a vector
;; stat:mat-map      - map a binary function on to matrices
;; </pre>


(if (< (sys-info -2) 10111)
	(constant (global 'term) name))

(constant (global '$HOME) 
	(or (env "HOME") (env "USERPROFILE") (env "DOCUMENT_ROOT") ""))
(constant (global '$TEMP) 
	(if (= ostype "Win32") (or (env "TEMP") "C:\\temp") (string $HOME "/tmp")))

(if-not $HOME (println "Cannot find home directory"))
(if-not $TEMP (println "Cannot find tmp or temp directory"))

(set 'files '(
	"/usr/local/bin/gnuplot" ; Unix
	"c:/gnuplot/bin/pgnuplot.exe" ; Win32
))

(set 'stat:gnuplot-program (files (or
                (find true (map file? files))
                (begin (println "Cannot find the gnuplot executable") (exit)))))

;; @syntax (stat:corr <X> <Y>)
;; @param <X> A list of numbers.
;; @param <Y> A list of numbers.
;; @return Correlation coefficient of lists <X> and <Y>.

;; @syntax (stat:cov <X> <Y>)
;; @param <X> A list of numbers.
;; @param <Y> A list of numbers.
;; @return Covariance of data in lists <X> and <Y>

;; @syntax (stat:cov-matrix <X>)
;; @param <X> A list of numbers.
;; @return Covariance matrix of <X> with <N> rows and <k> columns.

;; @syntax (stat:corr-matrix <X>)
;; @param <X> A list of numbers.
;; @return Correlation matrix of <X> with <N> rows and <k> columns.

;; @syntax (stat:cumulate <X>)
;; @param <X> A list of numbers.
;; @return The cumulated list of <X>.

;; @syntax (stat:diagonal <item> <N>)
;; @param <item> The diagonal element.
;; @return A diagonal matrix of length <N> with <item> in the diagonal.

;; @syntax (stat:fit <X> <Y>)
;; @param <X> A list of numbers.
;; @param <Y> A list of numbers.
;; @return fitted line based on '(stat:regression X Y)'.

;; @syntax (stat:f-prob <F> <df1> <df2>)
;; @param <F> The variance ratio.
;; @param <df1> Degrees of freedom.
;; @param <df2> Degrees of freedom.
;; @return Probablity of F variance ratio for <df1>, <df2> degress of freedom.

;; @syntax (stat:get-diagonal <X>)
;; @param <X> An matrix filled with numbers.
;; @return A list from the diagonal elements of <X>.

;; @syntax (stat:lag <X> <n>)
;; @param <X> A list of numbers.
;; @param <n> Lag n.
;; @return A differenced list of <X> with a lag of <n>.
;; If the length of list <X> is <l> then the length of the resulting
;; differenced list is <l - n>.

;; @syntax (stat:mat-map <op> <A> <B>)
;; @return Matrix map, e.g. '(stat:mat-map + A B)'.
;; Used for adding and subtracting matrices.

;; @syntax (stat:matrix <C1> .... <CN>)
;; @param <C1> The first column list of values.
;; @param <CN> The Nth column list of values.
;; @return A matrix off  <1> to <N> columns <C>.

;; @syntax (stat:mean <X>) 
;; @param <X> A list of numbers.
;; @return The mean of data in list <X>.

;; @syntax (stat:moments <X>)
;; @param <X> A list of numbers.
;; @return Calculates all moments of list <X>.

;; @syntax (stat:multiple-reg <X> <offY>)
;; @param <X> A list of numbers.
;; @param <offY> Zero based offset into <Y>.
;; @return Multiple regression of vars in <X> onto <Y> at <offsetY>.

;; @syntax (stat:plot <p1> <p2> ... <pN>)        
;; @param <p1> First list of data.
;; @param <p2> Second list of data.
;; @param <pN> Nth list o data
;; @return The process ID of the gnuplot process.
;; @example
;;    (stat:plot '(1 2 3 4 5 4 3 2 1))
;;
;;    (set 'data-A '(1 2 3 2 1 2 3))
;;    (set 'data-B '(2 2 2 2 2 2 2))
;;    (stat:plot data-A data-B)

;; The first example plots one data range. It will appear as <tt>series-1</tt>
;; in the legend. In the second example two data ranges are drawn each in a 
;; distinct color the variable names <tt>data-X</tt> and <tt>data-Y</tt> will
;; be used as labeld in the legend.

;; @syntax (stat:plotXY <list-X> <list-Y> [<srt-style>])
;; @param <list-X> List of x-coordinates to plot.
;; @param <list-Y> List of y-coordinates to plot.
;; @param <str-style> Optional style parameter, e.g. "line"
;; @return The process ID of the gnuplot process.
;; @example
;;    (set 'data-X (random 1 10 100))
;;    (set 'data-Y (random 2 8 100))
;;    (stat:plotXY data-X data-Y)

;; Data points X,Y are plotted on the plane. If no style is specyfied points
;; appear as little crosses. The style <tt>"line"</tt> would connect all data
;; points.

;; @syntax (stat:power <TS>)
;; @param <TS> A time series of numbers.
;; @return The power spectrum of a time series

;; @syntax (stat:regression <X> <Y>)
;; @param <X> A list of numbers.
;; @param <Y> A list of numbers.
;; returns <(b0 b1)> coefficients of regression <Y = b0 + b1*X>.

;; @syntax (stat:sdev <X>)
;; @param <X> A list of numbers.
;; @return Standard deviation of data in list <X>.

;; @syntax (stat:smooth <X> <alpha>)
;; @param <X> A list of numbers.
;; @param <alpha> Smoothing coefficient <0 &lt; alpha &lt; 1>.
;; @return Exponentially smoothed sequence in <X>.

;; @syntax (stat:sum <X>)
;; @param <X> A list of numbers,
;; @return Sum of data in list <X>.

;; @syntax (stat:sum-d2 <X>)
;; @param <X> A list of numbers.
;; @return Sum of squared diffs <(x - mean(X))^2> in list <X>.

;; @syntax (stat:sum-d2xy <X> <Y>)
;; @return Sum of squared differences <(x - y)^2> of elements in lists <X> and <Y>.

;; @syntax (stat:sum-sq <X>)              
;; @param <X> A list of numbers.
;; @return Sum of <x*x> data elements in list <X>.

;; @syntax (stat:sum-xy <X> <Y>)
;; @param <X> A list of numbers.
;; @param <Y> A list of numbers.
;; @return Sum of products <x*y> data elements in lists <X> and <Y>.

;; @syntax (stat:var <X>)
;; @param <X> A list of numbers.
;; @return The variance of the data in list <X>.



(context 'stat)

;---------------------------- gnuplot interface -------------------------------
; 
;  The plot functions produce data files for the plot data and acommand files
;  for gnuplot, then execute gnuplot. Thye data and command files are produced
;  in the platforms $HOME/tmp directory
; 
;  The file 'plot' contains the generated plot commands for gnuplot
;  data is saved in files with the names of symbols containing the data
;  or in files named 'series-1', 'series-2' etc.
; 
;  e.g.: (plot '(1 3 2 5 4)) will produce a 'series-1' ascii file 
;  but (plot age height) will produce 'age' and 'height' data files
; 

;
; plot one or more lists of numbers 'args' is used to access the list of
; input parameters to 'plot'
;
(define-macro (plot)
  (let  ( filelist '()
            cnt 0
            unix (< (& 0xF (last (sys-info))) 5)
            filepath ""
            filename ""
			data "")
	; for each list write an ascii file
	(dolist (elmnt (args))
		(if (symbol? elmnt)
			(begin
				(set 'filepath (string $TEMP "/" (term elmnt)))
				(set 'filename (term elmnt))
				(set 'data (eval elmnt)))
	   		(begin 
				(set 'filepath (string $TEMP "/series-" cnt))
				(set 'filename (string "series-" cnt))
				(set 'data (eval elmnt))))


		(inc cnt)
		(write-file filepath (list2ascii data))
		(push (append "'" filepath "' t \"" filename "\"") filelist))


	(set 'filelist (join (reverse filelist) ","))
	; write file with plot commands depending on OS - Windows doesn't have PWD

;	(println "$TEMP:" $TEMP)
;	(println "filelist:" filelist)
;	(println "filepath:" filepath)
;	(println "filename:" filename)
;	(println "program:" gnuplot-program)

	(if unix
		(begin
			(write-file (append $TEMP "/plot") 
					(append "set data style lines; plot " filelist "\r\n"))
			(process (append gnuplot-program " -persist "  (env "HOME") "/tmp/plot")))
	/* else Win32 */
		(begin
			(write-file (append $TEMP "/plot") 
					(append "set data style lines; plot " filelist ";pause -1;\r\n"))
			(process (append gnuplot-program " " $TEMP "/plot")))
)))

;
; plot to lists of numbers in XY-fashion
;
; optionally define a style e.g: "line"
; if style is unused dots are plotted
;
(define (plotXY X Y style , data st fle unix)
	(set 'unix (< (& 0xF (last (sys-info))) 5 ))
	(set 'data (map list (map string X) (map string Y)))
	(set 'filepath (append $TEMP "/plot-XY"))
	(set 'fle (open filepath "write"))
	(dolist (x data)	
		(write-line fle (join x " ")))
	(close fle)
	(if (not style) 
		(set 'st "")
		(set 'st (append "set data style " style "; ")))

	(if unix
		(begin
			(write-file (append $TEMP "/plot") (append st "plot '" filepath "' t \"\";"))
			(process (append gnuplot-program " -persist " $TEMP "/plot")))
		/* else Win32 */
		(begin
			(write-file (append $TEMP "/plot") (append st "plot '" filepath "' t \"\"; pause -1;"))
			(process (append gnuplot-program " " $TEMP "/plot")))
	)
)


;-------------------  General uni and bi-variate statistics --------------------

; sum of a data vector X
(define (sum X)
	(apply add X))

; mean of a data vector X
(define (mean X)
	(div (sum X) (length X)))

; variance of a data vecor X
(define (var X)
	(div (sum-d2 X) (sub (length X) 1)))

; standard deviation of a data vector X
(define (sdev X)
	(sqrt (var X)))

; sum of squares of a data vector X
(define (sum-sq X)
	(apply add (map mul X X)))

; sum of the product of data vectors X*Y
(define (sum-xy X Y)
	(apply add (map mul X Y)))

; covariance of data vectors X Y
(define (cov X Y)
	(sub (sum-xy X Y) (div (mul (sum X) (sum Y)) (length X))))

; sum of sqared differenses of X to mean of X
(define (sum-d2 X)
	(sub (sum-sq X) (div (mul (sum X) (sum X)) (length X))))

; Pearson r, product moment correlation of data vectors X and Y
(define (corr X Y)
	(div (cov X Y) (sqrt (mul (sum-d2 X) (sum-d2 Y)))))

; regression Yest = b0 + b1*X calculates intercept b0 and slope b1
(define (regression X Y)
	(set 'b1 (div (cov X Y) (sum-d2 X)))
	(set 'b0 (sub (mean Y) (mul b1 (mean X))))
	(list b0 b1))

; fitted line using regression Y on X
(define (fit X Y, coeffs b0 b1)
	(set 'coeffs (regression X Y))
	(set 'b0 (first coeffs))
	(set 'b1 (last coeffs))
	(map (lambda (x) (add b0 (mul x b1))) X))

; sum of squared differences of X and Y
(define (sum-d2xy X Y)
  (apply add (map (lambda (x y) (mul (sub x y) (sub x y))) X Y)))


; moments of a vector of numbers
;
(define (moments vector, n median mean avg-dev std-dev var skew kurtosis dev sum)
  (set 'n (length vector))

  (set 'sum (apply add vector))
  (set 'mean (div sum n))

  (set 'avg-dev 0 'std-dev 0 'var 0 'skew 0 'kurtosis 0)

  (set 'dev (map sub vector (dup mean n)))
  (set 'avg-dev (div (apply add (map abs dev)) n))
  (set 'var (div (apply add (map mul dev dev)) (- n 1)))
  (set 'skew (apply add (map mul dev dev dev)))
  (set 'kurtosis (apply add (map  mul dev dev dev dev)))

  (set 'std-dev (sqrt var))

  (if (> var 0.0)
    (begin
	(set 'skew (div skew (mul n var std-dev)))
	(set 'kurtosis (sub (div kurtosis (mul n var var)) 3.0))))

  (sort vector)
  (set 'mid (/ n 2))

  (if (= 0 (% n 2))
	(set 'median (div (add (nth mid vector) (nth (- mid 1) vector)) 2))
	(set 'median (nth mid vector)))

  (list n median mean avg-dev std-dev var skew kurtosis)

;  (println (format "n:                  %d" n))
;  (println (format "median:             %f" median))
;  (println (format "mean:               %f" mean))
;  (println (format "average_deviation:  %f" avg-dev))
;  (println (format "standard_deviation: %f" std-dev))
;  (println (format "variance:           %f" var))
;  (println (format "skew:               %f" skew))
;  (println (format "kurtosis:           %f" kurtosis))
)

;-------------------------------- Time Series ----------------------------------

; expontial smoothing with 0 < alpha <= 1
(define (smooth lst alpha , previous slist)
  (set 'previous (first lst))
  (set 'slist '())
  (dolist (elmnt lst) 
   (set 'previous (add (mul alpha elmnt) (mul (sub 1 alpha) previous))) 
   (push previous slist))
  (reverse slist)) ; could be written shorter starting v.9.9.5
                   ; because push returns the modified list

;
; seasonal difference list with variable lag
; the resulting list is lag shorterm than the original
;
(define (lag lst n , sLst)
	(set 'sLst lst)
	(dotimes (i n) (pop lst))
	(set 'sLst (slice sLst 0 (length lst)))
	(map sub lst sLst))


;
; cumulate of a list
;
(define (cumulate lst , sc cum)
	(set 'sc 0.0)
	(set 'cum '())
	(dolist (x lst)
		(push (inc sc x) cum))
	(reverse cum)) ; could be written shorter after 9.9.5
	               ; because push returns the list

;
; power spectrum
;
; takes a rows by 2 columns matrix with real part in the first and 
; imagenary part the in the second column. If all numbers are real
; then the second column is just 0's.
;
; returns a matrix with two rows. First row contains power numbers
; and second row contains the respective frequencies
;

(define (power ts , lenOrg fts n n2 ps mid frqs)
	; remember original length
	(set 'lenOrg (length ts))
	; do discrete fourier transform
	(set 'fts (transpose (fft ts)))
	; calc power spectrum 
	(set 'n (length (transpose fts)))
	(set 'n2 (mul n n))
	(set 'ps (map (lambda (x y) (add (mul x x) (mul y y))) (nth 0 fts) (nth 1 fts)))
	(set 'ps (map (lambda (x) (mul (div x n2) 2)) ps))
	; the first and last are not multiplied by 2, divide them back
	; use deprecated nth-set in versions older than 9.9.02
	(if (< (sys-info -2) 9902)
		(nth-set (ps 0) (div (first ps) 2))
		(setf (ps 0) (div (first ps) 2)))
	(set 'mid (sub (div n 2) 1))
	(replace mid ps (div (nth mid ps) 2))
	; calc a vector with frequencies, adjusted for the new power-2 length
	; which came back from the FFT
	(set 'frqs (sequence 0 (- n 1)))
	(set 'frqs (map (lambda (x) (mul (div x n) lenOrg)) frqs))
	(transpose (matrix ps frqs)))



;------------------------- multivariate statistics -----------------------------

;
; multiple regression of variables in X onto one of varsiables in X, Y
; indicated by column offset offY
; 
; X is N rows by k columns, the column at offset offY is Y
;
; returns a matrix with two rows:
; first row is regression coefficients and multiple R: b0, b1, b2 ....., R
; second row is sum of squares: regression-SQ, error-SQ, total-SQ
; (the unused part of the second row is zero padded)
;
; the SQs can be used to calculate mean sqares for regression and error:
;
; regression-MSQ = regression-SQ / (k - 1)
; error-MSQ = error-sq / (n - k - 1)
;
; F-ratio = regression-MSQ / error-MSQ  
; with k and (n - k - 1) df degreees of freedom
;
;
;
;
(define (multiple-reg X offY ,  Y Ycoffs b b0 R2 Yest sqErr sqTotal sqReg sq d)
	(set 'covX (cov-matrix X))
	(set 'Y (extract-col X offY))
	; covX is the covariance matrix
	(pop covX offY)
	(set 'cvX (transpose covX))
	; the covariance matrix is reduced to cvX and the
	; extracted values put in Ycoffs
	(set 'Ycoffs (matrix (pop cvX offY)))
	; b contains the regression coefficients except for b0
	(set 'b (multiply (invert cvX) Ycoffs))
	; calculate multiple R2 as b'*b / sqTotal
	(set 'sqTotal (sum-d2 Y))
	(set 'R2 (div (first (first (multiply (transpose b) Ycoffs))) sqTotal))
	; estimate Y without b0
	(set 'Yest (multiply (reduce-col X offY) b))
	; calculate b0, d is the difference between Y and the Y estimate
	; b0 is the mean of differences between Y and Yest
	(set 'd (mat-map sub (matrix Y) Yest))
	(set 'b0 (mean (first (transpose d))))
	; estimate Y including b0
	(set 'Yest (mat-map add Yest (matrix (dup b0 (length Yest)))))
	; error sum of squares
	(set 'sqErr (sum-d2xy Y (first (transpose Yest))))
	; regression sum of squares
	(set 'sqReg (sub sqTotal sqErr))
	; make list b out of b0, b1, b2 ... sqrt(R2)
	(set 'b (append (list b0) (first (transpose b)) (list (sqrt R2))))
	; make list sq out of sqReg, sqErr and sqTotal
	(set 'sq (list sqReg sqErr sqTotal))
	; return matrix with two rows:
	(transpose (matrix b sq)))



;
; covariance matrix cov 
; 
; matrix x with N rows and k columns
; 
;
(define (cov-matrix X , XtX N I sumX sumX2)
	(set 'XtX (multiply (transpose X) X)) 
	(set 'N (length X))
	(set 'I (matrix (dup t1 N)))
	(set 'sumX (multiply (transpose X) I)) 
	(set 'sumX2 (multiply sumX (transpose sumX)))  
	(set 'sumX2 (multiply sumX2 (diagonal (div 1 N) (length sumX2))))
	(mat-map sub XtX sumX2))


;
; correlation matrix
;
; matrix X with N rows and k columns
;
;
(define (corr-matrix X , covX N d dd)
	(set 'covX (cov-matrix X))
	(set 'd (matrix (get-diagonal covX)))
	(set 'dd (multiply d (transpose d)))
	(set 'dd (map (lambda (z) (map sqrt z)) dd))
	(mat-map div covX dd))

	
;
; probablity of F variance ratio with degrees of freedom df1 df2
;
;; 
(define (f-prob F df1 df2)
  (let (prob (mul 2 (betai (div df2 (add df2 (mul df1 F))) 
                           (mul 0.5 df2) 
                           (mul 0.5 df1)))) 
   (div (if (> prob 1) (sub 2 prob) prob) 2)))	


;----------------------------- utility functions -------------------------------

;
; make a matrix from 1 up to 16 lists
;
(define (matrix)
	(transpose (args)))


;
; make a diagonal matrix n by n and elmnt in the diagonal
; 
;
(define (diagonal elmnt n, m lst)
	(set 'm '())
	(dotimes (i n)
		(set 'lst (dup 0 n))
		; use deprecated nth-set in versions older than 9.9.02
		(if (< (sys-info -2) 9902)
			(nth-set (lst i) elmnt)
			(setf (lst i) elmnt))
		(push lst m))
	(reverse m)) ; make shorter starting v.9.9.5
		
;
; get the diagonal from a square matrix
;
(define (get-diagonal X , d x)
	(set 'd '())
	(dotimes (idx (length X))
		(push (nth idx (nth idx X)) d))
	(reverse d))

;
; matrix map
;
; e.g.: (mat-map sub A B) ;; for matrix subtraction
;
(define (mat-map op A B)
	(map (lambda (x y) (map op x y)) A B))

;
; reduce matrix by a column at offset
;
; returns the reduced matrix
;
(define (reduce-col mat off, X)
	(set 'mat (transpose mat))
	(pop mat off)
	(transpose mat))

;
; extract a column from a matrix
;
; returns the extracted column
;
(define (extract-col mat off, X)
	(pop (transpose mat) off))

;
; convert list to ascii lines terminated by CR-LF
; for storage in files usable by Gnuplot, R, Excel etc.
;
; example: 
;
; (write-file "MyData.txt" (list2ascii mydata-list))
;
(define (list2ascii lst)
	(append (join (map string lst) "\r\n") "\r\n"))

; eof


