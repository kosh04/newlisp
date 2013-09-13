;; @module plot.lsp
;; @description Routines for creating data plots.
;; @version 1.1 initial release
;; @version 1.2 allow different length data vectors
;; @version 2.0 added plotXY for plotting data points -> (x, y)
;; @version 2.1 added plot:reset for resetting optional labels and settings
;; @version 2.2 option plot:data-min, plot:data-max did not work
;; @author Lutz Mueller, September 2011, April 2012
;;
;; In its initial release the <tt>plot.lsp</tt> module can draw
;; simple line plots from one to five data sets. In its simplest
;; form only the <tt>plot</tt> command is necessary. A group of
;; parameters can be set to further customize the plot. Plots can
;; be save to a graphics file.
;;
;; Load the module using:
;; <pre>(load "plot.lsp")</pre>
;; This module runs the newLISP Java based Guiserver. The file
;; <tt>guiserver.jar</tt> is installed by one of the newLISP binary
;; installers in a standard location. Executing <tt>(test-plot)</tt>
;; will generate a test plots which can be seen here:
;; @link http://newlisp.org/code/example-line-plot.png line-plot
;; and 
;; @link http://newlisp.org/code/example-xy-plot.png XY-plot  .
;; 
;; Several variables can be set optionally to change the size,
;; and positioning of the plot area in the image.
;; Other variables can be set to control the partioning of the grid,
;; labeling of the horizontal axis and legend.
;; The following list shows all parameters with their default
;; values:
;; <pre>
;; ; mandatory and preset with default values
;; plot:wwidth        640 ; window width in pixels
;; plot:wheight       420 ; window height in pixels
;; plot:origin-x       64 ; top left x of plot area
;; plot:origin-y       64 ; top left y of plot area
;; plot:pwidth        520 ; width of plot area
;; plot:pheight       280 ; height of plot area<br/>
;; ; optional
;; plot:data-min      nil ; minimum value on the Y axis 
;; plot:data-max      nil ; maximum value on the Y axis 
;; plot:title         nil ; top centered main title
;; plot:sub-title     nil ; sub title under main title
;; plot:unit-y        nil ; a unit label centered left to the Y axis
;; plot:labels        nil ; a list of string labels for vertical grid
;; plot:legend        nil ; a list of string labels<br/>
;; ; optional for plotXY only
;; plot:data-x-min    nil ; minimum value on the X axis 
;; plot:data-x-max    nil ; maximum value on the X axis 
;; plot:data-y-min    nil ; minimum value on the Y axis 
;; plot:data-y-max    nil ; maximum value on the Y axis 
;; plot:unit-x        nil ; a unit label centered under the X axis
;; </pre>
;;
;; Only the the first group of variables is mandatory and preset to
;; the values shown above. Options in the second group will be either
;; suppressed or set automatically.

;; @syntax (plot <list-data> [<list-data> . . . ])
;; @param <list-data> One to five lists of data points.
;; <br>
;; The function draws one or more horizontal data lines from up
;; to five data sets in <list-data>. Colors are chosen in the sequence
;; red, green, blue, yellow and purple.
;; <br><br>
;; The following example doesn't set any extra options and plots to random
;; data sets. Scale and labels on the vertical and horizontal axis
;; will be set automatically. No title, sub-title and legends will
;; be printed. 
;;
;; @example
;; (plot (random 10 5 50) (normal 10 0.5 50))

;; The following example sets several options then plots and 
;; exports the image to a PNG graphics file:
;;
;; @example
;; (set 'plot:title "The Example Plot")
;; (set 'plot:sub-title "several random data sets")
;; (set 'plot:labels '("1" "" "20" "" "40" "" "60" "" "80" "" "100"))
;; (set 'plot:legend  '("first" "second" "third" "fourth"))
;;
;; ; display plot image
;; (plot (random 10 5 100) 
;;       (normal 10 0.5 100)
;;       (normal 8 2 100) 
;;       (normal 5 1 100) )
;;
;; ; save the displayed image to a file
;; (plot:export "example-plot.png")
;; 

;; @syntax (plot:XY <list-data-X> <list-data-Y>)
;; @param <list-data-X> List of X coordinates of data points.
;; @param <list-data-X> List of Y coordinates of data points.
;; <br>
;; Draws data points of data in <list-data-X> and <list-data-Y>.
;;

;; @syntax (plot:export <str-file-name>)
;; @param <str-file-name> The name of the file.
;;
;; Exports the current plot shown to a file in PNG format.
;;
;; @example
;; (plot:export "example-plot.png")

;; See the example plot
;; @link http://newlisp.org/code/example-plot.png here .

;; @syntax (plot:reset)
;; <br>
;; Resets all optional labels and sizes to 'nil'.

(define (plot:export file-name)
    (gs:export file-name))


(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(context 'plot)

(set 'wwidth 640)
(set 'wheight 420)

(set 'origin-x 64)
(set 'origin-y 64)

(set 'pwidth 520)
(set 'pheight 280)

(set 'title nil)
(set 'sub-title nil)

(set 'unit-x nil)
(set 'unit-y nil)
(set 'data-min nil)
(set 'data-max nil)
(set 'labels nil)
(set 'legend nil)

; colors
(set 'background-color '(0.3 0.3 0.3))
(set 'text-color '(0.9 0.9 0.9))
(set 'grid-color '(0.5 0.5 0.5))
(set 'frame-color '(0.7 0.7 0.7))

(set 'line-color (array 5 '(
    (1.0 0.35 0.35) ; 0 red
    (0.0 0.9 0.0)   ; 1 green
    (0.6 0.6 1.0)   ; 2 blue
    (0.9 0.7 0.0)   ; 3 orange
    (0.9 0.3 0.9)   ; 4 purple
)))


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auxiliary functions ;;;;;;;;;;;;;;;;;;;;;;

; round to 3 digits precision
; calculate rounding digits from (sub max min)
; then apply to all numbers
(define (rnd num)
     (round num (- (log num 10) 2)))

; setup the main window and plot area
(define (setup-window)
    (gs:init) 
    ;; describe the GUI
    (gs:frame 'PlotWindow 100 100 wwidth wheight "Plot")
    (gs:set-resizable 'PlotWindow nil)
    (gs:canvas 'PlotCanvas)
    (gs:set-background 'PlotCanvas background-color)
    (gs:add-to 'PlotWindow 'PlotCanvas)
    (gs:set-translation  origin-x origin-y)

    (gs:set-visible 'PlotWindow true)
    ; draw title
    (when title 
        (gs:set-font 'PlotCanvas "Lucida Sans Typewriter Regular" 18 "bold")        
        (gs:draw-text 'title title 
            (/ (- pwidth (* (length title) 11)) 2) -34 text-color)
    )
    ; draw sub title
    (when sub-title
        (gs:set-font 'PlotCanvas "Lucida Sans Typewriter Regular" 12 "plain")       
        (gs:draw-text 'title sub-title 
            (/ (- pwidth (* (length sub-title) 7)) 2) -18 text-color)
    )
)

(define (draw-grid L)
    ; draw horizontal grid lines
    (gs:set-stroke 1.0)
    (for (i 1 4)
        (let (y (/ (* i pheight) 5) )
            (gs:draw-line 'Grid 0 y pwidth y grid-color))
    )

    (if labels
        (set 'L (- (length labels) 1))
        (unless L 
            (set 'L (if (< N 50) (- N 1) 10)))
    )

    ; draw vertical grid lines
    (gs:set-stroke 1.0)
    (for (i 1 (- L 1))
        (let (x (/ (* i pwidth) L) )
             (gs:draw-line 'Grid x 0 x pheight grid-color)
        )
    )

)

(define (draw-grid-frame)
    (gs:set-stroke 1.0)
    (gs:draw-rect 'GridFrame 0 0 pwidth pheight)
    (gs:color-tag 'GridFrame frame-color)
    (gs:set-stroke 5.0)
    (gs:draw-rect 'GridMask -3 -3 (+ pwidth 6) (+ pheight 6))
    (gs:color-tag 'GridMask background-color)
)


(define (draw-labels) ; only used for line plot
    ; draw labels for x-range under vertical grid lines
    (gs:set-font 'PlotCanvas "Monospaced" 12 "plain")
    (if labels
      (let (step (div pwidth (- (length labels) 1))
          cnt 0 )
         (dolist (t labels)
           (unless (empty? t)
             (gs:draw-text 'Grid t (int (sub (mul step cnt) (mul (length t) 3.5)))  
                (+ pheight 14) text-color))
           (inc cnt))
      )
      ; else if no labels
      (begin
        (gs:draw-text 'Grid (format "%5d" 1) -32 (+ pheight 15) text-color)
        (gs:draw-text 'Grid (format "%5d" N) (- pwidth 26) (+ pheight 15) text-color)
      )
    )
)

(define (draw-y-numbers range, y-format step)
    ; draw y labels as of y-range left to horizontal grid lines
    (gs:set-font 'PlotCanvas "Monospaced" 12 "plain")
    (set 'y-format (if 
            (< range 1)   "%8.3f"
            (< range 10)  "%8.2f"  
            (< range 100) "%8.1f"
            true            "%8.0f"))

    (set 'step (rnd (div range 5)))
    (dotimes (i 6)
        (let (y (- pheight -4 (* i (/ pheight 5))))
            (gs:draw-text 'Grid 
            (format y-format (add ymin (mul i step))) 
                   -64 y text-color))
    )
 
    ; draw unit label to left center of grid
    (when unit-y
        (gs:draw-text 'Grid unit-y -50 (+ (/ pheight 2) 4) text-color))
)

(define (draw-x-numbers n range xmin, x-format x-offset L step)
    ; draw numbers for x-range under vertical grid lines
    (gs:set-font 'PlotCanvas "Monospaced" 12 "plain")

    (set 'x-format (if 
            (< range 1)   "%8.3f"
            (< range 10)  "%8.2f"  
            (< range 100) "%8.1f"
            true            "%8.0f"))

    (set 'x-offset (if 
            (< range 1)   20
            (< range 10)  14  
            (< range 100)  8 
            true             2 ))


    (set 'L 10)
    (set 'step (rnd (div range L)))
    
    (for (i 0 (- L 1))
        (let ( x (+ (* i  (/ pwidth L)) x-offset) )
             (gs:draw-text 'Grid  
                (format x-format (add xmin (mul (+ i 1) step)))
                x (+ pheight 15) text-color)
        )
    )
    ; unit label
    (when unit-x
        (gs:draw-text 'Grid unit-x (- (/ pwidth 2) 30) (+ pheight 30) text-color))
)

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; user functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; take one or more data vectors (lists) as arguments
(define (plot:plot)
    (setup-window)
    ; set data min, max and range over all plot vectors
    (set 'M (length (args)))
    (set 'N 0)
    (dotimes (m M)
        (set 'data (args m) )
        (set 'N (max N (length data)))
        (unless (and data-min data-max)
            (if (= 0 m)
                (begin
                    (set 'ymin (apply min data))
                    (set 'ymax (apply max data)) )
                (begin
                    (set 'ymin (min ymin (apply min data)))
                    (set 'ymax (max ymax (apply max data))))
            )
        )
    )

    ; overwrite ymin and ymax if data-min/mx are defined
    (when (and data-min data-max)
        (set 'ymin data-min)
        (set 'ymax data-max))

    (set 'y-range (sub ymax ymin))
    (draw-grid)
    (draw-labels)
    (draw-y-numbers y-range)

    ; for each data vector draw data
    (gs:set-stroke 1.5)
    (dotimes (m M)
        (set 'data (args m))
        ; draw data
        (set 'last-x nil 'last-y nil)
        (dotimes (i (length data))
            (let (x (div (mul i pwidth) (- N 1)) 
                  y (div (mul (sub (pop data) ymin) pheight) y-range))
                (when last-x 
                    (gs:draw-line 'seg last-x (- pheight last-y) 
                        (int x) (int (- pheight y)) (line-color m)))
                (set 'last-x (int x) 'last-y (int y))
            )
        )
    )

    ; draw legend depending on the number of plot lines
    ; style below 
    (dotimes (m M)
        (let (x (* m (/ pwidth 5)))
            (when (and (list? legend) (= M (length  legend))) 
                (gs:draw-line 'legend x (+ pheight 30) (+ x 25) (+ pheight 30) (line-color m))
                (gs:draw-text 'legend (legend m) (+ x 1 30) (+ pheight 33) text-color)
            )
        )
    )

    (draw-grid-frame)
    (gs:set-visible 'PlotWindow true)
)

(define (plot:XY data-x data-y (idx-color 3) , N, xmin, xmax, ymin, ymax)
    ; set data min, max and range over the two data vectors
    (set 'N (length data-x))
    (when (!= N (length data-y))
        (throw-error "X vecor and Y vector must be of equal length"))

    (setup-window)

    (set 'xmin (apply min data-x))
    (set 'xmax (apply max data-x)) 
    (set 'ymin (apply min data-y))
    (set 'ymax (apply max data-y)) 

    (when (and data-x-min data-x-max)
        (set 'xmin data-x-min)
        (set 'xmax data-x-max))
    
    (when (and data-y-min data-y-max)
        (set 'ymin data-y-min)
        (set 'ymax data-y-max))

    (set 'x-range (sub xmax xmin))
    (set 'y-range (sub ymax ymin))

    (draw-grid 10)
    (draw-y-numbers y-range)
    (draw-x-numbers N x-range xmin)

    ; draw XY data points in (line-color idx-color)
    (gs:set-stroke 1.5)
    (dotimes (i N)
        (let (x (div (mul (sub (pop data-x) xmin) pwidth) x-range)
              y (div (mul (sub (pop data-y) ymin) pheight) y-range))
            (if (and (< x pwidth) (< y pheight) (>= x 0) (>= y 0))
                (gs:draw-circle 'xy-points (int x) (int (- pheight y)) 3 (line-color idx-color)))
        )
    )

    (draw-grid-frame)
    (gs:set-visible 'PlotWindow true)
)

(define (plot:reset)
    (setq
        data-min      nil ; minimum value on the Y axis 
        data-max      nil ; maximum value on the Y axis 
        title         nil ; top centered main title
        sub-title     nil ; sub title under main title<br>
        unit-y        nil ; a unit label centered left to the Y axis
        labels        nil ; a list of string labels for vertical grid
        legend        nil ; a list of string labels
        data-x-min    nil ; minimum value on the X axis 
        data-x-max    nil ; maximum value on the X axis 
        data-y-min    nil ; minimum value on the Y axis 
        data-y-max    nil ; maximum value on the Y axis 
        unit-x        nil ; a unit label centered under the X axis
    )
)
;; </pre>

(context MAIN)

; test
(define (test-plot)
    ; optional title, sub-title, labels and legend, data min/max for Y
    (set 'plot:title "The Example Line Plot")
    (set 'plot:sub-title "several random data sets")
    (set 'plot:labels '("1" "" "20" "" "40" "" "60" "" "80" "" "100"))
    (set 'plot:legend  '("first" "second" "third" "fourth"))
    ;(set 'plot:data-min 0 'plot:data-max 20)

    (set 'plot:unit-y "unit y")
    ; display plot - only required statement
    (plot (random 10 5 100) 
          (normal 10 0.5 100)
          (normal 8 2 100) 
          (normal 5 1 100) )

    ; optionally save the display to a file
    (plot:export "example-line-plot.png")

    ; test XY plot
    (plot:reset)
    (set 'plot:title "The Example XY Plot")
    (set 'plot:sub-title "1000 normal distributed data points")
    (set 'plot:unit-x "unit x")

    (plot:XY (normal 10 3 1000) (normal 0 1 1000) )
    (plot:export "example-xy-plot.png")

)


;(plot (random 10 5 50) (normal 10 0.5 50))

;(test-plot)
;(exit)

