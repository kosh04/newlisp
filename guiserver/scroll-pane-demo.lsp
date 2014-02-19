; Demo to show three extra parameters in gs:scroll-pane 
; you can add three extra components, first is used as column header,
; second as row header, third as componenet in upper left corner


(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 
(gs:init)

(gs:table 'col-table 'gs:no-action "Col Header 1" "Col Header 2" "Col Header 3")
(gs:table-add-row 'col-table '("newLisp" "Java" "Assembler"))
(gs:table-add-row 'col-table '("fun!" "work" "...")) 
(gs:set-size 'col-table 500 50)

(gs:table 'row-table 'gs:no-action "N" "char+65")
(dotimes (row 40) 
	(gs:table-add-row 'row-table (string row) (char (+ row 65))))
(gs:set-size 'row-table 200 540)

(gs:table 'data-table 'gs:no-action "N/10" "Sinus" "Cosinus")
(dotimes (row 40) 
	(gs:table-add-row 'data-table (string (div row 10)) (string (sin row)) (string (cos row))))
(gs:set-size 'data-table 500 540)

(gs:canvas 'Canvas)
(gs:set-background 'Canvas 0.98 0.98 .98)
(gs:button 'Hide 'Hide-action "Hide Headers" 120 18)
(gs:button 'Show 'Show-action "Show Headers" 120 18)
(gs:add-to 'Canvas 'Hide  'Show)

;new parameters for scroll-pane
(gs:scroll-pane 'scroll 'data-table 500 350 'col-table 'row-table 'Canvas)

(gs:frame 'Frame 100 100 500 350 "Scroll Pane Demo")
(gs:set-border-layout 'Frame)
(gs:add-to 'Frame 'scroll "center")
(gs:set-visible 'Frame true)

(define (Hide-action)
	(gs:table-set-column-name 'row-table "" "")
	(gs:table-set-column-name 'col-table "" "" "")
	(gs:table-set-column-name 'data-table "" "" "")
)

(define (Show-action)
	(gs:table-set-column-name 'row-table "Row Header 1" "Row Header 2")
	(gs:table-set-column-name 'col-table "Col Header 1" "Col Header 2" "Col Header 3")
	(gs:table-set-column-name 'data-table "Data Header 1" "Data Header 2" "Data Header 3")
)

(gs:listen)
(exit)

 


