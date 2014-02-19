#!/usr/bin/newlisp

; table-demo.lsp - demo of the table UI

(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

; initialization
(gs:init)
;(gs:set-trace true)

; the main window frame
(gs:frame 'Frame 100 100 400 500 "Table")
(gs:set-grid-layout 'Frame 2 1 10 5)

; the table 
(gs:table 'Table 'action-handler "col1")
(gs:table-add-column 'Table "col2")
(gs:table-set-column 'Table 1 80 "right")

(gs:table-add-row 'Table "r0 c0" "r0 c1")
(gs:table-add-row 'Table "r1 c0" "r1 c1")
(gs:table-add-row 'Table "0" "1")

; buttons, cell contents, text area
(gs:panel 'ButtonPanel)
(gs:check-box 'rowNumber 'rownumber-action "row number")
(gs:button 'addRow 'addrow-action "add row")
(gs:button 'addCol 'addcol-action "add column")
(gs:button 'deleteRow 'deleterow-action "delete row 0")
(gs:button 'setCol 'delcol-action "set 3 columns")
(gs:button 'setRows 'setrows-action "10 rows")
(gs:text-field 'cellContents 'null-action 18)
(gs:set-editable 'cellContents nil)
(gs:add-to 'ButtonPanel 'rowNumber 'addRow 'addCol 'deleteRow 'setCol 'setRows 'cellContents)

(gs:text-area 'textArea 'null-action)
(gs:set-editable 'textArea nil)

(gs:panel 'Panel)
(gs:set-grid-layout 'Panel 2 1 10 5)
(gs:add-to 'Panel 'ButtonPanel 'textArea)

(gs:add-to 'Frame 'Table 'Panel)
(gs:set-visible 'Frame true)


(define (report-table-data id)
    (gs:table-get id)
	(gs:table-get-size id)
	(gs:set-text 'textArea (string "Table size(row,col) = " gs:table-size "\n"))
	(gs:append-text 'textArea (string "Table Value = " gs:table-full))
)

(define (action-handler id row col data)
	;(println "action-handler=>" id " " row " " col " " data)
    (gs:set-text 'cellContents data)
	(report-table-data id)
)

(define (addrow-action id)
	(gs:table-add-row 'Table)
	(report-table-data 'Table)
)

(define (deleterow-action id)
	(gs:table-remove-row 'Table 0)
	(report-table-data 'Table)
)


(define (addcol-action id)
	(gs:table-add-column 'Table "")
	(report-table-data 'Table)
)

(define (delcol-action id)
	(gs:table-set-column-name 'Table "first column" "second column" "third column")
	(report-table-data 'Table)
)

(define (setrows-action id)
	(gs:table-set-row-count 'Table 10)
	(report-table-data 'Table)
)


(define (rownumber-action id flag)
	(gs:table-show-row-number 'Table flag)
)

(define (null-action) nil)

(gs:listen)

(exit)

; eof
