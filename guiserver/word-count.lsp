;; demo script: word-count.lsp
;; this script is registered in the Tool menu of newlisp-edit.lsp

(set 'content (read-file (main-args 2)))

(println 
	"Words: " (length (find-all "\\w+" content))
  " Bytes: " ((file-info (main-args 2)) 0))

(exit)

