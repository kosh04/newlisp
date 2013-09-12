#!/usr/bin/newlisp

;; @module json.lsp
;; @description JSON to S-expression parser
;; @author L.M. March 2010, Jan 2011, Ted Walther 2011
;; @version v. 0.1  - initial release
;; @version v. 0.21 - bug fixes for long string objects and  empty token lists
;; @version v. 0.30 - now processes backslashed chars as of JSON spec
;;
;; The module defines a function <tt>jason2expr</tt> translating
;; @link http://json.org JSON
;; into Lisp S-expressions.

;; @syntax (json2expr <str-json-data>)
;; @params <str-json-data> The JSON formatted data string.
;; Only one JSON expression should be passed to the function.
;; @return The S-expression
;; @example
;;(setq json-text [text]
;; {
;;      "firstName": "John",
;;      "lastName": "Smith",
;;      "address": {
;;          "streetAddress": "21 2nd Street",
;;          "city": "New York",
;;          "state": "NY",
;;          "postalCode": "10021"
;;      },
;;      "phoneNumbers": [
;;          { "type": "home", "number": "212 555-1234" },
;;          { "type": "fax", "number": "646 555-4567" }
;;      ]
;;  }
;; [/text])
;;
;; ; parse json-text to s-expression
;; (json2expr json-text) =>
;;
;; (("firstName" "John") 
;;  ("lastName" "Smith") 
;;  ("address" (
;;     ("streetAddress" "21 2nd Street") 
;;     ("city" "New York") 
;;     ("state" "NY") 
;;     ("postalCode" "10021")))
;;  ("phoneNumbers" (
;;     (("type" "home") ("number" "212 555-1234")) 
;;     (("type" "fax") ("number" "646 555-4567")))))
;;
;;

(context 'json2expr)

; regex pattern for {,},[,],: and , (comma) and strings, and numbers
(constant 'json-pattern 
    {"(\\"|[^"])*"|true|false|null|\{|\}|\[|\]|:|,|([+-]?(0|[1-9]\d*)(\.\d*)?|\.\d+)([eE][+-]?\d+)?})

; strings are limited with quotes
(define (is-string tkn) 
  (regex {^".*"$} tkn))

; numbers can be integers or floats in decimal or scientific notation 
(define (is-number tkn)
  (regex {^([+-]?(0|[1-9]\d*)(\.\d*)?|\.\d+)([eE][+-]?\d+)?$} tkn))

(define (json2expr:json2expr str-json)
  (set 'tokens (find-all json-pattern str-json))
  (tokens2expr tokens)
)

(define (jstr2str str-json)
  (replace {\\([btnfr"/\\]|u[0-9a-fA-F]{4})} str-json
    (case ($0 1)
      ({"} {"}) ({\} {\}) ({/} {/})
      ({b} (char 8))                            ; backspace
      ({t} (char 9))                            ; horizontal tab
      ({n} (char 10))                           ; newline
      ({f} (char 12))                           ; formfeed
      ({r} (char 13))                           ; carriage return
      ({u} (char (int (string "0x" (2 5 $0))))) ; unicode
      (true (string "json2expr: INVALID STRING ESCAPE" $0))
    )
    0)
)

; the parser is recursively called for arrays and objects
(define (tokens2expr)
  (let ((tkn (pop tokens)) (expr '()))
    (cond 
      ((is-string tkn) (jstr2str (1 -1 tkn)))

      ((is-number tkn) (eval-string tkn))

      ((= "true" tkn) 'true)

      ((= "false" tkn) 'false)

      ((= "null" tkn) 'nil)

      ((= tkn "{") 
        (while (!= (setq tkn (pop tokens)) "}")
          (if (and (is-string tkn) (= ":" (pop tokens)))
            (push (list (jstr2str (1 -1 tkn)) (tokens2expr)) expr -1))
          (if (and tokens (= "," (first tokens))) (pop tokens))
          (unless tokens (throw-error "unfinished JSON object")))
        expr)

      ((= tkn "[")
        (while (!= "]" (first tokens))
          (push (tokens2expr) expr -1)
          (if (and tokens (= "," (first tokens))) (pop tokens))
          (unless tokens (throw-error "unfinished JSON array")))
        (pop tokens)
        expr)
			
      (true (throw-error (string "wrong JSON syntax:" tkn)))
    )
))	

(context MAIN)

; eof

