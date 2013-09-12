;; @module json.lsp
;; @description JSON to S-expression parser
;; @author L.M. March 2010, Jan 2011
;; @version v. 0.1  - initial release
;; @version v. 0.21 - bug fix for string objects longer 2047 characters
;;                    and missing checks for empty token lists added
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
    {".*?"|true|false|null|\{|\}|\[|\]|:|,|([+-]?(0|[1-9]\d*)(\.\d*)?|\.\d+)([eE][+-]?\d+)?})

; strings are limited with quotes
(define (is-string tkn) 
  (regex {^".*?"$} tkn))

; numbers can be integers or floats in decimal or scientific notation 
(define (is-number tkn)
  (regex {^([+-]?(0|[1-9]\d*)(\.\d*)?|\.\d+)([eE][+-]?\d+)?$} tkn))

(define (json2expr:json2expr str-json)
    (set 'tokens (find-all json-pattern str-json))
    (tokens2expr tokens)
)

; the parser is recursively called for arrays and objects
(define (tokens2expr)
  (let ((tkn (pop tokens)) (expr '()))
    (cond 
      ((is-string tkn) (1 -1 tkn))

      ((is-number tkn) (eval-string tkn))

      ((= "true" tkn) 'true)

      ((= "false" tkn) 'false)

      ((= "null" tkn) 'null)


      ((= tkn "{") 
        (while (!= (setq tkn (pop tokens)) "}")
          (if (and (is-string tkn) (= ":" (pop tokens)))
            (push (list (eval-string tkn) (tokens2expr)) expr -1))
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
