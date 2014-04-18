#!/usr/bin/env newlisp

# - syntax.cgi - this utility formats a newLISP source file as an HTML 
# file with syntax highlighting
#
# comment following line out when not using as CGI utilty
# for CGI files to convert which are local must end with .txt

# v.1.7 - switch for using as commandline or cgi utility
# v.1.9 - fixed highlighting problem with escaped quotes
# v.2.0 - fixed \r\n translation
# v.2.1 - more compatible CGI
# v.2.3 - changed syntax for write-line
# v.2.4 - added handling of < ? ... > XML tag
# v.2.5 - generate more compliant HTML 4.01 transitional
# v.2.6 - handle number scientific format with e
# v.2.7 - improved keyword regex
# v.2.8 - scientific format with E
# v.2.9 - support for newLISPdoc tag color
# v.3.0 - correctly highlight & and ^ keywords
# v.3.1 - change additional 'nil' arg in 'starts-with' to 1
# v.3.2 - replace 'name' with 'term' and make compatible with older versions
# v.3.3 - added utf-8 meta tag in header
# v.3.4 - fontsize 150% for iPad or iPhone
#
# formats newLISP source files with syntax highlighting in HTML
#
# use on the command line or as cgi file together with a webserver
# 
# EXAMPLE command line (set 'cgi-use nil):
#
#    ./syntaxi.cgi mysource.lsp > mysource.lsp.html
#
# formats mysorce.lsp and redirects output to a new file mysource.lsp.html
#
# EXAMPLE webserver CGI (tested on Apache) local files must end in txt for security
#
#    http://www.mysite.com/syntax.cgi?mysource.lsp.txt
#
# returns mysorce.lsp HTML formatted to the requesting client (browser)
#
# EXAMPLE webserver CGI with other site URL
#
#    http://www.mysite.com/syntax.cgi?http://othersite/afile.lsp
#
# displays afile.lsp formateed from other site
#
# the following situations are not handled correctly:
#    - nested curly braces for strings like {abd{def}ghi}
#    - multiline quoted strings, use [text] [/text] instead
#    - multiline braced strings, use [text] [/text] instead
#    - comments starting with # but not starting at beginning of line
#      use ; as comment starter when comment appears after code


(define cgi-use true)

; make compatible with versions prior to 10.1.11
(when (< (sys-info -2) 10111)
    (constant (global 'term) name))

(when cgi-use 
	(print "Content-Type: text/html\r\n\r\n")
	(set 'iPad (find "iPad" (env "HTTP_USER_AGENT") 1))
	(set 'iPhone (find "iPhone" (env "HTTP_USER_AGENT") 1))
)
		

(define keyword-color "#0000AA")      ; newLISP keywords
(define tag-color "#308080")          ; newLISPdoc tags
(define string-color "#008800")       ; single line quoted and braced strings
(define long-string-color "#008800")  ; multiline for [text], [/text] tags
(define paren-color "#AA0000")        ; parenthesis
(define comment-color "#555555")      ; comments
(define number-color "#665500")       ; numbers

(define function-name-color "#000088")    ; not implemented yet for func in (define (func x y z) ...)

(set 'keywords (map term (filter (fn (x) (primitive? (eval x))) (sort (symbols) > ))))
(push "nil" keywords)
(push "true" keywords)
(set 'keyword-regex (join keywords "|"))
(replace "&" keyword-regex "&amp&")
(replace "?" keyword-regex "\\?")
(replace "^" keyword-regex "\\^")
(replace "$" keyword-regex "\\$")
(replace "!" keyword-regex "\\!")
(replace "+" keyword-regex "\\+")
(replace "*" keyword-regex "\\*")
(replace "||" keyword-regex "|\\|")
(set 'keyword-regex (append {(\G|\s+|\(|\))(} keyword-regex {)(\s+|\(|\))}))

(set 'file (if cgi-use 
	(or (read-line) (env "QUERY_STRING"))
	(main-args 2)))

(set 'title file)

(when cgi-use
    (when (and (not (starts-with file "http://" 1)) (not (ends-with file ".txt")))
        (println "<h3>File not allowed for viewing: " file "</h3>")
        (exit))
)


(if (starts-with file "http://" 1)
	(set 'file (get-url file 10000))
	(set 'file (read-file file )))

(unless file
    (println "<h3>Cannot find file</h3>")
    (exit))

(define (clean-comment str)
	(replace {<font color='#......'>} str "" 0)
	(replace {</font>} str "")
	(replace {[text]} str "&#091&text]")
	(replace {[/text]} str "&#091&/text]")
)

(define (format-quoted-string str)
	(replace {<font color='#......'>} str "" 0)
	(replace {</font>} str "")
	(replace ";" str "&#059&")
	(replace "{" str "&#123&")
	(replace "}" str "&#125&")
	(replace {\} str "&#092&")
	(replace {[text]} str "&#091&text]")
	(replace {[/text]} str "&#091&/text]")
	(format {<font color='%s'>%s</font>} string-color str)
)

(define (format-braced-string str)
	(replace {<font color='#......'>} str "" 0)
	(replace {</font>} str "")
	(replace ";" str "&#059&")
	(replace {"} str "&#034&")
	(replace {[text]} str "&#091&text]")
	(replace {[/text]} str "&#091&/text]")
	(format {<font color='%s'>%s</font>} string-color str)
)

(define (format-tagged-string str)
	(replace {<font color='#......'>} str "" 0)
	(replace {</font>} str "")
	(replace ";" str "&#059&")
	(format {<font color='%s'>%s</font>} string-color str)
)

; replace special HTML characters
(replace "\r\n" file "\n")
(replace "&" file "&amp&")
(replace "<(\\w)" file (append "&lt&" $1) 0)
(replace "(\\w)>" file (append $1 "&gt&") 0)
(replace "/>" file "/&gt&" 0)
(replace "</" file "&lt&/" 0)
(replace "<!" file "&lt&!" 0)
(replace "<\\?" file "&lt&?" 0)
; replace escaped quotes
(replace  "\092\034"  file "&#092&&#034&")

; color keywords
(replace keyword-regex file (format {%s<font color='%s'>%s</font>%s} $1 keyword-color $2 $3) 0)
;(replace keyword-regex file (println "->" $0 "<-<br>") 0)
;(exit)

; color numbers
(replace  
;   <-- lead --><---- hex ---->|<- oct ->|<------- decimal ----- and ----- scientific -->
	{(\s+|\(|\))(0x[0-9a-fA-F]+|[+-]?0\d+|([+-]?(0|[1-9]\d*)(\.\d*)?|\.\d+)([eE][+-]?\d+)?)} file 
         (format {%s<font color='%s'>%s</font>} $1 number-color $2) 0)


; color parens
(replace "(" file (format {<font color='%s'>(</font>} paren-color))
(replace ")" file (format {<font color='%s'>)</font>} paren-color))

; color braced strings
(replace "{.*?}" file (format-braced-string $0) 0) ; no multiline string
; color quoted strings
(replace {".*?"} file (format-quoted-string $0) 0) ; no multiline strings

; color ;  comments
(replace ";.*" file (clean-comment $0) 0)
(replace ";.*" file (format {<font color='%s'>%s</font>} comment-color $0) 0)
(replace "(;;.*)(@.*)(\\s.*\n)" file (append $1 (format {<font color='%s'>%s</font>} tag-color $2) $3) 512)

; color # comments
(set 'buff "")
(dolist (lne (parse file "\n"))
	(replace "^\\s*#.*" lne  (clean-comment $0) 0)
	(replace "^\\s*#.*" lne (format {<font color='%s'>%s</font>} comment-color $0) 0)
	(if (< (sys-info -2) 9909) 
		(write-line lne buff)
		(write-line buff lne)))
(set 'file buff)

; color tagged strings
(replace {\[text\].*?\[/text\]} file (format-tagged-string $0) 4) ; handles multiline strings

; xlate back special characters
(replace "&amp&" file "&amp;")   ; ampersand
(replace "&lt&" file "&lt;")     ; less
(replace "&gt&" file "&gt;")     ; greater
(replace {&#034&} file "&#034;") ; quotes
(replace {&#059&} file "&#059;") ; semicolon
(replace {&#123&} file "&#123;") ; left curly brace
(replace {&#125&} file "&#125;") ; right curly brace
(replace {&#091&} file "&#091;") ; left bracket
(replace {&#092&} file "&#092;") ; back slash

; add pre and post tags
(println (append 
	{<!DOCTYPE HTML PUBLIC "4.01 Transitional">}
	"<html><head><title>" title "</title>\n"
	{<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />}
	{<style type="text/css">}
	(if (or iPad iPhone)
		{pre {font-size: 150%}}
		{pre {font-size: 100%}} )
	"</style></head><body>\n<pre>\n" 
	file "\n</pre>" 
))

(println {<center><font face='Arial' size='-2' color='#444444'>}
         {syntax highlighting with <a href="http://newlisp.org">newLISP</a>&nbsp;}
         {and <a href="http://newlisp.org/syntax.cgi?code/syntax-cgi.txt">syntax.cgi</a>}
         {</font></center>})

(println {</body></html>})

(exit)

;; eof

