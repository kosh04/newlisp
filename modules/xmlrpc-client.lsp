;; @module xmlrpc-client.lsp 
;; @description XMLRPC protocol client routines
;; @version 0.3 - comments redone for automatic documentation
;; @version 0.4 - multiple indices with <tt>nth</tt> redone to be compatible with future versions
;; @version 0.5 - doc changes
;; @version 0.6 - fixed bug in error handler
;; @version 0.61 - fixed doc typo
;; @version 0.7 - check for valid list type in (get-value expr) thanks Kosh
;; @version 0.8 - changed references to /usr/ to /usr/local/
;; @version 0.9 - make system.listMethods more flexible for no of args - thanks Ofoe
;; @author Lutz Mueller 2005-2011, Kosh 2012, Oofoe 2016
;;
;; <h2>Functions for XML-RPC client</h2>
;; To use this module include a 'load' statement at the beginning of the program:
;; <pre>
;; (load "/usr/local/share/newlisp/modules/xmlrpc-client.lsp")
;; ; or shorter
;; (module "xmlrpc-client.lsp")
;; </pre>
;; The script 'xmlrpc.cgi' implements a method 'newLISP.evalString'. This  module contains 
;; a client side function for this method for testing purposes. The file 'xmlrpc.cgi'
;; can be found in the 'example' directory of the newLISP source distribution.
;; 
;; For further information on XML-RPC consult 
;; @link http://www.xmlrpc.com/ http://www.xmlrpc.com/ .
;;
;; Whenever a connection could be made, method functions will return a response
;; formatted by the XML-RPC server in XML. If a connection failed the function will
;; return 'nil' and a call to '(XMLRPC:error)' will return an error text.
;;
;; If the XML received cannot be parsed into SXML, the function returns 'nil'
;; and '(XMLRPC:error)' will return an XML error. SXML is XML transformed into
;; LISP S-expressions.

;; If the XML received is syntactically correct but not correctly formatted,
;; XML garbage is returned or 'nil' is returned and an error message in 
;; '(XMLRPC:error)'.

;; @syntax (XMLRPC:system.listMethods <str-url>)                    
;; @param <str-url> The URL of the XML-RPC server
;; @return A list of methods supported.
;; The server at <url> returns a list of methods supported.

;; @syntax (XMLRPC:system.methodHelp <str-url> <str-method-name>)        
;; @param <str-url> The URL of the XML-RPC server.
;; @param <method-name> The name of the method to get help for.
;; @return Help for <str-method-name>
;; The server at <str-url> returns help for the method in <str-method-name>

;; @syntax (XMLRPC:system.methodSignatures <str-url> <str-method-name>) 
;; @param <str-url> The URL of the XML-RPC server.
;; @param <method-name> The name of the method to get the signature for.
;; @return The signature for a server method.
;; Gets the calling parameter conventions (signature) for a method
;; <method-name> at <str-url>.

;; @syntax (XMLRPC:execute <str-url> <str-xml-request>)
;; @param <str-url> The URL of the XML-RPC server.
;; @param <str-xml-request> A XML formatted request.
;; @return XML formatted server response
;; This is a generic method for making XML-RPC requests.
;; The request must be XML formatted correctly by the sender (client).


;; @syntax (XMLRPC:newLISP.evalString <str-url> <str-expression>)
;; @param <str-url> The URL of the XML-RPC server.
;; @param <str-expression> The expresssion to be evaluated in a string.
;; @return The result of the expression evaluation.
;; The expression in <str-expression> is encoded in base64 and then
;; transmitted to the remote server.

;; @syntax (XMLRPC:error)
;; @return Error text of last error occured.

(context 'XMLRPC)

(set 'request
[text]<?xml version="1.0"?>
<methodCall>
   <methodName>%s</methodName>
   <params>
      <param>
         <value>%s</value>
       </param>
    </params>
</methodCall>
[/text])

(set 'error-msg "")


######### extract value(s) from XML-RPC response XML with <params> #############

; get result data from result structure
;
(define (get-result-data xml)
    (if (starts-with xml "ERR:") 
        (begin
            (set 'error-msg xml)
            (throw nil)))
    (xml-type-tags nil nil nil nil)
    (set 'sxml (xml-parse xml (+ 1 2 4)))   
    (if (not sxml) (throw (format "XML error: %s" (first (xml-error)))))

    (if (match '(("methodResponse" ("fault" *))) sxml) 
        (begin
            (set 'error-msg 
                (let (fault (sxml 0 1 1 1 1 2 1 1) 
                      text (sxml 0 1 1 1 2 2 1 1))
                     (append "Fault " fault ": " text)))
                (throw nil)))

    (get-value (sxml 0 1 1 1)))
   

; get contents from expr = (value ...)
;
(define (get-value expr)
    (if 
        (empty? expr) nil
        
        (list? (expr 1))
        (case (expr 1 0)
            ("i4" (int (expr 1 1)))
            ("int" (int (expr 1 1)))
            ("boolean" (if (= "0" (expr 1 1) ) nil true))
            ("double" (float (expr 1 1)))
            ("base64" (base64-dec (expr 1 1)))
            ("dateTime.iso8601" (expr 1 1))
            ("array" (if (= (expr 1) "array") 
                         "array" ;; if untagged string "array"
                         (get-array (rest (expr 1 1)))) )
            ("struct" (get-struct (rest (expr 1))))
            ("string" (expr 1 1))
            (true (expr 1))
        ) ; end case
        
        true (string (expr 1))
    ) ; end if
) 

; get contents from expr = ((value ...) (value ...) ...)
;
(define (get-array expr)
    (if (empty? expr) 
        '()
        (cons (get-value (first expr)) (get-array (rest expr)))))


; get contents from expr = ((member ...) (member) ...)
;
(define (get-struct expr)
    (if (empty? expr)
        '()
        (cons (get-member (first expr)) (get-struct (rest expr)))))


; get contents from expr = (member ...)
;
(define (get-member expr)
    (list (expr 1 1)  (get-value (last expr))))


################################ standard system methods #######################

# convert to SXML
(xml-type-tags nil nil nil nil)

# ( method /arg.../ -- XML) Compose XML request.
(define (format-request method)
  (let ((xml (format
             "<?xml version=\"1.0\"?><methodCall><methodName>%s</methodName><params>"
             method)))
    (dolist (value (args))
      (push (format "<param><value>%s</value></param>" value) xml -1))
    (push "</params></methodCall>\n" xml -1)))

# return method names in a list of strings
#
# (XMLRPC:system.listMethods <url>)  
#
(define (system.listMethods url)
    (execute url (format-request "system.listMethods")))

# get help for a methodName at url
# return help in a string
#
# (XMLRPC:system.methodHelp <url> <method-name) 
#
(define (system.methodHelp url methodName)
    (execute url (format request "system.methodHelp" methodName) ))


# get method signatures of methodName at url
# return ans array of strings
#
# (XMLRPC:system.methodSignatures <url> <method-name>) 
#
(define (system.methodSignature url methodName)
    (execute url (format request "system.methodSignature" methodName) ))


(define (error) error-msg)


# Execute a method on url with XML formatted request
#
# This is a generic method, but with XML formatted by caller.
#
# (XMLRPC:execute <url> <xml-request>)
#
(define (execute url parameter-XML)
    (if (not (catch (begin
        (set 'error-msg "")
        (set 'xml (post-url url parameter-XML "text/xml"))
        (get-result-data xml)) 'result))

        (begin (set 'error-msg "Wrong format in XML-RPC") nil)
        result))

######################### newLISP XML-RPC specific methods #####################

# evaluate a newLISP expression in str at newLISP XML-RPC server at url
# return evaluation result in a string 
#

(define (newLISP.evalString url str)
    (execute url 
        (format request 
                "newLISP.evalString" (append "<base64>" (base64-enc str) "</base64>")))
)

(context MAIN)

# eof

