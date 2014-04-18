#!/usr/bin/env newlisp
#
# xmlrpc.cgi - CGI script to handle XML-RPC requests
#
# This is similar to xmlrpc-server, but stateless as a new
# newLISP process is invoked everytime this script is executed.
# For a XML-RPC server maintaining state run xmlrpc-server.
#
# v.1.0 - 2005-01-14  Lutz Mueller
#
# v.1.1 - 2005-03-20
#	method name for newLISP.evalString was listed wrong
# v.1.2 - 2010-02-09
#	method name for newLISP.evalString was listed wrong
# v.1.3  - 2010-10-07
#	replaced obsolete 'error-text' with 'last-error'
# v.1.4  - 2012-03-16 bugfixes, thanks Kosh
#
# supports the following methods:
#
#    Method                   Return     Parameter
#    ------                   ------     ---------
#    system.listMethods       string     n/a
#    system.methodHelp        string     string
#    system.methodSignature   array      string
#    newLISP.evalString       base64     base64
#
#

(set 'version "1.4")

# formatting templates for responses

(set 'normal-response
[text]<?xml version="1.0"?>
<methodResponse>
   <params>
      <param>
         <value>%s</value>
       </param>
    </params>
</methodResponse>
[/text])

(set 'fault-response
[text]<?xml version="1.0"?>
<methodResponse>
   <fault>
      <value>
         <struct>
            <member>
               <name>faultCode</name>
               <value><int>%d</int></value>
               </member>
            <member>
               <name>faultString</name>
               <value><string>%s</string></value>
            </member>
         </struct>
      </value>
   </fault>
</methodResponse>
[/text])


# event handler called when newLISP receives a request

(define (process-post request)
    (if (not (catch (handle request) 'page))
	(set 'page (format fault-response 0 page)))
    (print
            "Content-Type: text/xml\r\n"
            "Content-Length: " (length page) "\r\n\r\n"
            page))


(define (handle input, XML contentlength methodName params)
    (set 'XML "")
    (xml-type-tags nil nil nil nil)
    (if (not (set 'XML (xml-parse input (+ 1 2 4 8 16))))
        (begin
            (if (not (xml-error)) 
                (error 3 "No XML or XML is empty")
		(error 4 (append "XML error: " 
                                 (first (xml-error))))))

        (set 'XML (first XML)))

    ; get methodName and parameter section
    (set 'm (match '(methodCall (methodName *) *) XML))
    (if (not m)
        (error 5 "Invalid XML-RPC format"))

    (set 'methodName (first (first m)))
    (set 'params (last m))

    (case methodName
        ("newLISP.evalString" (newLISP.evalString params))
        ("system.listMethods" (system.listMethods))
        ("system.methodHelp" (system.methodHelp params))
        ("system.methodSignature" (system.methodSignature params))
        (true (error 6 "Method name not known")))
)

(define (error no msg)
    (throw (format fault-response no 
        (append "newLISP XML-RPC v." version " - " msg))))


######################### remote callable methods ##############################

(define (system.listMethods)
[text]<?xml version="1.0"?>
<methodResponse>
   <params>
      <param><value><array><data>
         <value><string>system.listMethods</string></value>
         <value><string>system.methodHelp</string></value>
         <value><string>system.methodSignature</string></value>
         <value><string>newLISP.evalString</string></value>
      </data></array></value></param>
    </params>
</methodResponse>
[/text])
    

(define (system.methodHelp params, methodName)
    (set 'methodName (params 0 1 1 1 1))
    (case methodName
        ("system.listMethods" (format normal-response "Lists all methods implemented."))
        ("system.methodHelp" (format normal-response "Documents a method."))
        ("system.methodSignature" (format normal-response "Shows the signatures of a method."))
        ("newLISP.evalString" (format normal-response "Evaluate a base64 encoded string."))
        (true (error 7 "Method name in system.methodHelp not known")))
)

(define (system.methodSignature params)
    (set 'methodName (params 0 1 1 1 1))
    (case methodName
        ("system.listMethods" (format normal-response 
"<array>
  <data>
   <value>
    <array>
     <data>
      <value>array</value>
     </data>
    </array>
   </value>
 </data>
</array>"))

        ("system.methodHelp" (format normal-response 
"<array>
  <data>
   <value>
    <array>
     <data>
      <value>string</value>
      <value>string</value>
     </data>
    </array>
   </value>
 </data>
</array>"))

	("system.methodSignature" (format normal-response 
"<array>
  <data>
   <value>
    <array>
     <data>
      <value>array</value>
      <value>string</value>
     </data>
    </array>
   </value>
 </data>
</array>"))

	("newLISP.evalString" (format normal-response 
"<array>
  <data>
   <value>
    <array>
     <data>
      <value>base64</value>
      <value>base64</value>
     </data>
    </array>
   </value>
 </data>
</array>"))

        (true (error 7 "Method name in system.methodSignature not known")))
)

(define (newLISP.evalString params, m, result)
    (set 'm (match '((params (param (value (base64 *))))) params))
    (if (not m) 
        (error 8 "Invalid format for method newLISP.evalString")
        (set 'result 
            (string (eval-string (base64-dec (first (first m))) MAIN (last (last-error)))))
    (format normal-response 
        (append "<base64>" (base64-enc result) "</base64>")) )
)
     

########################### MAIN ENTRY POINT #######################

(set 'input (read-line))

(if (not input)
    (print
        "Content-type: text/html\r\n\r\n"
        "<h2>newLISP XML-RPC v." version 
        ": not a valid XML-RPC request</h2>")
    (begin
        (while (read-line) (write input (current-line)))
        (process-post input))
)


(exit)


# eof
