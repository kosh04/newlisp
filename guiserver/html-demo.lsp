#!/usr/bin/newlisp
;;
;; html-demo.lsp - demonstrate  the text pane with HTML

;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

;;;; describe the GUI
(gs:frame 'HtmlDemo 100 100 400 270 "HTML text-pane demo")
(gs:text-pane 'HtmlPane 'htmlpane-action "text/html" 200 500)
(gs:set-editable 'HtmlPane nil)
(gs:set-tab-size 'HtmlPane 4)
(gs:set-font 'HtmlPane "Monospaced" 14 "plain")
(gs:set-background 'HtmlPane 1 1 0.96)

(if (= ostype "Windows")
	(set 'url (string "file:///" (env "NEWLISPDIR") "/guiserver/html-demo.lsp"))
	(set 'url "file:///usr/share/newlisp/guiserver/html-demo.lsp")
)

(set 'page (format [text]
<html>
<center><br><h2>Text panes for HTML</h2></center>
<blockquote>
<p>This is a page of <i>html</i> text with a clickable hyperlink 
<a href="%s">html-demo.lsp</a>.</p>

<p>The previous link expects the file <tt>html-demo.lsp</tt> in 
the current directory from where the program was started.</p>

<p>Only simple pages are processed correctly.</p>

</blockquote>
</html>
[/text] url))

(gs:set-text 'HtmlPane page)
(gs:append-text 'HtmlPane "This is appended text")
(gs:add-to 'HtmlDemo 'HtmlPane)
(gs:set-visible 'HtmlDemo true)

;;;; define actions
(define (urlfield-action id txt)
)

(define (htmlpane-action id text)
)

;;;; listen for incoming action requests and dispatch
(gs:listen)

;; eof

