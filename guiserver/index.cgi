#!/usr/bin/env newlisp

(print  "Content-type: text/html\r\n\r\n")
(println "<h2>" (real-path) "</h2>")

(println {<table>})
(dolist (file (sort (directory)))
    (set 'slash (if (directory? file) "/" ""))
    (println {<tr><td><tt><a href="http:} file slash {">} file  slash {</a>&nbsp;&nbsp;</tt></td><td><tt>}
        (date (last (file-info file)) 0 "%Y-%m-%d %H:%M:%S") {</tt></td>} 
        {<td><pre>} (format "%12.3f KB" (div (file-info file 0) 1000)) {</pre></td></tr>}))
(println {</table>})
(exit)

