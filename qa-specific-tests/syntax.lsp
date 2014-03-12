# test code # also starts comments like ;
# newLISP uses "," {,} and [text],[/text] as delimiters
(dotimes (i 10) (println "Hello World"))
; this is also a comment
(dotimes (i 10) (println "Hello World")) # comment

; numbers
+12345 -1.234 0xABCdef 0345 1e2 1.23E-5 0b10101

; colon is operator but can be attached
; separation
(set 'Foo:foo 123)

; no escaping necessary but balance inside {,}
{kjhj"khj{lkhl}jkhljkh} 

; [,] are just normal chars but show match
[kjhjklhlj]khljhljkh] ; looks like any symbol

(dotimes (i 10) (println "Hello World")) ; comment

[text]
this is a
[/text
{"{{}
\"multi line
text
[/text] ; *everything* can go in text tags

(dotimes (i 10) (println "Hello World"))

{jljkh"[text][/text] {}jhkkjhkjh}

; the end ;

