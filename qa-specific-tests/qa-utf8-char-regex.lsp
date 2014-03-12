#!/usr/bin/newlisp

(define (utf8-char-tests) 
  (and
    (= (char "킘") 0xd098)
    (= "킘" (char 0xd098))
    (= (char "퀿") 0xd03f)
    (= "퀿" (char 0xd03f))
    (= (char "Ω") 937)
    (= "Ω" (char 937))
    (= (char "Φ") 934)
    (= "Φ" (char 934))
))

(define (utf8-regex-tests)
  (and
    (= (regex "킘" "킘") '("킘" 0 3))
    (= (regex "퀿" "퀿") '("퀿" 0 3))
    (= (regex "Ω" "ΦabcΩdef") '("Ω" 5 2))
    (= (regex "Φ" "ΩabcΦdef") '("Φ" 5 2))
))

(if (utf8-char-tests)
    (println ">>>>> utf8-char-tests SUCESSFUL")
    (println ">>>>> ERROR in utf8-char-tests")
    )


(if (utf8-regex-tests)
    (println ">>>>> utf8-regex-tests SUCESSFUL")
    (println ">>>>> ERROR in utf8-regex-tests")
    )


(exit)

