#!/usr/bin/newlisp

(load (append (env "NEWLISPDIR") "/guiserver.lsp"))
(gs:init)

(gs:midi-init)

;(gs:get-instruments)

; set up keys and volume
(map set '(C C# D D# E F F# G G# A A# B c c# d e f f# g g# a a# b) (sequence 60 82))
(set 'pp 30 'p 40 'm 64 'f 127) ; set velocity/volume

; attach instruments to channels
(gs:midi-patch "Piano" 0)
(gs:midi-patch "Pizzicato Strings" 1)
(gs:midi-patch "Woodblock" 2)

; modify speed before creating tracks
;(gs:midi-bpm 140) ; speed up from 120 default

; add tracks to sequence
(gs:add-track 0 '( (C 24 m) (C# 8 m) (D 32 m) (c 32 f) (D 32 m)) )
(gs:add-track 1 (dup '(d 8 pp) 16))
(gs:add-track 2 '( (c 8 p) (c 24 p) (c 8 p) (c 24 p) (c 8 p) (c 24 p)  (c 8 p) (c 24 p)) )

; play 
(gs:play-sequence)
;(gs:save-sequence "/Users/lutz/Desktop/midi2-demo.mid")

; wait until play has finished
(sleep 6000)
(gs:midi-close)

(exit)


