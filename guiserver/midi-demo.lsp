#!/usr/bin/newlisp

;; - midi-demo.lsp -
;;
;; using gs:play-note in this demo only one channel can be played at the same time
;; to play several tracks/channels in parallel use gs:play-track as shown in
;; the file midi2-demo.lsp

(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

; initialize guiserver
(gs:init) 

; initialize MIDI subsystem
(gs:midi-init)
; get all instruments into the gs:instruments variable
; enable this to see a list of instruments in the terminal
;(gs:get-instruments)

; assign instruments to channels
(gs:midi-patch "Piano" 0)

;(gs:midi-bpm 240) ;double speed

; play chromatic scale on default instrument (piano)
; each note 4 ticks - a quarter note fo 16 ticks
(for (key 24 95) (gs:play-note key 4 95 0))

; wait until play has finished before exiting
(sleep (+ 3000 (* 72 128)))
(exit)

;; eof


