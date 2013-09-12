;; @module win32demo.lsp
;; @description Event loop demo
;; @author Cyril Slobin
;; @location http://slobin.pp.ru/newlisp/win32demo.lsp
;; @version $Id: win32demo.lsp,v 1.4 2010/06/10 02:14:08 slobin Exp $

; see also: http://slobin.livejournal.com/339071.html ; added by LM

(println "win32demo")

(define (import-list library flist)
    (dolist (fname flist) (import library (string fname))))

(define-macro (@setq %var %value)
    (set %var (eval %value))
    (println %var " " (eval %var)))

(import-list "kernel32" '(GetModuleHandleA GetConsoleWindow))
(import-list "user32" '(PostQuitMessage DefWindowProcA))
(import-list "user32" '(LoadCursorA RegisterClassA CreateWindowExA))
(import-list "user32" '(ShowWindow UpdateWindow))
(import-list "user32" '(GetMessageA TranslateMessage DispatchMessageA))

(setq WM_CREATE 1 WM_DESTROY 2 WM_CHAR 0x102 WM_LBUTTONDOWN 0x201)
(setq IDC_ARROW 0x7F00 CS_VREDRAW 1 CS_HREDRAW 2 COLOR_WINDOW 5)
(setq WS_OVERLAPPEDWINDOW 0xCF0000 HWND_MESSAGE -3 SW_SHOWDEFAULT 10)

(@setq hinstance (GetModuleHandleA 0))
(@setq hconsole (GetConsoleWindow))
(@setq cursor (LoadCursorA 0 IDC_ARROW))

(define (window-callback-function hwnd message wparam lparam)
    (cond
        ((= message WM_CREATE) (println "created") 0)
        ((= message WM_LBUTTONDOWN) (println "click!") 0)
        ((= message WM_CHAR) (println (format "char %c" wparam)) 0)
        ((= message WM_DESTROY) (println "destroyed") (PostQuitMessage 0) 0)
        (true (DefWindowProcA hwnd message wparam lparam))))

(setq wndproc (callback 0 'window-callback-function))
(setq classname "newlisp class")

(setq wc (pack (dup "ld" 10)
            (| CS_HREDRAW CS_VREDRAW)   ; style
            wndproc                     ; lpfnWndProc
            0                           ; cbClsExtra
            0                           ; cbWndExtra
            hinstance                   ; hInstance
            0                           ; hIcon
            cursor                      ; hCursor
            (+ COLOR_WINDOW 1)          ; hbrBackground
            0                           ; lpszMenuName
            classname                   ; lpszClassName
         ))

(@setq hwc (RegisterClassA wc))
(@setq hwnd (CreateWindowExA
                0                       ; dwExStyle
                "newlisp class"         ; lpClassName
                "newlisp window"        ; lpWindowName
                WS_OVERLAPPEDWINDOW     ; dwStyle
                80 60 640 480           ; x y w h
                0                       ; hwndParent
                0                       ; hMenu
                hinstance               ; hInstance
                0                       ; hParam
           ))

; hconsole and HWND_MESSAGE are other useful values for hwndParent

(ShowWindow hwnd SW_SHOWDEFAULT)
(UpdateWindow hwnd)

(setq msg (pack "n28"))

(until (member (GetMessageA msg hwnd 0 0) '(0 -1))
    (TranslateMessage msg)
    (DispatchMessageA msg))

(println "the end")

(exit)

