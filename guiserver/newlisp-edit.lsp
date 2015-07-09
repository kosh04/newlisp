#!/usr/bin/newlisp

; newlisp-edit.lsp - multiple tab LISP editor and support for running code from the editor
; needs 9.9.2 version minimum to run

; version 1.26 fixed old tab-switching bug when closing a tab
; version 1.27 took out writing debug edit.txt to Application folder
; version 1.28 decrementing font size exited editor (missing dec conversion for 10.0)
; version 1.30 change fonts in both: editor and monitor depending on active window
; version 1.31 cmd-x/v/z/Z and ctrl-x/v/z/Z did not mark edit buffer as dirty
; version 1.32 newlispDoc directory configured now depending on NEWLISPDIR on Unix
; version 1.33 eliminated manuals in help on all but OSX platform
; version 1.34 fix for OSX Homebrew when NEWLISPENV is not /usr/share/newlisp
; version 1.50 fix for run-shell for Java 7 update 21 (also runs on previous Java)
; version 1.51 changes for ostype Windows and guisever/index.html renamed to index-gs.html

(set-locale "C")

;;;; initialization
(set 'newlispDir (env "NEWLISPDIR"))

(set 'newlispDoc (if (= ostype "Windows")
   newlispDir (join (reverse (cons "doc/newlisp" (rest (reverse (parse newlispDir "/"))))) "/")))

(load (string newlispDir "/guiserver.lsp"))

(constant (global '$HOME) (or (env "HOME") (env "USERPROFILE") (env "DOCUMENT_ROOT") ""))
(constant '$TEMP (if (= ostype "Windows") (or (env "TEMP") "C:\\temp") "/tmp"))

(if (= ostype "Windows")
	(begin
		(set 'userSettingsDir (string
			(or (env "APPDATA") (env "HOME") (env "USERPROFILE") (env "DOCUMENT_ROOT")) "/newLISP"))
		(set 'userSettingsPath (append userSettingsDir "/newlisp-edit.config"))
		(set 'recentFilesPath (append userSettingsDir "/newlisp-edit-recent"))
		(if (not (directory userSettingsDir)) (make-dir userSettingsDir))
	)
	(begin
		(set 'userSettingsPath (append $HOME "/.newlisp-edit.conf"))
		(set 'recentFilesPath (append $HOME "/.newlisp-edit-recent"))
	)
)

;; init guiserver

(gs:init 64001 "localhost") ; default port is 64001 but can be changed
;(gs:set-trace true)


;; create default user settings

(gs:get-screen)
(set 'config:currentAppWidth 800)
(set 'config:currentAppHeight (- (gs:screen 1) 80))
(set 'config:currentAppX (/ (- (gs:screen 0) config:currentAppWidth) 3))
(set 'config:currentAppY (/ (- (gs:screen 1) config:currentAppHeight) 2))
(set 'config:currentForeground '(0.0 0.0 0.2))
(set 'config:currentBackground '(1.0 1.0 1.0))
(set 'config:currentDir $HOME)
(set 'config:currentFontName (if (= ostype "Windows") "Monospaced" "Lucida Sans Typewriter"))
(set 'config:currentFontSize (if (= ostype "Windows") 14 13))
(set 'config:currentMonitorFontName (if (= ostype "Windows") "Monospaced" "Lucida Sans Typewriter"))
(set 'config:currentMonitorFontSize (if (= ostype "Windows") 14 13))
(set 'config:currentToolbarFloatable "no")
(set 'config:currentTabsize 16)
(set 'config:currentTabsPosition "top")
(set 'config:currentToolbarShow "yes")
(set 'config:currentThemeIdx 0)
(set 'config:currentAltShell "")
(set 'config:currentMonitorForeground '(0.1 0.1 0.5))
(set 'config:currentMonitorBackground '(0.95 0.95 0.95))
(set 'config:currentExtension "")

;; configure themes

; name background, foreground, caret, selection
; comments, keywords, strings
; numbers, quoted, parentheses

(set 'config:currentThemes '(
	("Mozart" (1 1 1) (0 0 0) (0.5 0.5 0.8) (0.7 0.7 1.0)
		(0.5 0.5 0.5) (0 0 0.75)	(0 0.5 0.0)	
		(0.50 0.5 0)	(0.350 0.350 0.50)	(0.50 0 0))

	("Herrmann" (0.3242 0.3984 0.4648) (0.6875 0.6992 0.5781) (0.918 0.4961 0.1016) (0.2773 0.3164 0.4258)
		(0.5 0.5781 0.597)	(0.125 0.2031 0.332)	(0.5547 0.6562 0.6562)	
		(0.8203 0.6055 0.1953)	(0.8203 0.6055 0.1953)	(0.125 0.2031 0.332))

	("Shostakovich" (0.2 0.2 0.2) (0.9 0.9 0.9) (0.7 0.7 0.7) (0.8 0.8 1.0)
		(0.6 0.6 0.6)	(0.9 0.9 0.3) (0.4 0.9 0.4) 
		(0.75 0.75 0.95) (0.5 0.5 0.9) (1.0 0.3 0.3))
))

;; initialize script list
(set 'config:currentScripts 
	(list 
		(list "Word count" (string newlispDir "/guiserver/word-count.lsp") "content")
		(list "Uppercase" (string newlispDir "/guiserver/uppercase.lsp") "selection" "F4")
))

;; menu-item handler for themes
(define (theme-handler id)
  ; extract theme index from id string and extract theme from list
	(letn ( (idx (int (9 id))) (T (config:currentThemes idx)) )
		(gs:set-background currentEdit (T 1))
		(set 'currentBackground (T 1))
		(gs:set-foreground currentEdit (T 2))
		(set 'currentForeground (T 2))
		(gs:set-caret-color currentEdit (T 3))
		(gs:set-selection-color currentEdit (T 4))
		(gs:set-syntax-colors (T 5) (T 6) (T 7) (T 8) (T 9) (T 10))
		(gs:set-syntax currentEdit currentSyntaxStatus)
		(gs:set-selected 'ViewSyntax (true? currentSyntaxStatus))
		(set 'currentThemeIdx idx)
		(dotimes (i (length config:currentThemes))
			(gs:enable (string "ViewTheme" i)))
		(gs:disable (string "ViewTheme" idx))
	)
)

(define (current-file-syntax)
	(cond
		((ends-with currentFile ".lsp") "lsp")
		((ends-with currentFile ".c") "c")
		((ends-with currentFile ".cpp") "cpp")
		((ends-with currentFile ".h") "cpp")
		((ends-with currentFile ".java") "java")
		((ends-with currentFile ".php") "php")
		(true nil)
	)
)

;; script-handler, saves current edit tab to a temporary file
;; and passes the file name as an argument to the script
;; scripts are registered in the settings file
;; scripts must exit or newlisp-edit will hang.
(define (script-handler id)
	(letn ( (idx (int (10 id))) (S (config:currentScripts idx)))
		(set 'currentScriptFile (S 1))
		(if (file? currentScriptFile)
			(begin
				(set 'currentScriptMode (S 2))
				(if (= currentScriptMode "selection")
					(gs:get-selected-text currentEdit 'script-execute)
				; else "content"
    			(gs:get-text currentEdit 'script-execute))
			)
			(output-monitor (string ";--- could not find script " currentScriptFile " ---\n"))
		)
	)
)

(define (script-execute id text)
	(if (not text) (set 'text "===="))
	(let (file (string $TEMP "/" (uuid)))
		(if (= ostype "Windows")
			(write-file file (replace "\n" (base64-dec text) "\r\n"))
			(write-file file (base64-dec text)))
		(if (= ostype "Windows")
			(catch (exec (string {newlisp.exe "} currentScriptFile {" } file " > " (string file "out"))) 'result)
			(catch (exec (string "/usr/bin/newlisp " currentScriptFile " " file)) 'result)
		)
		(if (list? result)
			(begin
				(set 'result (if (= ostype "Windows")
					(read-file (string file "out"))
 					(join result "\n")))
				(if (= currentScriptMode "selection")
					(paste-action result)
					(if (= ostype "Windows")
						(output-monitor result)
						(output-monitor (string result "\n")))
				)
			)
			(output-monitor result)
		)
		(if (= ostype "Windows") (delete-file (string file "out")))
		(delete-file file)
	)
)



;; if newlisp-edit.config exists load user-settings
(if (file? userSettingsPath) 
	(if (not (catch (load userSettingsPath) 'result))
		(set 'loadUserSettingsError result))
	(if (not (catch (save userSettingsPath 'config) 'result))
		(set 'loadUserSettingsError result))
)

(if (not	(catch (load recentFilesPath) 'result))
	(set 'recentFiles '()))

(set 'currentAppX config:currentAppX)
(set 'currentAppY config:currentAppY)
(set 'currentAppWidth config:currentAppWidth)
(set 'currentAppHeight config:currentAppHeight)
(set 'currentForeground config:currentForeground)
(set 'currentBackground config:currentBackground)
(set 'currentDir config:currentDir)
(set 'currentFile "Untitled.lsp")
(set 'currentFontName config:currentFontName)
(set 'currentFontSize config:currentFontSize)
(set 'currentMonitorFontName config:currentMonitorFontName)
(set 'currentMonitorFontSize config:currentMonitorFontSize)
(set 'currentToolbarFloatable config:currentToolbarFloatable)
(set 'currentTabsPosition config:currentTabsPosition)
(set 'currentToolbarShow config:currentToolbarShow)
(set 'currentThemeIdx config:currentThemeIdx)
(set 'currentAltShell config:currentAltShell)
(set 'currentMonitorForeground config:currentMonitorForeground)
(set 'currentMonitorBackground config:currentMonitorBackground)
(set 'currentExtension config:currentExtension)
(set 'currentPath (string currentDir "/" currentFile))
(set 'currentSyntaxStatus "lsp")

;(gs:set-look-and-feel "com.sun.java.swing.plaf.motif.MotifLookAndFeel")
;(gs:set-look-and-feel "javax.swing.plaf.metal.MetalLookAndFeel")
;(gs:set-look-and-feel "com.sun.java.swing.plaf.windows.WindowsLookAndFeel")
;(gs:set-look-and-feel "javax.swing.plaf.mac.MacLookAndFeel")
;(gs:set-look-and-feel "com.sun.java.swing.plaf.gtk.GTKLookAndFeel")

(define (start-newlisp-shell)
	(if (= ostype "Windows")
		(gs:run-shell 'OutputArea 
			(string newlispDir "/newlisp.exe") (string currentExtension " -C -w \"" $HOME "\""))
		(gs:run-shell 'OutputArea 
			(string "/usr/bin/newlisp") (string currentExtension " -C -w " $HOME))
	)
)

(define (startshell-handler)
	(gs:run-shell 'OutputArea currentAltShell)
)

;;;; describe the GUI ;;;;;;;;;;;;;;;;;;;;;;;

(gs:frame 'TheEditor currentAppX currentAppY currentAppWidth currentAppHeight "newLISP edit")
(gs:frame-closed 'TheEditor 'quitbutton-handler)

(set 'default-currentFontName currentFontName)

(gs:set-border-layout 'TheEditor 0 0)
(gs:tool-bar 'ToolBar (= currentToolbarFloatable "yes"))
(gs:set-flow-layout 'ToolBar "left" 16 4)
(gs:image-button 'NewButton 'newbutton-handler "/local/new32.png" "/local/new-down32.png")
(gs:image-button 'ClearButton 'clearbutton-handler "/local/clear32.png" "/local/clear-down32.png")
(gs:image-button 'LoadButton 'loadbutton-handler "/local/folder-opened32.png" "/local/folder-opened-down32.png")
(gs:image-button 'SaveButton 'savebutton-handler "/local/save32.png" "/local/save-down32.png")
(gs:image-button 'CutButton 'cutbutton-handler "/local/cut32.png" "/local/cut-down32.png")
(gs:image-button 'CopyButton 'copybutton-handler "/local/copy32.png" "/local/copy-down32.png")
(gs:image-button 'PasteButton 'pastebutton-handler "/local/paste32.png" "/local/paste-down32.png")
(gs:image-button 'FindButton 'findbutton-handler "/local/search32.png" "/local/search-down32.png")
(gs:image-button 'ExecButton 'process-or-execbutton-handler "/local/run32.png" "/local/run-down32.png")
(gs:image-button 'RestartButton 'start-newlisp-shell "/local/restart32.png" "/local/restart-down32.png")
(gs:image-button 'FontBookButton 'fontbookbutton-handler "/local/font-book32.png" "/local/font-book-down32.png")
(gs:set-tool-tip 'NewButton "Open a new tab")
(gs:set-tool-tip 'LoadButton  "Load file into editor")
(gs:set-tool-tip 'SaveButton "Save file in editor")
(gs:set-tool-tip 'ClearButton "Clear editor panel")
(gs:set-tool-tip 'CutButton "Cut selection to clipboard")
(gs:set-tool-tip 'CopyButton "Copy selection to clipboard")
(gs:set-tool-tip 'PasteButton "Paste from clipboard")
(gs:set-tool-tip 'FindButton "Find")

(gs:set-tool-tip 'ExecButton "Run editor content")
(gs:set-tool-tip 'RestartButton "Restart auxiliary newLISP process")
(gs:set-tool-tip 'FontBookButton "Select editor font")

(gs:add-to 'ToolBar 'NewButton 'LoadButton 'SaveButton)
(gs:add-separator 'ToolBar)
(gs:add-to 'ToolBar 'ClearButton 'CutButton 'CopyButton 'PasteButton)
(gs:add-separator 'ToolBar)
(gs:add-to 'ToolBar 'FindButton)
(gs:add-separator 'ToolBar)
(gs:add-to 'ToolBar 'ExecButton 'RestartButton)
(gs:add-separator 'ToolBar)
(gs:add-to 'ToolBar 'FontBookButton)

(gs:panel 'FontPanel 46 18)
(gs:set-grid-layout 'FontPanel 1 3)
(gs:label 'FontSmallerLabel "A" "center")
(gs:label 'FontSizeLabel (string currentFontSize) "center")
(gs:label 'FontBiggerLabel "A" "center")
(gs:set-tool-tip 'FontSizeLabel "Font size" "right")
(gs:set-font 'FontSmallerLabel "Lucida Sans Regular" 10 "italic")
(gs:set-font 'FontSizeLabel "Lucida Sans Regular" 10 "plain")
(gs:set-font 'FontBiggerLabel "Lucida Sans Regular" 13 "italic")
(gs:add-to 'FontPanel 'FontSmallerLabel 'FontSizeLabel 'FontBiggerLabel)

(gs:add-to 'ToolBar 'FontPanel)

(gs:mouse-event 'FontBiggerLabel 'fontpanel-event)
(gs:mouse-event 'FontSmallerLabel 'fontpanel-event)

(gs:set-tool-tip 'FontBiggerLabel "Bigger font")
(gs:set-tool-tip 'FontSmallerLabel "Smaller font")

;; disable Cut- and Copy- buttons until selection is make
;; SaveButton util content in EditArea
(gs:disable 'CutButton 'CopyButton 'SaveButton)

(if (= currentToolbarShow "yes")
	(gs:add-to 'TheEditor 'ToolBar "north"))

(set 'tabs-stack '())

;; configure text area
(define (make-editor-tab dir file-name)
	(let (edit-tab (append "tab-" (uuid)) )
		(push (list edit-tab dir file-name (list true 0 0 (current-file-syntax))) tabs-stack -1)
		(gs:text-pane edit-tab 'editarea-handler "text/plain")
		(gs:mouse-event edit-tab 'editarea-mouse-handler)
		(gs:set-foreground edit-tab currentForeground)
		(gs:set-background edit-tab currentBackground) 
		(gs:set-tab-size edit-tab config:currentTabsize)
		(gs:set-font edit-tab currentFontName currentFontSize "plain")
	edit-tab)
)

(set 'currentDot 0 'currentMark 0)
(set 'edit-buffer-clean true)
(set 'currentEdit (make-editor-tab currentDir currentFile))
(gs:set-syntax currentEdit (ends-with currentFile ".lsp"))
(set 'currentTabIndex 0)
(gs:set-text 'TheEditor (string "newLISP edit - " currentPath))

(set 'editUndoCount 0)

(gs:tabbed-pane 'EditorTabs 'editortabs-handler currentTabsPosition
	currentEdit "Untitled.lsp")

(gs:set-icon 'EditorTabs "/local/green10.png" currentTabIndex)

; configure output area
(gs:text-area 'OutputArea 'gs:no-action)
(gs:mouse-event 'OutputArea 'outputarea-mouse-handler)
(gs:set-background 'OutputArea currentMonitorBackground)
(gs:set-foreground 'OutputArea currentMonitorForeground)
;(gs:set-font 'OutputArea "Monospaced" currentMonitorFontSize "plain")
(gs:set-font 'OutputArea currentMonitorFontName currentMonitorFontSize "plain")
(gs:split-pane 'TextPanel "horizontal" 0.70 0.5 5)
(gs:add-to 'TextPanel 'EditorTabs 'OutputArea)
(gs:add-to 'TheEditor  'TextPanel "center")

;; configure main menu
(gs:menu 'FileMenu "File")
(gs:menu-item 'FileClear 'clearbutton-handler "Clear tab" true)
(gs:menu-item 'FileNew 'newbutton-handler "New tab")
(gs:menu-item 'FileOpen 'loadbutton-handler "Open ...")
(gs:menu-item 'FileClose 'fileclose-handler "Close tab")
(gs:menu-item 'FileSave 'savebutton-handler "Save")
(gs:menu-item 'FileSaveAs 'saveasbutton-handler "Save As ...")
(gs:menu-item 'FileSettings 'savesettings-handler "Save Settings")
(gs:menu-item 'FileQuit 'quitbutton-handler (if (= ostype "Windows") "Exit" "Quit"))

(gs:menu 'FileRecent "Recent Files")


(if recentFiles (dolist (f recentFiles)
	(if (file? (f 1))
		(begin
			(gs:menu-item (f 0) 'recentfiles-handler (f 1))
			(gs:add-to 'FileRecent (f 0)))
		(replace f recentFiles))
))

(gs:menu 'EditMenu "Edit")
(gs:menu-item 'EditUndo 'undo-handler "Undo")
(gs:menu-item 'EditRedo 'redo-handler "Redo")
(gs:menu-item 'EditCut 'cutbutton-handler "Cut")
(gs:menu-item 'EditCopy 'copybutton-handler "Copy")
(gs:menu-item 'EditPaste 'pastebutton-handler "Paste")
(gs:menu-item 'EditGoto 'goto-handler "Goto Line")
(gs:menu-item 'EditPosition 'position-handler "Get Position")
(gs:menu-item 'EditGotoEditor 'switchwindow-handler "Goto Editor")
(gs:menu-item 'EditGotoShell 'switchwindow-handler "Goto Shell")
(gs:menu-item 'EditFind 'findbutton-handler "Find")
(gs:menu-item 'EditFindNext 'findtextnext-action "Find next")
(gs:menu-item 'EditFindPrevious 'findtextprevious-action "Find Previous")
(gs:menu-item 'EditReplace 'findtextreplace-action "Replace Selection")
;(gs:menu-item 'EditReplaceNext 'findtextreplace-action "Replace Next")
(gs:menu-item 'EditFindDispose 'finddispose-handler "Find Dispose")

(gs:menu-popup 'EditMenuPopup "Edit")
(gs:menu-item 'EditCutP 'cutbutton-handler "Cut")
(gs:menu-item 'EditCopyP 'copybutton-handler "Copy")
(gs:menu-item 'EditPasteP 'pastebutton-handler "Paste")

(gs:menu 'ViewMenu "View")
(gs:menu-item 'ViewClearMonitor 'viewclearmonitor-handler "Clear monitor")
(gs:menu-item-check 'ViewToolbar 'viewtoolbar-handler "Toolbar" (= currentToolbarShow "yes"))
(gs:menu-item-check 'ViewSyntax 'viewsyntax-handler "Syntax coloring" true)
(dolist (T config:currentThemes)
	(gs:menu-item (string "ViewTheme" $idx) 'theme-handler (T 0)))
(gs:menu-item 'ViewFontBook 'fontbookbutton-handler "Font faces ...")
(gs:menu-item 'ViewFontSmaller 'viewfontsmaller-handler "Font smaller")
(gs:menu-item 'ViewFontBigger 'viewfontbigger-handler "Font bigger")


(gs:menu 'ToolMenu "Tools")
(gs:menu-item 'ToolEditSettings 'tooleditsettings-handler "Edit Settings")
(dolist (T config:currentScripts)
	(gs:menu-item (string "ToolScript" $idx) 'script-handler (T 0))
	(if (= 4 (length T)) 
		(gs:set-accelerator (string "ToolScript" $idx) (T 3)))
)

(gs:menu 'HelpMenu "Help")
(unless (= ostype "OSX") ; on OSX use about option in top frame menu
	(gs:menu-item 'HelpAbout 'helpabout-handler "About newLISP-GS"))
(gs:menu-item 'HelpDemos 'opendemos-handler "Open Demo Folder")
(when (= ostype "OSX")
	(gs:menu-item 'HelpManual 'helpmanual-handler "newLISP Manual and Reference")
	(gs:menu-item 'HelpGuiserver 'helpguiserver-handler "GS Manual"))

(gs:menu 'RunMenu "Run")
(gs:menu-item 'RunRun 'process-or-execbutton-handler "Run")
(gs:set-icon 'RunRun "/local/run16.png")
(gs:menu-item 'RunRestart 'start-newlisp-shell "Restart")
(gs:menu-item 'RunShell 'startshell-handler "Alternate shell")

(gs:menu-popup 'SyntaxMenu "Syntax")
(gs:menu-item 'SyntaxNewlisp 'syntaxmenu-handler "newLISP syntax")
(gs:menu-item 'SyntaxC 'syntaxmenu-handler "C syntax")
(gs:menu-item 'SyntaxCPP 'syntaxmenu-handler "C++ syntax")
(gs:menu-item 'SyntaxJava 'syntaxmenu-handler "Java syntax")
(gs:menu-item 'SyntaxPHP 'syntaxmenu-handler "PHP syntax")
(gs:add-to 'SyntaxMenu 'SyntaxNewlisp 'SyntaxC 'SyntaxCPP 'SyntaxJava 'SyntaxPHP)

(if (= ostype "OSX")
	(begin ;; MacOS X keyboard
		(gs:set-accelerator 'FileClear "shift meta N")
		(gs:set-accelerator 'FileNew "meta N")
		(gs:set-accelerator 'FileOpen "meta O")
		(gs:set-accelerator 'FileClose "meta W")
		(gs:set-accelerator 'FileSave "meta S")
		(gs:set-accelerator 'FileSaveAs "shift meta S")
		(gs:set-accelerator 'EditUndo "meta Z")
		(gs:set-accelerator 'EditRedo "shift meta Z")
		(gs:set-accelerator 'EditCut "meta X")
		(gs:set-accelerator 'EditCopy "meta C")
		(gs:set-accelerator 'EditPaste "meta V")
		(gs:set-accelerator 'EditGoto "meta L")
		(gs:set-accelerator 'EditPosition "shift meta L")
		(gs:set-accelerator 'EditGotoEditor "meta 1")
		(gs:set-accelerator 'EditGotoShell "meta 2")
		(gs:set-accelerator 'EditFind "meta F")
		(gs:set-accelerator 'EditFindDispose "meta D")
		(gs:set-accelerator 'EditFindPrevious "shift meta G")
		(gs:set-accelerator 'EditFindNext "meta G")
		(gs:set-accelerator 'EditReplace "meta J")
;		(gs:set-accelerator 'EditReplaceNext "shift meta J")
		(gs:set-accelerator 'RunRun "meta R")
		(gs:set-accelerator 'RunRestart "shift meta R")
		(gs:set-accelerator 'ViewClearMonitor "meta M")
		(gs:set-accelerator 'ViewFontBook "meta T")
		(gs:set-accelerator 'ViewFontSmaller "meta MINUS")
		(gs:set-accelerator 'ViewFontBigger "shift meta EQUALS")
		(gs:set-accelerator 'ViewSyntax "meta Y")
	)
	(begin ;; PC keyboard 
		(gs:set-accelerator 'FileClear "ctrl N")
		(gs:set-accelerator 'FileNew "shift ctrl N")
		(gs:set-accelerator 'FileOpen "ctrl O")
		(gs:set-accelerator 'FileClose "ctrl W")
		(gs:set-accelerator 'FileSave "ctrl S")
		(gs:set-accelerator 'FileSaveAs "shift ctrl S")
		(gs:set-accelerator 'EditUndo "ctrl Z")
		(gs:set-accelerator 'EditRedo "shift ctrl Z")
		(gs:set-accelerator 'EditCopy "ctrl C")
		(gs:set-accelerator 'EditCut "ctrl X")
		(gs:set-accelerator 'EditPaste "ctrl V")
		(gs:set-accelerator 'EditGoto "alt L")
		(gs:set-accelerator 'EditPosition "shift alt L")
		(gs:set-accelerator 'EditGotoEditor "alt 1")
		(gs:set-accelerator 'EditGotoShell "alt 2")
		(gs:set-accelerator 'EditFind "ctrl F")
		(gs:set-accelerator 'EditFindPrevious "shift ctrl G")
		(gs:set-accelerator 'EditFindNext "ctrl G")
		(gs:set-accelerator 'EditFindDispose "ctrl D")
		(gs:set-accelerator 'EditReplace "ctrl J")
;		(gs:set-accelerator 'EditReplaceNext "shift ctrl J")
		(gs:set-accelerator 'RunRun "alt R")
		(gs:set-accelerator 'RunRestart "shift alt R")
		(gs:set-accelerator 'ViewClearMonitor "ctrl M")
		(gs:set-accelerator 'ViewFontBook "ctrl T")
		(gs:set-accelerator 'ViewFontSmaller "ctrl MINUS")
		(gs:set-accelerator 'ViewFontBigger "ctrl EQUALS")
		(gs:set-accelerator 'ViewSyntax "alt Y")
	)
)

;; disable Save and SaveAs until content in EditArea
(gs:disable 'FileSave 'FileSaveAs)
;; disable Cut and Copy menu items until selection is made
(gs:disable 'EditUndo 'EditRedo 'EditCut 'EditCutP 'EditCopy 'EditCopyP)
;; disable various find dialog options until dialog is up first
(gs:disable 'EditFindPrevious 'EditFindNext 'EditReplace 'EditFindDispose)
;; disable monitor clear until something is in it
;(gs:disable 'ViewClearMonitor)

; File menu
(gs:add-to 'FileMenu 'FileClear 'FileNew 'FileClose)
(gs:add-separator 'FileMenu)
(gs:add-to 'FileMenu 'FileRecent)
(gs:add-separator 'FileMenu)
(gs:add-to 'FileMenu 'FileOpen 'FileSave 'FileSaveAs)
(gs:add-separator 'FileMenu)
(gs:add-to 'FileMenu 'FileSettings)
(gs:add-separator 'FileMenu)
(gs:add-to 'FileMenu 'FileQuit)

; Edit menun
(gs:add-to 'EditMenu 'EditUndo 'EditRedo)
(gs:add-separator 'EditMenu)
(gs:add-to 'EditMenu 'EditCut 'EditCopy 'EditPaste)
(gs:add-separator 'EditMenu)
(gs:add-to 'EditMenu 'EditGoto 'EditPosition 'EditGotoEditor 'EditGotoShell)
(gs:add-separator 'EditMenu)
(gs:add-to 'EditMenu 'EditFind 'EditFindNext 'EditFindPrevious 'EditReplace)
(gs:add-separator 'EditMenu)
(gs:add-to 'EditMenu 'EditFindDispose)
(gs:disable 'EditFindDispose)
; edit area popup
(gs:add-to 'EditMenuPopup 'EditCutP 'EditCopyP 'EditPasteP)

; View menu
(gs:add-to 'ViewMenu 'ViewClearMonitor)
(gs:add-separator 'ViewMenu)
(gs:add-to 'ViewMenu 'ViewToolbar 'ViewSyntax)
(gs:add-separator 'ViewMenu)
(dolist (T config:currentThemes)
	(gs:add-to 'ViewMenu (string "ViewTheme" $idx)))
(gs:add-separator 'ViewMenu)
(gs:add-to 'ViewMenu 'ViewFontBook 'ViewFontSmaller 'ViewFontBigger)

(if (empty? currentAltShell)
	(gs:add-to 'RunMenu 'RunRun 'RunRestart)
	(gs:add-to 'RunMenu 'RunRun 'RunRestart 'RunShell))

; Tool menu
(gs:add-to 'ToolMenu 'ToolEditSettings)
(gs:add-separator 'ToolMenu)
(dolist (T config:currentScripts)
	(gs:add-to 'ToolMenu (string "ToolScript" $idx)))

; Help menu
; manuals are only added on Mac OSX, other platforms block
; the IDE from working when the browser is opened until the 
; it is closed again
(when (= ostype "OSX")
	(gs:add-to 'HelpMenu 'HelpManual 'HelpGuiserver)
	(gs:add-separator 'HelpMenu))

(gs:add-to 'HelpMenu 'HelpDemos)

(unless (= ostype "OSX")
	(gs:add-separator 'HelpMenu)
	(gs:add-to 'HelpMenu 'HelpAbout))


(gs:menu-bar 'TheEditor 'FileMenu 'EditMenu 'RunMenu 'ViewMenu 'ToolMenu 'HelpMenu)

(gs:set-visible 'TheEditor true)
(gs:dispose-splash)

; start auxiliary shell newLISP process for evaluation of edit area in OutputArea
(start-newlisp-shell)

(gs:request-focus currentEdit) ; set focus on editarea

; check if user settings where loaded succesfully
(if loadUserSettingsError
	(gs:message-dialog 'TheEditor (string "Problem loading: " userSettingsPath ".")
		loadUserSettingsError "warning")
)

;;;; define actions

(define (clear-current-tab)
	(gs:clear-text currentEdit)
	(set 'currentDir $HOME)
	(set 'currentFile "Untitled.lsp")
	(set 'currentPath (string currentDir "/" currentFile))
	(set 'currentDot 0 'currentMark 0)
	(update-current-tab)
	(gs:disable 'SaveButton 'CutButton 'CopyButton 'FileSave 'EditCut 'EditCutP 'EditCopy 'EditCopyP)
	(gs:set-icon 'EditorTabs "/local/green10.png" currentTabIndex)
	(gs:set-text 'EditorTabs currentFile currentTabIndex)
	(gs:set-text 'TheEditor (string "newLISP edit - " currentPath))
)
		
(define (fileclose-handler)
	(if (not edit-buffer-clean)
		(gs:confirm-dialog 'TheEditor 'fileclose-action "Close file tab" 
			(string "Abandon unsaved " currentFile "?") "yes-no")
		(fileclose-action 'TheEditor 0)
	)	
)

(define (fileclose-action id result)
;(println "in fileclose-action")
	(if (= result 0)
		(if (> (length tabs-stack) 1)
			(begin
				(gs:remove-tab 'EditorTabs currentTabIndex)
;(println "currentTabIndex before pop:" currentTabIndex)
;(println (assoc currentEdit tabs-stack))
				(pop-assoc currentEdit tabs-stack)
;(println "currentTabIndex after pop:" currentTabIndex)
				(if (= currentTabIndex (length tabs-stack)) ; it was the right most tab
					(dec currentTabIndex)
					(begin ; its was not the most right which was removed
						(set 'currentEdit (first (tabs-stack currentTabIndex)))
						(switch-to-tab currentEdit)
					)
				)
			)
			(clear-current-tab)
		)
	)
)

(define (newbutton-handler)
	(update-current-tab)
	(set 'currentDir $HOME)
	(set 'currentFile "Untitled.lsp")
	(set 'currentPath (string currentDir "/" currentFile))
	(set 'currentDot 0 'currentMark 0)
	(set 'edit-buffer-clean true)
	(set 'currentEdit (make-editor-tab currentDir currentFile))
	(gs:insert-tab 'EditorTabs currentEdit currentFile (length tabs-stack))
	(gs:request-focus 'EditorTabs (length tabs-stack))
	(gs:request-focus currentEdit) ; set focus in edit area
	(theme-handler (string "ViewTheme" currentThemeIdx))
)

(define (recentfiles-handler id)
	(update-current-tab)
	(let (file (lookup id recentFiles))
		(if (not (file? file))
			(gs:message-dialog 'TheEditor "Loading file" (append "Cannot find: " file))
			(begin
				(set 'currentPath (lookup id recentFiles))
				(open-currentpath-in-tab)
			)
		)
	)
)

(define (loadbutton-handler id)
	(gs:open-file-dialog 'TheEditor 'openfile-action  currentDir 
		".lsp .c .h .txt .java .htm .html .css .php .pl .py .rb .lisp .el .cl .cpp .tcl .config .cgi .js .py .pl .sh:" 
		"Various text formats")
)

(define (openfile-action id  op file)
	(if file
		(begin
			(update-current-tab)
			(set 'currentPath (base64-dec file))
			(open-currentpath-in-tab)
		)
	)
)

(define (open-currentpath-in-tab)
	(set 'currentDir (join (chop (parse currentPath {\\|/} 0)) "/" ))
	(set 'currentFile (last (parse currentPath {\\|/} 0)))
	(set 'currentEdit (make-editor-tab currentDir currentFile))
	(set 'edit-buffer-clean true)
	(set 'currentDot 0 'currentMark 0)
	(gs:insert-tab 'EditorTabs currentEdit currentFile (length tabs-stack))
	(gs:request-focus 'EditorTabs (length tabs-stack))
	(gs:request-focus currentEdit) ; set focus in edit area
	(gs:set-cursor currentEdit "wait")

	(gs:set-text 'TheEditor (string "newLISP edit - " currentPath))
	(gs:enable 'FileSaveAs)

	(gs:load-text currentEdit currentPath)
	(set 'currentSyntaxStatus (current-file-syntax))
	(theme-handler (string "ViewTheme" currentThemeIdx))
	(gs:set-cursor currentEdit "default")
)

(define (savebutton-handler id)
	(if (= currentFile "Untitled.lsp")
		(saveasbutton-handler id)
		(savefile-action id op (base64-enc currentPath) true)
	)
)

(define (saveasbutton-handler id) 
	(gs:save-file-dialog 'TheEditor 'savefile-action currentDir currentFile)
)

(define (savefile-action id op file no-check)
	(set 'save-file-candidate file)
	(if file (if no-check 
		(writefile-prepare file)
		(begin
			(if (file? (base64-dec file))
				(gs:confirm-dialog 'TheEditor 'confirmsave-action "Save As ..." 
					(append "Overwrite " (base64-dec file) "?") "yes-no-cancel")
				(writefile-prepare file)
			)
		))
	)
)

(define (confirmsave-action id result)
	(if (= result 0) 
		(writefile-prepare save-file-candidate))
	(if (= result 1)
		(saveasbutton-handler 'FileSaveAs))
)	

(define (writefile-prepare file)
	(set 'currentPath (base64-dec file))
	(set 'currentDir (join (chop (parse currentPath {\\|/} 0)) "/" ))
	(set 'currentFile (last (parse currentPath {\\|/} 0)))

	(gs:set-text 'TheEditor (string "newLISP edit - " currentPath))
	(gs:set-text 'EditorTabs currentFile currentTabIndex)
	(gs:disable 'FileSave 'SaveButton)
	(gs:set-icon 'EditorTabs "/local/green10.png" currentTabIndex)
	(set 'edit-buffer-clean true)

	(gs:get-text currentEdit 'writefile-action)
)

(define (writefile-action id text)
	(local (bytes)
		(if text
			(if (= ostype "Windows")
				(set 'bytes (write-file currentPath (replace "\n" (base64-dec text) "\r\n")))
				(set 'bytes (write-file currentPath (base64-dec text)) ) ))
		(save-recent-list)
		(if (not bytes)
			(gs:message-dialog 'TheEditor "Saving file" (append "Could not save " currentPath))
			(output-monitor (string ";--- " bytes " bytes saved to " currentPath " ---\n"))
		)
	)
)

(define (save-recent-list)
	(if (ref currentPath recentFiles)
		(push (pop recentFiles (first (ref currentPath recentFiles))) recentFiles)
		(push (list (uuid) currentPath) recentFiles))
	(set 'recentFiles (0 12 recentFiles))
	(save recentFilesPath 'recentFiles)
)

(define (savesettings-handler)
	(pretty-print 256) ; force one lone line for themes
	(gs:get-bounds 'TheEditor)
	(set 'currentAppX (gs:bounds 0))
	(set 'currentAppY (gs:bounds 1))
	(set 'currentAppWidth (gs:bounds 2))
	(set 'currentAppHeight (gs:bounds 3))
	(set 'config:currentAppX currentAppX)
	(set 'config:currentAppY currentAppY)
	(set 'config:currentAppWidth currentAppWidth)
	(set 'config:currentAppHeight currentAppHeight)
	(set 'config:currentForeground currentForeground)
	(set 'config:currentBackground currentBackground)	
	(set 'config:currentDir currentDir)
	(set 'config:currentFontName currentFontName)
	(set 'config:currentFontSize currentFontSize)
	(set 'config:currentMonitorFontName currentMonitorFontName)
	(set 'config:currentMonitorFontSize currentMonitorFontSize)
	(set 'config:currentToolbarFloatable currentToolbarFloatable)
	(set 'config:currentTabsPosition currentTabsPosition)
	(set 'config:currentToolbarShow currentToolbarShow)
	(set 'config:currentThemeIdx currentThemeIdx)
	(set 'config:currentThemeHelp
			{background foreground caret selection comments keywords strings numbers quoted parentheses})
	(set 'config:currentAltShell currentAltShell)
	(set 'config:currentMonitorForeground currentMonitorForeground)
	(set 'config:currentMonitorBackground currentMonitorBackground)
	(save userSettingsPath 'config)
	(output-monitor 
		(string ";--- saved settings in: " userSettingsPath " ---\n"))
)

(define (tooleditsettings-handler)
	(set 'currentPath userSettingsPath)
	(open-currentpath-in-tab)
)

(define (opendemos-handler)
	(gs:open-file-dialog 'TheEditor 'openfile-action  (string newlispDir "/guiserver")
		".lsp" "newLISP files")
)

(define (quitbutton-handler)
	(let (is-clean-tabs true)
		(dolist (tab tabs-stack)
			(if (not (tab 3 0)) (set 'is-clean-tabs nil)))
		(if (and is-clean-tabs edit-buffer-clean)
			(quitconfirm-action nil 0)
;			(gs:confirm-dialog 'TheEditor 'quitconfirm-action
;				"Quit newLISP edit" "You really want to quit?" "yes-no")
			(gs:confirm-dialog 'TheEditor 'quitconfirm-action
				"Quit newLISP edit" "Quit and lose unsaved content?" "yes-no")
		)
	)
)

(define (quitconfirm-action id result)
	(if (= result 0) 
		(begin
			;(println "destroying shell")
			(gs:destroy-shell 'OutputArea)
			(exit))
	)
)

(define (clearbutton-handler)
	(if (not edit-buffer-clean)
		(gs:confirm-dialog 'TheEditor 'clearconfirm-action 
			"New edit" (string "Abandon unsaved content in " currentFile) "yes-no")
		(clearbutton-action)
	)
)

(define (clearconfirm-action id result)
	(if (= result 0)
		(clearbutton-action))
)

(define (clearbutton-action)
	(set 'currentPath (string currentDir "/" currentFile))
	(gs:set-text 'TheEditor (string "newLISP edit - " currentPath))
	(gs:clear-text currentEdit)
	(gs:set-icon 'EditorTabs "/local/green10.png" currentTabIndex)
	(set 'edit-buffer-clean true)
	(gs:disable 'SaveButton 'FileSave))

(define (undo-handler)
	(gs:undo-text currentEdit ))

(define (redo-handler)
	(gs:redo-text currentEdit))

(define (copybutton-handler)
	(gs:copy-text currentEdit))

(define (cutbutton-handler)
	(set-buffer-dirty)
	(gs:cut-text currentEdit)
	(gs:request-focus 'CutButton))

(define (pastebutton-handler)
	(paste-action))

; text can be 'nil' to take from clipboard
(define (paste-action text)
	(set-buffer-dirty)
	(gs:paste-text currentEdit text))

(define (set-buffer-dirty)
	(gs:enable 'FileSave 'FileSaveAs 'SaveButton)
	(gs:set-icon 'EditorTabs "/local/red10.png" currentTabIndex)
	(set 'edit-buffer-clean nil))

;; goto line

(define (goto-handler)
	(gs:dialog 'GotoDialog 'TheEditor "Goto Line" 200 60 nil nil)
	(gs:set-resizable 'GotoDialog nil)
	(gs:set-flow-layout 'GotoDialog "center")
	(gs:label 'GotoTextLabel "Line:")
	(gs:text-field 'GotoTextField 'gotoline-action 4)
	(gs:button 'GotoButton 'gotogettext-action "Goto")
	(gs:add-to 'GotoDialog 'GotoTextLabel 'GotoTextField 'GotoButton)
	(gs:set-visible 'GotoDialog true)
)

(define (gotoline-action id text)
	(if text
		(let (line (int (base64-dec text) 0))
			(gs:goto-text currentEdit line 0))	)
	(gs:dispose 'GotoDialog)
	(gs:request-focus currentEdit)
)

(define (gotogettext-action)
	(gs:get-text 'GotoTextField 'gotoline-action)
)

(define (position-handler)
	(gs:get-text-position currentEdit)
	(output-monitor (string ";--- line: " (gs:text-position 0) " column: " (gs:text-position 1) " ---\n"))
)

(define (switchwindow-handler id)
	(when (= id "MAIN:EditGotoEditor")
		(set 'cursor-in-outputarea nil)
		(gs:set-text 'FontSizeLabel (string currentFontSize))
		(gs:request-focus currentEdit))
	(unless (= id "MAIN:EditGotoEditor")
		(set 'cursor-in-outputarea true)
		(gs:set-text 'FontSizeLabel (string currentMonitorFontSize))
		(gs:request-focus 'OutputArea)
		(gs:set-caret 'OutputArea 100000))
	
)

;;;;;;;;;;;;; find text ;;;;;;;;;;;;;;;

(define (findbutton-handler)
	(if findDialogOpen 
		(begin
			(gs:request-focus 'FindTextField)
			(gs:select-text 'FindTextField 0))
		(openFindDialog)
	)
)

(define (openFindDialog)
	(gs:dialog 'FindDialog 'TheEditor "Find text" 460 200 nil nil)
	(gs:set-resizable 'FindDialog nil)
	(gs:frame-closed 'FindDialog 'finddialogclose-handler)
	(gs:set-grid-layout 'FindDialog 4 1)
	
	(gs:panel 'FindPanel)
	(gs:label 'FindTextLabel "Find:")
	(gs:text-field 'FindTextField 'findtextnext-action 24)
	(gs:add-to 'FindPanel 'FindTextLabel 'FindTextField)
	
	(gs:panel 'ReplacePanel)
	(gs:label 'FindReplaceLabel "Replace:")
	(gs:text-field 'FindReplaceField 'findtextnext-action 24)
	(gs:add-to 'ReplacePanel 'FindReplaceLabel 'FindReplaceField)
	(if (not (null? currentSearchText)) (gs:set-text 'FindTextField currentSearchText))
	(if (not (null? currentReplaceText)) (gs:set-text 'FindReplaceField currentReplaceText))

	(gs:panel 'ButtonPanel-1)
	(gs:button 'FindTextPreviousButton 'findtextprevious-action "Previous")
	(gs:button 'FindTextNextButton 'findtextnext-action "Next")
	(gs:button 'FindTextReplaceButton 'findtextreplace-action "Replace")
	(gs:add-to 'ButtonPanel-1 'FindTextPreviousButton 'FindTextNextButton 'FindTextReplaceButton)
	(gs:panel 'ButtonPanel-2)
	(gs:button 'FindTextReplaceNextButton 'findtextreplacenext-action "Replace and Next")
	(gs:button 'FindTextUndoPrevButton 'findtextundoprev-action "Undo Previous")
	(gs:add-to 'ButtonPanel-2 'FindTextReplaceNextButton 'FindTextUndoPrevButton)

	(gs:set-tool-tip 'FindTextPreviousButton "Find previous occurrence of the find text")
	(gs:set-tool-tip 'FindTextNextButton "Find next occurrence of the find text")
	(gs:set-tool-tip 'FindTextReplaceButton "Replace selected text with replacement text")
	(gs:set-tool-tip 'FindTextReplaceNextButton "Replace next occurence")
	(gs:set-tool-tip 'FindTextUndoPrevButton "Undo previous replacement")

	(gs:add-to 'FindDialog 'FindPanel 'ReplacePanel 'ButtonPanel-1 'ButtonPanel-2)
	(gs:set-visible 'FindDialog true)
	(gs:disable 'FindTextReplaceButton 'FindTextReplaceNextButton 'FindTextUndoPrevButton)
	(gs:enable 'EditFindPrevious 'EditFindNext 'EditReplace 'EditFindDispose)
	(gs:select-text 'FindTextField 0)
	(set 'findDialogOpen true)
)

(define (finddispose-handler)
	(gs:dispose 'FindDialog)
	(gs:disable 'EditFindDispose)
	(set 'findDialogOpen nil)
)

(define (finddialogclose-handler id)
	(gs:enable 'FindButton 'EditFind)
	(gs:disable 'EditFindDispose)
	(set 'findDialogOpen nil)
)

(define (findtextcheckbox-action id flag)
	(println id " " flag)
)

;; find next, this handler is enterd first by all
;; FindDialog events, text-field(s) and button(s)

(define (findtextnext-action id text)
	(if (and (or (= id "MAIN:FindTextField") (= id "MAIN:FindReplaceField")) (not text)) 
		(finddispose-handler) ; ESC key was pressed
		(begin
			(set 'currentSearchDirection "next")
			(gs:get-text 'FindTextField 'getfindtext-action)
		)
	)
)

;; find previous

(define (findtextprevious-action)
	(set 'currentSearchDirection "previous")
	(gs:get-text 'FindTextField 'getfindtext-action)
)


;; retrieve search field text

(define (getfindtext-action id text)
	(if text
		(begin
			(set 'currentSearchText (base64-dec text))
			(gs:get-text 'FindReplaceField 'getreplacetext-action)
		)
		(gs:request-focus currentEdit)
	)
)

;; rertrieve replace field text

(define (getreplacetext-action id text)
	(set 'currentReplaceText (if text (base64-dec text) ""))
	(if (not (null? currentSearchText))
		(gs:find-text currentEdit currentSearchText 'findtextresult-action currentSearchDirection))
)

(define (findtextresult-action id result)
	(if (= result -1)
		(begin
			(gs:set-text 'FindDialog "Not found")
			(gs:disable 'FindTextReplaceNextButton)
			(when (and (= currentDot currentMark) (= currentSearchDirection "next"))
				(set 'currentMark (inc currentDot))
				(gs:set-caret currentEdit currentMark)
			)
		)
		(begin
			(gs:set-text 'FindDialog "Find text")
			(gs:enable 'FindTextReplaceButton 'FindTextReplaceNextButton)
			(gs:request-focus currentEdit)
		)
	)
)

;; replace

(define (findtextreplace-action)
	(gs:undo-enable currentEdit nil)
	(if (!= currentMark currentDot)
		(gs:paste-text currentEdit currentReplaceText))
	;(gs:request-focus currentEdit)
	(gs:disable 'FindTextReplaceButton 'FindTextReplaceNextButton)
	(gs:enable 'FindTextUndoPrevButton)
	(gs:set-icon 'EditorTabs "/local/red10.png" currentTabIndex)
	(set 'edit-buffer-clean nil)
	(gs:enable 'FileSave 'FileSaveAs 'SaveButton 'EditUndo)
	(gs:undo-enable currentEdit true)
)
	
;; replace and next

(define (findtextreplacenext-action)
	(gs:undo-enable currentEdit nil)
	(gs:paste-text currentEdit currentReplaceText)
	(gs:disable 'FindTextReplaceButton)
	(gs:enable 'FindTextReplaceNextButton 'FindTextUndoPrevButton)
	(gs:set-icon 'EditorTabs "/local/red10.png" currentTabIndex)
	(set 'edit-buffer-clean nil)
	(gs:enable 'FileSave 'FileSaveAs 'SaveButton 'EditUndo)
	(set 'currentSearchDirection "next")
	(gs:get-text 'FindTextField 'getfindtext-action)
	(gs:undo-enable currentEdit true)
)

; previous and undo
(define (findtextundoprev-action)
	(gs:undo-enable currentEdit nil)
	(gs:find-text currentEdit currentReplaceText 'findpreviousresult-action "previous")
	(gs:undo-enable currentEdit true)
)

(define (findpreviousresult-action id result)
	(if (= result -1)
		(begin
			(gs:set-text 'FindDialog "Not found for undo")
			(gs:disable 'FindTextUndoPrevButton 'FindTextReplaceButton 'FindTextReplaceNextButton)
		)
		(begin
			(gs:paste-text currentEdit currentSearchText)
			(gs:request-focus currentEdit)
		)
	)
)

;; view menu fonts bigger/smaller handlers

(define (viewfontsmaller-handler)
	(if cursor-in-outputarea
		(begin 
			(dec currentMonitorFontSize)
			(gs:set-text 'FontSizeLabel (string currentMonitorFontSize))
			(gs:set-font 'OutputArea currentMonitorFontName currentMonitorFontSize "plain"))
		(begin 
			(dec currentFontSize)
			(gs:set-text 'FontSizeLabel (string currentFontSize))
			(gs:set-font currentEdit currentFontName currentFontSize "plain"))
	)
)
	

(define (viewfontbigger-handler)
	(if cursor-in-outputarea
		(begin 
			(inc currentMonitorFontSize)
			(gs:set-text 'FontSizeLabel (string currentMonitorFontSize))
			(gs:set-font 'OutputArea currentMonitorFontName currentMonitorFontSize "plain"))
		(begin 
			(inc currentFontSize)
			(gs:set-text 'FontSizeLabel (string currentFontSize))
			(gs:set-font currentEdit currentFontName currentFontSize "plain"))
	)
)
	

;; 

(define (fontbookbutton-handler)
	(gs:dialog 'FontBookSelection 'TheEditor "Click on a font name to select it" 300 200 nil nil)
	(gs:set-background 'FontBookSelection 1 1 1)
	(gs:get-fonts)
	(gs:panel 'FontPanel)
	(gs:set-grid-layout 'FontPanel (length gs:fonts) 1 0 0)
	(dolist (font gs:fonts)
		(set 'font-label (string "label-" $idx))
		(gs:label font-label font)
		(if cursor-in-outputarea
			(if (= font currentMonitorFontName)
				(gs:set-foreground font-label 0.8 0.5 0.0))
			(if (= font currentFontName)
				(gs:set-foreground font-label 0.8 0.5 0.0))
		)
		(gs:set-size font-label 100 30)
		(gs:set-font font-label font 24 "plain")
		(gs:mouse-event font-label 'mouse-action)
		(gs:add-to 'FontPanel font-label))

	(gs:scroll-pane 'Scroll 'FontPanel)
	(gs:add-to 'FontBookSelection 'Scroll)
	(gs:set-visible 'FontBookSelection true)
)

;; handle mouse clicks in font book
(define (mouse-action id type x y button cnt mods)
	(if (= type "pressed")
		(gs:set-foreground id 0.8 0.5 0.0)
		(if cursor-in-outputarea
			(begin
				(set 'currentMonitorFontName (gs:fonts (int (last (parse id "-")) 0)))
				(gs:set-font 'OutputArea currentMonitorFontName currentMonitorFontSize "plain")
				(gs:set-foreground id 0 0 0))
			(begin
				(set 'currentFontName (gs:fonts (int (last (parse id "-")) 0)))
				(gs:set-font currentEdit currentFontName currentFontSize "plain")
				(gs:set-foreground id 0 0 0))
		)
	)
)

;; font panel mouse click handler

(define (fontpanel-event id  type x y button cnt modifiers)
	(if (= type "clicked") 
		(case id
			("MAIN:FontBiggerLabel" (viewfontbigger-handler))
			("MAIN:FontSmallerLabel" (viewfontsmaller-handler))
		)
	)
)

;; initialize syntax for first tab
(theme-handler (string "ViewTheme" currentThemeIdx))
;;;;;;;;;;;; exec  newlisp over editor contents ;;;;;;;;;;

(define (process-or-execbutton-handler)
	(if (not (directory? $TEMP))
		(gs:message-dialog 'TheEditor "Cannot find temporal directory" 
				(append "Need to create a directory " $TEMP) "information")
		(begin
			(disable-main-tools)
;			(gs:get-text currentEdit 'exec-handler)
			(gs:get-text currentEdit 'auxiliary-process-handler)
			(gs:enable 'ViewClearMonitor)
		)
	)
)

; evaluates content of editor area in the auxiliary newLISP
; process, as output is generated it is displayed in the
; monitor area
(define (auxiliary-process-handler id text)
	(if text
		(begin
			(set 'text (base64-dec text))
			;;(write-file "editor.txt" text)
			(gs:eval-shell 'OutputArea (string "[cmd]\n" text "\n[/cmd]\n"))))
	(after-exec-or-process)
)

; after the exec or auxiliary process execution
; enable buttons, menus and edit area
(define (after-exec-or-process)
	(gs:enable 'FileMenu 'EditMenu 'ViewMenu 'RunMenu)
	(gs:enable 'NewButton 'ClearButton 'PasteButton 
				'LoadButton 'ExecButton 'RestartButton 'FindButton 'FontBookButton)
	(gs:set-editable  currentEdit true)
	(if (not edit-buffer-clean) (gs:enable 'SaveButton))
	(gs:request-focus currentEdit)
	(gs:select-text currentEdit currentDot currentMark)
	(if is-selection 
		(gs:enable 'CutButton 'CopyButton))
)

; disable main menus and toolbar
(define (disable-main-tools)
		(gs:disable 'FileMenu 'EditMenu 'ViewMenu 'RunMenu)
		(gs:disable 'NewButton 'ClearButton 'LoadButton 'SaveButton 
				'CutButton 'CopyButton 'PasteButton 
				'ExecButton 'RestartButton 'FindButton 'FontBookButton)
		(gs:set-editable  currentEdit nil)
)

;;;;;;;;;;;;;;;;;;;;;;;; end auxiliary process handling ;;;;;;;;;;;;;;;;;;;;;;;

;; clear bottom monitor area
(define (viewclearmonitor-handler)
	(gs:clear-text 'OutputArea)
	;(gs:disable 'ViewClearMonitor)
)

;; output to monitor area
(define (output-monitor str)
	(gs:append-text 'OutputArea str)
	(gs:enable 'ViewClearMonitor)
	(gs:eval-shell 'OutputArea "\n")
)

;; dtach/attach toolbar
(define (viewtoolbar-handler id flag)
	(if flag
		(begin
			(set 'currentToolbarShow "yes")
			(gs:add-to 'TheEditor 'ToolBar "north")
			; if the toolbar was not visible on startup
			; it will not be visible now, inspite of layout
			; this forces components of the container to be redrawn
			(gs:set-visible 'TheEditor true)
			(gs:layout 'TheEditor)
		)
		(begin
			(set 'currentToolbarShow "no")
			(gs:remove-from 'TheEditor 'ToolBar)	
			(gs:layout 'TheEditor)
		)
	)
)

;; syntax highlighting and themes 1,2,3
;; for menu-item theme-handler function
;; see beginning of file

(define (viewsyntax-handler id flag)
	(if flag
		(begin
			(set 'currentSyntaxStatus (current-file-syntax))
			(if (not currentSyntaxStatus)
					(begin
						(gs:set-selected 'ViewSyntax nil)
						(gs:show-popup 'SyntaxMenu 'TheEditor 100 100))
					(gs:set-syntax currentEdit currentSyntaxStatus)))
		(begin
			(set 'currentSyntaxStatus nil)
			(gs:set-syntax currentEdit nil))
	)
)

(define (syntaxmenu-handler id idx)
	(gs:set-syntax currentEdit (set 'currentSyntaxStatus 
		(case id
			("MAIN:SyntaxNewlisp" "lsp")
			("MAIN:SyntaxC" "c")
			("MAIN:SyntaxCPP" "cpp")
			("MAIN:SyntaxJava" "java")
			("MAIN:SyntaxPHP" "php")
		))
	)
	(gs:set-selected 'ViewSyntax (true? currentSyntaxStatus))
)

;; handle character and caret events from edit area
(define (editarea-handler id code mods dot mark len undo redo)
	(if undo (gs:enable 'EditUndo) (gs:disable 'EditUndo))
	(if redo (gs:enable 'EditRedo) (gs:disable 'EditRedo))
	(set 'currentDot dot 'currentMark mark)
	;(println code ":" mods)
	(if (= code 65535) ; crtl or meta keys with or w/o shift
		(begin
			; caret movement only
			(if (not is-selection)
				(when (!= dot mark) ; selection started
						(gs:enable 'CutButton 'CopyButton 'EditCut 'EditCutP 'EditCopy 'EditCopyP)
						(set 'is-selection true))
				(when (= dot mark)  ; de-selected
						(gs:disable 'CutButton 'CopyButton 'EditCut 'EditCutP 'EditCopy 'EditCopyP)
						(set 'is-selection nil))
			)
			; cmd-z or cmd-Z (undo, redo)
			(if (or (= code 256) (= code 320) (= code 128) (= code 192))
				(set-buffer-dirty))	
		)
		; character typed
		(if edit-buffer-clean
			(when (or 	(<  mods 128) 
						(and (= mods 256) (or (= code 118) (= code 120)))
						(and (= mods 128) (or (= code 24) (= code 22))))
				(set-buffer-dirty)))
	)			
)

;; handle mouse clicks from editeara for popup menu
(define (editarea-mouse-handler id type x y button cnt modifiers)
	(gs:set-text 'FontSizeLabel (string currentFontSize))
	(set 'cursor-in-outputarea nil)
	(if (or (= button 3) (= modifiers 18)); right button or ctrl click
		(gs:show-popup 'EditMenuPopup currentEdit x y)
	)
)


(define (outputarea-mouse-handler)
	(gs:set-text 'FontSizeLabel (string currentMonitorFontSize))
	(set 'cursor-in-outputarea true)
)

;; tabs have switched or a new tab has been inserted
(define (editortabs-handler id tab title idx)
;(println "id:" id " tab:" tab " title:" title " idx:" idx)
	; update statis of previous tab if it still exists
	(if (assoc tab tabs-stack) (update-current-tab))
	(set 'currentTabIndex idx)
	; get new tab edit area settings
	(switch-to-tab tab)
)

(define (switch-to-tab tab)
;(println "in switch-to-tab")
;(println "currentTabIndex:" currentTabIndex)
	(set 'currentEdit tab)
;(println (assoc currentEdit tabs-stack))
	(set 'currentDir (lookup currentEdit tabs-stack 1))
	(set 'currentFile (lookup currentEdit tabs-stack 2)) 
	(set 'currentPath (string currentDir "/" currentFile))
	(set 'currentStatus (lookup currentEdit tabs-stack 3))
	(set 'edit-buffer-clean (currentStatus 0))
	(if edit-buffer-clean
		(begin
			(gs:set-icon 'EditorTabs "/local/green10.png" currentTabIndex)
			(gs:disable 'FileSave 'SaveButton)
		)
		(begin
			(gs:set-icon 'EditorTabs "/local/red10.png" currentTabIndex)
			(gs:enable 'FileSave 'SaveButton)
		)
	)
	(set 'currentDot (currentStatus 1))
	(set 'currentMark (currentStatus 2))
	(set 'currentSyntaxStatus (currentStatus 3))
	(if (= currentDot currentMark) 
		(begin
			(set 'is-selection nil)
			(gs:disable 'CutButton 'CopyButton 'EditCut 'EditCutP 'EditCopy 'EditCopyP)
			(gs:request-focus currentEdit) )
		(begin
			(set 'is-selection true)
			(gs:enable 'CutButton 'CopyButton 'EditCut 'EditCutP 'EditCopy 'EditCopyP)
			(gs:request-focus currentEdit)
			(gs:select-text currentEdit currentDot currentMark) )
	)
	(gs:set-text 'TheEditor (string "newLISP edit - " currentPath))
	(theme-handler (string "ViewTheme" currentThemeIdx))
	(gs:set-font currentEdit currentFontName currentFontSize "plain")
)

(define (update-current-tab)
	(set 'currentStatus (list edit-buffer-clean currentDot currentMark currentSyntaxStatus))
	; save previous tab edit area settings
	(if (assoc currentEdit tabs-stack)
		(setf (assoc currentEdit tabs-stack) 
			(list currentEdit currentDir currentFile currentStatus)) )
)

;; help about box
;; on Mac OS X the built-in about box is shown (contained in guiserver.jar)
;; selectable from the Apple system menu
;; On other OSs the Help menu contains the following (identical loooking)
;; about box

(define (helpabout-handler)
	(if (!= ostype "OSX")
		(begin
			(gs:get-version)
			(gs:message-dialog 'TheEditor (string "newLISP-GS v." gs:version)
				(string "Software: copyright (c) 2007-15 Lutz Mueller http://newlisp.org\n" 
						"Icons: copyright (c) 2007-15 Michael Michaels http://neglook.com\n"
						"All rights reserved.")
				"information" "/local/newLISP64.png" )
		)
	)
)

;; show newLISP Users Manual and Reference

(define (helpmanual-handler)
	(load-platform-help "/manual_frame.html")
)

;; show GS Manual

(define (helpguiserver-handler)
	(load-platform-help "/guiserver/index-gs.html")
)

; help menu items for HTNL documentaion have been taken out because
; only on Mac OSX they will not block the IDE. On Windows and Linux
; the IDE stops working until the browser window is closed.

(define (load-platform-help file-name , prog files)
	(if (not (file? (string newlispDoc file-name)))
		(gs:message-dialog 'TheEditor "Display documentation"
				(string "Cannot find file: " newlispDoc file-name)
                "warning"))
	(cond
		; Mac OS X
		((= ostype "OSX")
			(exec (string "open file://" newlispDoc file-name))
		)
		; MS Windows
		((= ostype "Windows")
			(begin
				(set 'prog (string "cmd /c \"" (env "PROGRAMFILES") "/Internet Explorer/IEXPLORE.EXE\""))
				;(println "->" prog "<-")
				(exec (string prog " file://" newlispDoc file-name)))
		)
		; all other UNIX
		(true 
			(set 'files '(
					"/usr/bin/sensible-browser"
					"/usr/bin/x-www-browser"
					"/usr/bin/mozilla"
					"/usr/bin/firefox"
					"/usr/bin/konqueror"
				))
			(set 'prog (find true (map file? files)))
			(if prog
				(exec (string (files prog) " file://" newlispDoc file-name))
				(gs:message-dialog 'TheEditor "Display documentation"
						"Cannot find browser to display documentation" "warning")
			)
		)
	)
)

;; start listening for GUI events and output from auxiliary newLISP process
;; append out put from newLISP process to monitor area

(while (gs:check-event 10000)
	(if (and console (net-select console "read" 10000))
		(begin
			(if (> (net-peek console) 0) (begin
				(net-receive console response 10024)
				(output-monitor (or response ""))
				(sleep 100)
				))
			(check-status)
		)
	)
)

;; eof
