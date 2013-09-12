#!/usr/bin/newlisp
;;
;; widgets-demo.lsp - demonstrate all widgets


;;;; initialization
(set-locale "C")
(load (append (env "NEWLISPDIR") "/guiserver.lsp")) 

(gs:init) 

;(gs:set-trace true)

;(gs:set-look-and-feel "com.sun.java.swing.plaf.motif.MotifLookAndFeel")
;(gs:set-look-and-feel "javax.swing.plaf.metal.MetalLookAndFeel")
;(gs:set-look-and-feel "com.sun.java.swing.plaf.windows.WindowsLookAndFeel") ;????
;(gs:set-look-and-feel "javax.swing.plaf.mac.MacLookAndFeel") ; ????

;;;; describe the GUI
(gs:frame 'WidgetsDemo 200 40 640 640 "демо ГУИ-элементов")
(gs:set-grid-layout 'WidgetsDemo 5 1 10 1)

;; the monitor area
(gs:text-area 'MonitorArea 'action-handler)
(gs:set-editable 'MonitorArea nil)
(gs:set-background 'MonitorArea 0.5 0.5 0.5)
(gs:set-font 'MonitorArea "Monospaced" 12 "plain")
(gs:set-foreground 'MonitorArea 1 1 0.0)

;; the button panel
(gs:panel 'ButtonPanel)
(gs:set-flow-layout 'ButtonPanel "left" 10 10)
(gs:set-titled-border 'ButtonPanel "кнопки")
(gs:button 'TheButton 'button-handler "кнопка")
;(gs:set-color 'TheButton 1 0 0) ; just for testing
(gs:image-button 'TheImageButton 'action-handler "/local/newLISP32.png")
(gs:toggle-button 'TheToggleButton 'action-handler "тумблер")
;(gs:set-selected 'TheToggleButton true)
(gs:radio-button 'TheRadioButton 'action-handler "выбрать")
;(gs:set-selected 'TheRadioButton true)
(gs:check-box 'TheCheckBox 'action-handler "отметить")
(gs:set-selected 'TheCheckBox true)
(gs:button 'TheMessage 'action-handler "нажать")
(gs:add-to 'ButtonPanel 'TheButton 'TheImageButton 'TheToggleButton 'TheRadioButton 'TheCheckBox 'TheMessage)

;; list and combo boxes
(gs:panel 'ListPanel)
(gs:set-flow-layout 'ListPanel "center"  50 1)
(gs:set-titled-border 'ListPanel "комбо и списки")
(gs:combo-box 'TheComboBox 'action-handler "один" "два" "три")
(gs:select-list-item 'TheComboBox "два")
(gs:set-background 'TheComboBox 1.0 0.9 0.9)
(gs:set-foreground 'TheComboBox 0 0 1)
(gs:set-font 'TheComboBox "Lucida Serif Typewriter" 14 "plain")
(gs:list-box 'TheListBox 'action-handler "первое" "второе" "третье")
(gs:set-background 'TheListBox 1.0 0.9 0.9)
(gs:set-foreground 'TheListBox 0 0 1)
(gs:set-size 'TheListBox 200 80)
(gs:add-list-item 'TheListBox "четвертое" "пятое" "шестое" "седьмое")
(gs:add-list-item 'TheListBox "восьмое")
(gs:insert-list-item 'TheListBox "щелкнуть дважды" 2)
;(gs:remove-list-item 'TheListBox 2) ; just for testing
(gs:add-to 'ListPanel 'TheComboBox 'TheListBox )

;; text entry widgets
(gs:panel 'TextPanel)
(gs:set-flow-layout 'TextPanel "left" 10 1)
(gs:set-titled-border 'TextPanel "текстовое поле и текстовый ввод")
(gs:text-field 'TheTextField 'textfield-handler 10)
(gs:set-background 'TheTextField 0.7 0.8 0.8)
(gs:text-area 'TheTextArea 'textarea-handler 160 80)
(gs:set-background 'TheTextArea 0.7 0.8 0.8)
(gs:button 'GetTextFieldButton 'gettextfield-handler "получить текст")
(gs:button 'GetTextAreaButton 'gettextarea-handler "получить текст")
(gs:add-to 'TextPanel 'TheTextField 'GetTextFieldButton 'TheTextArea 'GetTextAreaButton)

;; slider, progress-bar and scroll-area
(gs:panel 'SlidePanel)
(gs:set-titled-border 'SlidePanel "движок, индикатор загрузки, прокрутки панели")
(gs:set-flow-layout 'SlidePanel "left" 20 1)
(gs:slider 'TheSlider 'slider-handler "горизонтальны" 1 100 30)
(gs:label 'SliderStatus "30" "right" 30 10)
(gs:progress-bar 'TheProgress 1 100 30)
(gs:image-label 'TheLogo "/local/newLISP128.png")
(gs:scroll-pane 'TheScrollPane 'TheLogo 100 90)

(gs:add-to 'SlidePanel 'TheSlider 'SliderStatus 'TheProgress 'TheScrollPane)


;; add all panels to the grid laount in the main frame
(gs:add-to 'WidgetsDemo 'MonitorArea 'ButtonPanel 'ListPanel 'TextPanel 'SlidePanel)

(gs:set-visible 'WidgetsDemo true)

;;;; define actions

(define (button-handler)
	(gs:dialog 'TheDialog 'WidgetsDemo "окно-диалог" 300 200 true true))


;; several widgets can be served by one handler
;; the first parameter is always the source of the event
(define (action-handler)
	(if (= "MAIN:TheToggleButton" (args 0))
		(if (true? (args 1))
			(gs:disable 'TheButton 'TheImageButton 'TheRadioButton 'TheCheckBox 'TheMessage)
			(gs:enable 'TheButton 'TheImageButton 'TheRadioButton 'TheCheckBox 'TheMessage)
		))

	(if (= "MAIN:TheImageButton" (args 0))
		(gs:color-dialog 'WidgetsDemo 'action-handler "выберите цвет" 1 1 1))

	(if (= "MAIN:TheMessage" (args 0))
		(gs:confirm-dialog 'WidgetsDemo 'action-handler 
				"сообщение" "приятного пользования ГУИ-сервером" 
				"да-нет"
				;(amb "error" "informaton" "warning" "question" "plain")
		))

	(let ( (out-text (list (string (args 0)))) (len (length (args))))
		(if (> len 1) (push (string (args 1)) out-text -1))
		(if (> len 2) (push (base64-dec (string (args 2))) out-text -1))
		(if (> len 3) (push (string (args 3)) out-text -1))
		(push "\n" out-text -1)
		(gs:append-text 'MonitorArea (join out-text " ")))
)

(define (textfield-handler id text)
	(gs:append-text 'MonitorArea (base64-dec text))
	(gs:append-text 'MonitorArea "\n")
	(gs:add-list-item 'TheListBox (base64-dec text))
	(gs:add-list-item 'TheComboBox (base64-dec text))
)

(define (textarea-handler)) ; not interested in keystrokes

(define (gettextarea-handler id text)
	(gs:get-text 'TheTextArea 'gettextcallback-handler)
)

(define (gettextfield-handler id text)
	(gs:get-text 'TheTextField 'gettextcallback-handler)
)

(define (gettextcallback-handler id text)
	(if text
		(begin
			(gs:append-text 'MonitorArea (base64-dec text))
			(gs:append-text 'MonitorArea "\n"))
	)
)

(define (slider-handler id value)
	(gs:set-text 'SliderStatus (string value))
	(gs:set-value 'TheProgress value)
)

;;;; listen for incoming action requests and dispatch
(gs:listen)

;; eof

