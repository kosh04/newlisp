;; @module guiserver.lsp
;; @description Functions for programming GUIs and 2D graphics.
;; @version 1.40 use text-field as a password field with additional parameter
;; @version 1.41 bug fixes for gs:listen and gs:check-event
;; @version 1.42 new table UI
;; @version 1.43 bug fix in new table UI action parameters
;; @version 1.44 fixes in newlisp-edit.lsp
;; @version 1.50 doc fixes
;; @version 1.51 return value for gs:export
;; @version 1.52 fix in run-shell for Java 7 update 21
;; @version 1.53 doc fixes
;; @version 1.60 new table functions, new naming gs:table-show-row-number
;; @version 1.61 more options for gs:scroll-pane added by FdB
;; @version 1.62 doc corrections
;; @version 1.63 make deprecated gs:table-set-row-number work
;; @version 1.70 default comm port with Guiserver are now 64001 and 64002
;; @author LM, 2008, 2009, 2010, 2015, Unya 2012, FdB 2013
;;
;; This module has been tested on MacOS X 10.5 (Leopard) and Windows XP, both with the
;; Standard SUN Java RE v.1.5 (runtime environment) which came pre-installed on
;; those platforms. On Linux the installation of the original Sun Java Runtime 
;; Environment is required the preinstalled GNU Java is not compatible. After
;; installation a soft-link has to be made from the original java executable to
;; '/usr/bin/java'.
;; 
;;
;; On Windows the MIDI sound features require a soundbank file to
;; be installed. See the description for 'gs:play-note' for details.
;; <br><br>
;; <h2>What is newLISP-GS</h2>
;; 'guiserver.lsp' is a module for interfacing to 'guiserver.jar'
;; a Java server application for generating GUIs (graphical user interfaces)
;; and 2D graphics for newLISP applications. The 'guiserver.lsp', module
;; implements a newLISP API much smaller and more abstract than the APIs of the 
;; Java Swing libraries which it interfaces with. Because of this, GUI applications
;; can be built much faster than when using the original Java APIs.
;; <br><br>
;; <h2>Usage</h2>
;; At the beginning of the program file, include a 'load' statement for the module:
;; <pre>
;; (load "/usr/share/newlisp/guiserver.lsp")
;; </pre>
;; or on MS Windows:
;; <pre>
;; (load "c:/Program Files/newlisp/guiserver.lsp")
;; </pre>
;; 'guiserver.lsp' expects the server 'guiserver.jar' to be
;; in the directoey specified in the environment variable NEWLISPDIR.
;; When newLISP starts up and this variable is not set yet, it sets it
;; to a default value of '/usr/share/newlisp' on MacOS X and Unix OSs, and 
;; to 'C:\Program Files\newlisp' or whatever it finds in the 'PROGRAMFILES'
;; environment variable on MS Windows systems and adding '/newlisp' to it.
;; This can be overwritten by specifying system wide  setting for the environment 
;; variable <tt>NEWLISPDIR</tt>, which normally is set to '%PROGRAMFILES%/newlisp' 
;; on MS Windows. When using the MS Windows binary installer 'NEWLISPDIR' is written 
;; to the registry automatically and gets into effect after rebooting.
;; <br><br>
;; <h2>Architecture of a newLISP GUI application</h2>
;; A GUI application in newLISP is composed of four parts:
;;
;; <blockquote>
;; <b>initialization</b> - this means starting the newLISP-GS 'guiserver.jar' and initializing 
;; communications with it. Only one function call is required to do this.
;;
;; <b>building widgets</b> - in this step windows, buttons, text fields etc., and
;; all visual aspects of the GUI are described. newLISP newLISP-GS offers a wide range
;; of different control widgets.
;;
;; <b>defining event actions</b> - in this step all the functions are defined to
;; react to events coming from the GUI as a consequence of button pushes, keystrokes,
;; mouse-movements etc.. These event actions send many commands back to the GUI
;; to change information for the user, popup dialogs etc..
;;
;; <b>listening for events</b> - the newLISP program sits in a loop waiting for
;; events and dispatching them to the defined event actions. Only one function call
;; is required for this step.
;; </blockquote>
;; <br><br>
;; <h2>Example</h2>
;; The following example application shows all the essential elements of a newLISP GUI
;; application:
;;
;; @example
;; #!/usr/bin/newlisp
;; ; button-demo.lsp - demonstrate the button control
;;  
;; ; initialization
;; (load (append (env "NEWLISPDIR") "/guiserver.lsp")) 
;;
;; (gs:init) 
;;  
;; ; describe the GUI
;; (gs:frame 'ButtonDemo 100 100 400 300 "Button demo")
;; (gs:set-resizable 'ButtonDemo nil)
;; (gs:panel 'ColorPanel 360 200)
;; (gs:set-color 'ColorPanel (random) (random) (random))
;; (gs:button 'aButton 'abutton-action "color")
;; (gs:set-flow-layout 'ButtonDemo "center" 2 15)
;; (gs:add-to 'ButtonDemo 'ColorPanel 'aButton)
;; (gs:set-visible 'ButtonDemo true)
;;  
;; ; define actions
;; (define (abutton-action id)
;;     (gs:set-color 'ColorPanel (random) (random) (random)))
;;  
;; ; listen for incoming action requests and dispatch
;; (gs:listen)
;;  
;; ; eof 

;; <br>
;; <h2>Application start</h2>
;; <pre>
;;     ./button-demo       ; on MacOS X and Unix
;;     newlisp button-demo ; on MS Windows
;; </pre>
;; By default guiserver.jar uses the ports 64001 and 64002, but this setting can be overwritten 
;; either by supplying a port number parameter to the 'gs:init' function or by overwriting the
;; port number from the command-line. newLISP-GS will then use the port number supplied and the number
;; following it:
;; <pre>
;;     ./button-demo 10001       ; on MacOS X and Unix
;;     newlisp button-demo 10001 ; on MS Windows
;; </pre>
;; newLISP-GS 'guiserver.jar' will now use the ports '10001' and '10002'.
;; Ports under <tt>1024</tt> should not be used, as many of them are already in use by other
;; OS services and need administrator privileges to use them.
;;
;; A second method to start a newLISP-GS application starts the 'guiserver.jar' first, which then
;; starts the newLISP application:
;; <pre>
;;     java -jar /usr/share/newlisp/guiserver.jar 64001 /usr/home/aUser/MyApplication.lsp
;; </pre>
;; A different port number can be used. Port numbers below 1024 need administrator
;; permissions. Optionally a splash screen can be specified as the last parameter:
;; <pre>
;;     java -jar /usr/share/newlisp/guiserver.jar 64001 /home/apps/myapp.lsp /local/newLISP128.png
;; </pre>
;; The example specifies an image inside 'guiserver.jar'. Any other image path on the local file system
;; can be used.
;;
;; On MS Windows similar methods can be used replacing the appropriate file paths, but on MS Windows Java jar files
;; can also be treated as executables and executed directly without calling Java explicitly. By default
;; 'guiserver.jar' and 'guiserver.lsp' are  installed in 'c:\Program Files\newlisp\' or any other 
;; directory configured on a MS Windows platform using the 'PROGRAMFILES' environment variable:
;; <pre>
;;    "c:\Program Files\newlisp\guiserver.jar" 64001 c:\myprogs\MyApplication.lsp
;; </pre>
;; Quotes are necessary when spaces are present in the argument string. The example assumes that
;; 'newlisp.exe' is in the path for executables, and it also assumes that the Windows registry has
;; an association of the <tt>.jar</tt> file extension with the <tt>javaw.exe</tt> executable. This
;; association is normally present when a java run-time environment (JRE) is installed in Windows.
;; If this association is not registered, the following method can be used:
;; <pre>
;;    javaw -jar "c:\Program Files\newlisp\guiserver.jar" 64001 c:\myprogs\MyApplication.lsp
;; </pre>
;; The quotes are necessary for path-names containing spaces.
;;
;; <h2>Debugging</h2>
;; <b>Tracing commands to newLISP-GS</b><br><br>
;; For debugging purpose put the following directive at the beginning of your application
;; or at the place from where to start tracing.
;; <pre>
;;         (gs:set-trace true)
;; </pre>
;; Then start the application from a terminal or command shell window. Now newLISP-GS
;; will output startup, version and connection messages and a trace for each 'gs:xxs' directive
;; as it is received by the newLISP-GS dispatcher:
;;
;; <blockquote><pre>
;; newLISP-GS v.0.94
;; listening on 64001
;; accepted from 0.0.0.0
;; connecting to 0.0.0.0 64002
;; retrying to connect
;; connected
;; -> frame MAIN:ButtonDemo 100 100 400 300 QnV0dG9uIGRlbW8= nil
;; -> set-resizable MAIN:ButtonDemo nil
;; -> panel MAIN:ColorPanel 360 200
;; -> set-color MAIN:ColorPanel 0 1 0 0.2
;; -> button MAIN:aButton MAIN:abutton-action Y29sb3I=
;; -> set-flow-layout MAIN:ButtonDemo center 2 15
;; -> add-to MAIN:ButtonDemo MAIN:ColorPanel MAIN:aButton 
;; -> set-visible MAIN:ButtonDemo true
;; -> set-color MAIN:ColorPanel 0.8401877172 0.3943829268 0.7830992238
;; server shut down
;; </blockquote></pre>
;;
;; Text strings for button names, icon paths and other texts are encode in
;; Base64 strings as the first trace line for MAIN:ButtonDemo shows. To switch
;; off tracing mode use:
;; <pre>
;;        (gs:set-trace nil)
;; </pre>
;; Even if trace mode is switched off, wrong or missing parameters are still messaged
;; by newLISP-GS in a small message box. After such an error the application and guiserver
;; will exit. Unknown commands will be ignored. Functions which are not applicable to
;; certain widgets will also pop up an error message box. In certain situations a
;; function will have no effect, e.g. 'gs:set-size' or 'gs:set-color' sometimes do not
;; have an effect, depending on how a widget is configured or depending on the layout
;; which hosts the widget. Sometimes the platform look-and-feel overwrites colors.
;; <br><br>
;; <h2>Event handlers</h2>
;; For most widgets, event handlers must be defined. Sometimes an event handler is
;; not required. In this case specify <tt>'gs:no-action</tt> as the event handler
;; symbol. When developing programs it is useful to watch the event handler first
;; before coding for it. This can be done easily by printing out event parameters:
;; <blockquote><pre>
;; (gs:button 'aButton 'abutton-handler "press")
;; (define (abutton-handler id)
;;     (println id))
;; </pre></blockquote>
;; Sometimes the same event handler function is attached to several widgets' keyboard
;; or mouse events. Some of these events receive a greater number of parameters. There
;; are two easy ways to discover the nature of an event:
;; <blockquote><pre>
;; (define (the-handler)
;;     (doargs (p)
;;         (println "->" p)))
;; </pre></blockquote>
;; The other method looks at the source of the event as it was transmitted by the newLISP-GS.
;; This is useful to recognize the data types used in the event:
;; <blockquote><pre>
;; (define (the-handler)
;;     (println gs:event))
;; </pre></blockquote>
;; All text from text fields are received as base64-encoded strings. E.g. the text: 
;; '"Hello World"' would be received as: '"SGVsbG8gV29ybGQ="':
;; <blockquote><pre>
;; (gs:text-field 'TextField 'textfield-handler)
;;
;; (define (textfield-handler id text)
;;     (printnl id ": " (base64-dec text)))
;; </pre></blockquote>
;; When the text "Hello World" is entered in the text field, the following output
;; would be generated:
;; <blockquote><pre>
;; TextField: "Hello World"
;; </pre></blockquote>
;; In case the ESC key is pressed in the text field, the event handler would
;; report 'nil' for the text field. A handler should therefore always check text
;; for string contents before trying to apply the 'base64-dec' function on it.
;; <br><br>
;; <h2>Mapping or applying 'gs:xxx' functions</h2>
;; Like any newLISP functions, 'gs:xxx' functions can be mapped or applied to lists of 
;; parameters using the newLISP 'map' and 'apply' functions. When doing this, make sure to 
;; map or apply the quoted <tt>'gs:xxx symbol</tt>, so the <tt>gs:xx</tt> functions 
;; get executed under the <tt>gs</tt> context, prefixing symbols in parameter lists
;; correctly with the context prefix of their origin.
;; <pre>
;;    (map 'gs:panel '(first second third fourth)) ; note quoted gs: function
;; </pre>
;; <h2>Some shortcuts when writing 'gs:xxx' functions</h2>
;; Due to the nature of transfer between newLISP and the guiserver as text, the following
;; convenient shortcuts can be taken when writing functions:
;; <blockquote>
;; <ul>
;; <li>Symbol ids of components can be expressed as strings.</li>
;; <li>Number values can be expressed as strings.</li>
;; <li>Numbers can be expressed as floats or integers.</li>
;; </ul>
;; </blockquote>
;; Here are some examples:
;; <pre>
;;     (gs:panel 'ColorPanel 360 200)
;;     ; is the same as
;;     (gs:panel "ColorPanel" 360 200)
;;     ; is the same as
;;     (gs:panel "ColorPanel" "360" "200")
;;     ; is the same as
;;     (gs:panel "ColorPanel" 360.0 "200.00")
;; </pre>
;; Although the first form is preferred for clarity and readability, in some cases coding
;; may be more efficient using the other forms.
;;
;; Except for the symbols used for action handlers, all symbols are used only by their
;; face (term) value. This means that reserved symbols of the newLISP programming
;; language can be used freely in symbol ids for all components, e.g:
;; <pre>
;;     (gs:label 'term "Input here") ; term is a reserved name since v10.2.0
;; </pre>
;; The usage of the reserved symbol 'term' will not pose a problem.
;; <br><br>
;; <h2>Return values</h2>
;; newLISP-GS is an event driven asynchronous system. Most functions return
;; right away the number of characters sent out to the server.
;; In general, return values of 'gs:xxx' functions do not have
;; any specific meaning and can be discarded. Only the functions 'gs:get-bounds',
;; 'gs:get-fonts', 'gs:get-font-metrics', 'gs:get-instruments'
;; 'gs:get-screen', 'gs:get-text' and 'gs:get-version' return meaningful values or
;; lists of values, which are stored in similar named variables: 'gs:bounds',
;; 'gs:fonts', 'gs:font-metrics', 'gs:instruments', 'gs:screen', 'gs:text' and
;; 'gs:version'. These functions will not return right away but block until
;; the return valuse is sent back from newLISP-GS.
;;
;; The function 'gs:get-text' can work both ways: event driven or with a return
;; value depending on the call pattern used.
;; <br><br>
;; <h2>Function overview</h2>
;; <ul>
;; <li><b>Initialization and application setup</b><br>
;; <pre>
;;    (gs:init [<server-port>])
;; </pre>
;; The initialization function starts <tt>guiserver.jar</tt> which will listen to the <i>server-port</i>
;; and initiate another connection on <tt>server-port + 1</tt> back to newLISP. If a <server-port>
;; is not supplied <tt>guiserver</tt> will assume <tt>64001</tt> and <tt>64002</tt>.
;;
;; As the last statement in the application put:
;; <pre>
;;    (gs:listen)
;; </pre>
;; This function listens on <tt>64002</tt> (by default) for event messages from newLISP-GS
;; and dispatches them to the user-defined action handlers. To avoid newLISP shutting down
;; when the guiserver shuts down, use:
;; <pre>
;;    (gs:listen true)
;; </pre>
;; </li>
;; Sometimes it is necessary to run other tasks while listening for events. In this case use
;; 'gs:check-event', which will wait for certain amount of microseconds for an event
;; to be executed. After the wait-time, it returns. The function is typically used in a loop:
;; <pre>
;;    (while (gs:check-event 10000) ; check for 10 milli seconds
;;			(do-myprocess))
;;    (exit)
;; </pre>
;; The loop will exit when 'gs:check-event' returns 'nil' on communications errors, e.g.
;; when the window's close button was clicked.
;;
;; <li><b>Containers</b><br>
;; A <em>container</em> can contain any other container or control widget. Except for the 
;; <tt>menu-bar</tt> and the <tt>split-pane</tt>, containers can have a special layout-manager set 
;; with one of the three layout-manager function commands. By default containers have a flow layout. By
;; nesting different containers and using different layout-manager settings, complex layouts
;; can be configured. The function/command <tt>add-to</tt> is used to add components to containers.
;; <pre>
;;    (gs:dialog <sym-id> <sym-parent-frame> <str-message> <int-width> <int-height> [<boolean-visible> [<boolean-modal>]])
;;    (gs:frame <sym-id> <int-x> <int-y> <int-width> <int-height> [<str-title> <boolean-visible>])
;;    (gs:menu-bar <sym-frame> [<sym-menu-1> ...])  
;;    (gs:panel <sym-id> [<int-width> <int-height>])
;;    (gs:scroll-pane <sym-id> <sym-widget> [<int-width> <int-height>])
;;    (gs:split-pane <sym-id> <str-orientation> [<float-weight> [<float-location> [int-divider-size>]]])
;;    (gs:tabbed-pane <sym-id> <sym-action> <str-orientation> [<sym-widget> <sym-tab-title> ...])
;;    (gs:tool-bar <sym-frame> [<bool-floatable> <int-hgap> <int-vgap>])
;;    (gs:canvas <sym-id>) 
;;    (gs:window <sym-id> <int-x> <int-y> <int-width> <int-height>)
;; </pre>
;; </li>
;; <li><b>Labels</b><br>
;; Labels can have text or an image or both. A normal text <tt>label</tt> can have an icon
;; added to it and a <tt>image-label</tt> can have text added to it. Labels don't initiate
;; actions and can be placed in any container like all button-type widgets - buttons, checkboxes
;; and menu items. A basic set of icon images is built into <tt>guiserver.jar</tt>,
;; but user-supplied images and icons in <tt>.jpg</tt>, <tt>.png</tt> and <tt>.gif</tt> formats
;; can be used.
;; <pre>
;;    (gs:label <sym-id> <str-text> [<str-align> [<int-width> <int-height>]])
;;    (gs:image-label <sym-id> <str-icon-path> [<str-align>])
;; </pre>
;; </li>
;; <li><b>Control widgets</b><br>
;; Except for the passive progress bar, all control widgets fire action requests to
;; the newLISP program. These requests must be served to avoid error messages but can
;; be defined as empty functions, if an action is not required:
;; <pre>
;;    ; empty action definition
;;    (define (my-button-action) )
;;
;;    ; action handler printing the name of the button pressed
;;    (define (my-button-action id) (println id " has been pressed"))
;; </pre>
;; All action events calls carry information about the widget, that initiated that the event,
;; and event parameters like keystrokes, slider positions etc..
;; <pre>
;;    (gs:button <sym-id> <sym-action> [<str-text> [<int-width> <int-height>]])
;;    (gs:check-box <sym-id> <sym-action> [<str-text> [<bool-selected>]])
;;    (gs:combo-box <sym-id> <sym-action> [<str-item-1> ...])
;;    (gs:combo-box <sym-id> <sym-action> [<list-str-items>])
;;    (gs:image-button <sym-id> <sym-action> <str-icon-path> [<str-down-icon-path> [<int-width> <int-height>]])
;;    (gs:list-box <sym-id> <sym-action> [<str-item-1> ...])
;;    (gs:list-box <sym-id> <sym-action> [<list-str-items>])
;;    (gs:menu <sym-id> <str-text>)
;;    (gs:menu-popup <sym-id> <str-text>)
;;    (gs:menu-item <sym-id> <sym-action> <str-text>)
;;    (gs:menu-item-check <sym-id> <sym-action> <str-text> [<bool-selected>])
;;    (gs:progress-bar <sym-id> <int-min> <in-max> <int-initial-value>)
;;    (gs:radio-button <sym-id> <sym-action> [<str-text> [<bool-selected>]])
;;    (gs:slider <sym-id> <sym-action> <str-orientation> <int-min> <int-max> <int-initial-value>)
;;    (gs:text-area <sym-id> <sym-action> [<int-width> <int-height>])
;;    (gs:text-field <sym-id> <sym-action> <int-columns>[<str-echo-char>])
;;    (gs:text-pane <sym-id> <sym-action> <str-style> [<int-width> <int-height>])
;;    (gs:toggle-button <sym-id> <sym-action> [<str-text> <bool-selected>])
;; </pre>
;; For all button widgets, and the check box and menu-item widgets, icons can be set using 
;; the 'gs:set-icon' and 'gs:set-pressed-icon' functions.
;; </li><br>
;; <li><b>Placing components in containers</b><br>
;; For the flow and grid layouts the components are added in the sequence they are listed.
;; The grid-layout fills the grid row by row starting with the left most column. 
;; <pre>
;;    (gs:add-to <sym-container> <sym-component> [<sym-component ...])
;; </pre>
;; For the border layout an orientation parameter is specified as either <tt>"north"</tt>,
;; <tt>"west"</tt>, <tt>"center"</tt>, <tt>"east"</tt> or <tt>"south"</tt>.
;; <pre>
;;    (gs:add-to <sym-container> [<sym-component> <str-orientation> ...])
;; </pre>

;; </li>
;; <li><b>Summary of commands</b><br>
;; Most of the commands set special attributes of containers or control widgets. 
;; Not all functions can be applied to all containers and control widgets. A wrong
;; application will either pop up an error message box or do nothing.
;;
;; Some functions will work on certain widgets only in certain situations. For example
;; <tt>set-size</tt> will work not on components in a grid layout but on components
;; in a flow layout. Some widgets have a preset background color which cannot be changed
;; or is overwritten by the current 'gs:look-and-feel' settings.
;;	<pre>
;;    (gs:add-list-item <sym-list-combo> <str-text> [<str-text> ...])
;;    (gs:add-separator <sym-menu-tool-bar>)
;;    (gs:add-to <sym-container> <sym-component> [<sym-component ...])
;;    (gs:add-to <sym-container> <sym-component> <str-orientation> [<sym-component> <str-orientation> ...])
;;    (gs:append-text <sym-id> <str-text>)
;;    (gs:check-event <int-microseconds>)
;;    (gs:clear-list <sym-id>)
;;    (gs:clear-text <sym-id>)
;;    (gs:copy-text <sym-id>)
;;    (gs:cut-text <sym-id>)
;;    (gs:destroy-shell <sym-text-area>)
;;    (gs:disable <sym-id-1> [<sym-id-2> ...])
;;    (gs:dispose <sym-id>)
;;    (gs:dispose-splash)
;;    (gs:enable <sym-id-1> [<sym-id-2> ...])
;;    (gs:eval-shell <sym-text-area> <str-commmand>)
;;    (gs:find-text <sym-id> <str-text> <sym-action> [<str-direction>]])
;;    (gs:frame-closed <sym-id> <sym-action>)
;;    (gs:get-fonts)
;;    (gs:get-bounds <sym-id>)
;;    (gs:get-font-metrics <sym-id> <str-text>)
;;    (gs:get-screen)
;;    (gs:get-selected-text <sym-id> <sym-action>)
;;    (gs:get-text <sym-id> [<sym-action>])
;;    (gs:get-text-position <sym-id>)
;;    (gs:get-version);
;;    (gs:goto-text <sym-id> <sym-row> <sym-column>)
;;    (gs:insert-list-item <sym-list-combo> <str-text> <int-index> [<str-text> <int-index>])
;;    (gs:insert-tab <sym-tabbed-pane> <sym-component> [<str-text> [<int-index> [<str-icon-path>]])
;;    (gs:insert-text <sym-id> <str-text> <int-pos>)
;;    (gs:layout <sym-container>)
;;    (gs:load-text <sym-id> <str-path>)
;;    (gs:no-action)
;;    (gs:paste-text <sym-id> [<str-text>])
;;    (gs:redo-text <sym-id>)
;;    (gs:remove-from <sym-container> <sym-component> [<sym-component> ...])
;;    (gs:remove-list-item <sym-list-combo> <int-index> [<int-index> ...])
;;    (gs:remove-tab <sym-tabbed-pane> <int-index>)
;;    (gs:request-focus <sym-id>)
;;    (gs:run-shell <sym-text-area> <str-commmand> <str-args>)
;;    (gs:select-list-item <sym-id> <str-item> [<boolean-flag>])
;;    (gs:select-text <sym-id> <int-from> [<int-to>])
;;    (gs:set-accelerator <sym-menu-item> <str-keystroke>)
;;    (gs:set-background <sym-id> <float-red> <float-green> <float-blue> [<float-alpha>])
;;    (gs:set-background <sym-id> <list-rgb> [<float-alpha>])
;;    (gs:set-bevel-border <sym-panel> <str-type>)
;;    (gs:set-border-layout <sym-container>  [<int-hgap> <int-vgap>])
;;    (gs:set-caret <sym-id> <int-offset>)
;;    (gs:set-caret-color <sym-id> <list-rgb>)
;;    (gs:set-caret-color <sym-id> <float-red> <float-green> <float-blue>)
;;    (gs:set-color <sym-id> <float-red> <float-green> <float-blue> [<float-alpha>])
;;    (gs:set-color <sym-id> <list-rgb> [<float-alpha>])
;;    (gs:set-cursor <sym-id> <str-shape>)
;;    (gs:set-echo-char <sym-id> <str-echo-char>)
;;    (gs:set-editable <sym-id> <boolean-flag>)
;;    (gs:set-flow-layout <sym-container> [<str-alignment> [<int-hgap> <int-vgap>]])
;;    (gs:set-font <sym-id> <str-family> <int-size> <str-type>)
;;    (gs:set-foreground <sym-id> <float-red> <float-green> <float-blue> [<float-alpha>])
;;    (gs:set-foreground <sym-id> <list-rgb> [<float-alpha>])
;;    (gs:set-grid-layout <sym-container> <int-rows> <int-columns> [<int-hgap> <int-vgap>])
;;    (gs:set-icon <sym-id> <str-icon-path> [<int-index>])
;;    (gs:set-look-and-feel <str-look>)
;;    (gs:set-resizable <sym-frame> <boolean-flag>)
;;    (gs:set-pressed-icon <sym-id> <str-icon-path>)
;;    (gs:set-selected <sym-id> <boolean-flag>)
;;    (gs:set-size <sym-id> <int-width> <int-height>)
;;    (gs:set-selection-color <sym-id> <float-red> <float-green> <float-blue> [<float-alpha>])
;;    (gs:set-syntax <sym-id> <str-type>) 
;;    (gs:set-syntax-colors <list-rgb-comments> <list-rgb-keywords> <list-rgb-string> <list-rgb-number> <list-rgb-quoted> <list-rgb-prantheses>)
;;    (gs:set-tab-size <sym-id> <int-size>)
;;    (gs:set-text <sym-id> <str-text> [<int-index>])
;;    (gs:set-titled-border <sym-component> <str-title>)
;;    (gs:set-tool-tip <sym-id> <str-text>)
;;    (gs:set-trace <boolean-flag>)
;;    (gs:set-utf8 <boolean-flag>)
;;    (gs:set-value <sym-id> <int-value>)
;;    (gs:set-visible <sym-id> <boolean-visible>)
;;    (gs:undo-text <sym-id>)
;;    (gs:undo-enable <sym-id> <boolean-enabled>)
;; </pre>
;; </li>
;; <li><b>The Table UI</b><br>
;;  Since version 1.42 Guiserver has a table widget and supporting functions.
;; <pre>
;;    (gs:table <sym-id> <sym-action> [<str-column-header-name> ...])
;;    (gs:table-add-column <sym-id> <str-column-header-name> ...)
;;    (gs:table-add-row <sym-id> [<str-columns> ... ])
;;    (gs:table-get <sym-id>)
;;    (gs:table-get-cell <sym-id> <int-row> <int-column>)
;;    (gs:table-get-size <sym-id>)
;;    (gs:table-remove-row <sym-id> <int-row>
;;    (gs:table-set-cell <sym-id> <int-row> <int-column> <str-value>)
;;    (gs:table-set-column <sym-id> <int-column-number> <int-width> [<str-justification>])
;;    (gs:table-set-column-name <sym-id> [<str-name1> [<str-name2> ...])
;;    (gs:table-set-row-count <sym-id> <int-count>
;;    (gs:table-set-row-number <sym-id> <bool-row-number>) DEPRECATED use gs:table-show-row-number
;;    (gs:table-show-row-number <sym-id> <bool-row-number>)
;; </pre>
;; </li>
;; <li><b>Special dialogs</b><br>
;;  These are standard dialogs for opening and saving files and for choosing colors.
;;  Each dialog when closed fires an event for which a handler function must be
;;  defined by the newLISP program. 
;;	<pre>
;;    (gs:color-dialog <sym-parent-frame> <sym-action> <str-title> <float-red> <float-green> <float-blue>)
;;    (gs:message-dialog <sym-parent-frame> <str-title> <str-message> [<str-type> [<str-icon-path>]])
;;    (gs:confirm-dialog <sym-parent-frame> <sym-action> <str-title> <str-message> [<str-type>])
;;    (gs:open-file-dialog <sym-parent-frame> <sym-action> [<str-directory> [<str-mask> <str-description>]])
;;    (gs:save-file-dialog <sym-parent-frame> <sym-action> [<str-directory> [<str-initial-file> [<str-mask> <str-description>]]])
;; </pre>
;; </li>
;; <li><b>2D Graphics functions</b><br>
;; Every call to a 'gs:draw-xxx' or 'gs:fill-xxx' function will create a new graphics object, which
;; will persist until destroyed by a call to 'gs:delete-tag'. Graphics objects are animated
;; using the tag operations 'gs:move-tag', 'gs:rotate-tag', 'gs:scale-tag' and 
;; 'gs:shear-tag' or using 'gs:hide-tag' and 'gs:show-tag'. 
;;
;; Any 'gs:draw-xxx' or 'gs:fill-xxx' will create graphics objects but not force a screen update.
;; The canvas is automatically redrawn when made visible for the first time using 'gs:set-visible' or
;; after a call to 'gs:update'. Redrawing is also forced when resizing the window which hosts the canvas
;; or any action covering and uncovering the canvas. 
;; 
;; After all tag operations redrawing is initiated by default, but it can be forced off by specifying 
;; 'nil' as the last parameter in a 'gs:xxx-tag' command. Suppressing immediate redraw is useful to avoid 
;; flicker when a batch of tag operations is performed.
;; 
;; Every graphics object (line, shape, text, or image) carries a group tag. Objects are deleted
;; using 'gs:delete-tag' and will disappear immediately from the canvas. Using 'gs:move-tag'
;; lines, shapes, text and images can be moved to a different position.
;;
;; All positions given in <tt>int x</tt> and <tt>int y</tt> must be given as integers. Values
;; will not be converted automatically as is the case with newLISP's built-in functions. To
;; guarantee integer type, values can be casted e.g.: 
;; <pre>(gs:draw-circle 'MyCircle (int x) (int y) (int r))</pre>
;; <pre>
;;    (gs:color-tag <sym-tag> <list-rgb> [<boolean-repaint>])
;;    (gs:delete-tag <sym-tag>[<boolean-repaint>])
;;    (gs:draw-arc <sym-tag> <int-x> <int-y> <int-width> <int-height> <int-start> <int-angle> [<list-rgb>])
;;    (gs:draw-circle <sym-tag> <int-x> <int-y> <int-radius> [<list-rgb>])
;;    (gs:draw-ellipse <sym-tag> <int-x> <int-y> <int-radius-x> <int-radius-y> [<list-rgb>])
;;    (gs:draw-image <sym-tag> <str-path> <int-x> <int-y> [<int-width> <int-height>])
;;    (gs:draw-line <sym-tag> <int-x1> <int-y1> <int-x2> <int-y2> [<list-rgb>])
;;    (gs:draw-path <sym-tag> <list-points> [<list-rgb>])
;;    (gs:draw-polygon <sym-tag> <list-points> [<list-rgb>])
;;    (gs:draw-rect <sym-tag> <int-x> <int-y> <int-width> <int-height> [<list-rgb>])
;;    (gs:draw-round-rect <sym-tag> <int-x> <int-y> <int-width> <int-height> <int-arc-width> <int-arc-height> [<list-rgb>])
;;    (gs:draw-text <sym-tag> <str-text> <int-x> <int-y> [<list-rgb> [<float-angle>]])
;;    (gs:export <str-path-file> [<int-width> <int-height>])
;;    (gs:fill-arc <sym-tag> <int-x> <int-y> <int-width> <int-height> <int-start> <int-angle> [<list-rgb>])
;;    (gs:fill-circle <sym-tag> <int-x> <int-y> <int-radius> [<list-rgb>])
;;    (gs:fill-ellipse <sym-tag> <int-x> <int-y> <int-radius-x> <int-radius-y> [<list-rgb>])
;;    (gs:fill-polygon <sym-tag> <list-points> [<list-rgb>])
;;    (gs:fill-rect <sym-tag> <int-x> <int-y> <int-width> <int-height> [<list-rgb>])
;;    (gs:fill-round-rect <sym-tag> <int-x> <int-y> <int-width> <int-height> <int-arc-width> <int-arc-height> [<list-rgb>])
;;    (gs:hide-tag <sym-tag>  [<boolean-repaint>])
;;    (gs:move-tag <sym-tag> <int-dx> <int-dy> [<boolean-repaint>])
;;    (gs:reorder-tags <list-tags>)
;;    (gs:rotate-tag <sym-tag> <float theta> <int-x> <int-y> [<boolean-repaint>])
;;    (gs:save-text <sym-id> <str-path>)
;;    (gs:scale-tag <sym-tag> <float-x> <float-y> [<boolean-repaint>])
;;    (gs:shear-tag <sym-tag> <float-x> <float-y> [<boolean-repaint>])
;;    (gs:show-popup <sym-tag> <sym-host> <int-x> <int-y>)
;;    (gs:show-tag <sym-tag> [<boolean-repaint>])
;;    (gs:set-canvas <sym-tag>)
;;    (gs:set-paint <list-rgb>)
;;    (gs:set-rotation <float-angle>)
;;    (gs:set-scale <int-x> <int-y>)
;;    (gs:set-stroke <float-width> [<str-cap> [<str-join> [<float-miterlimit>]]])
;;    (gs:set-translation <int-x> <int-y>)
;;    (gs:set-anti-aliasing <boolean-flag>)
;;    (gs:translate-tag <sym-tag> <int-x> <int-y> [<boolean-repaint>])
;;    (gs:update)
;; </pre>
;; </li>
;; <li><b>Events</b><br>
;; Additionally to the event actions registered when creating a widget,
;; the canvas, windows and dialogs can fire events as a result of key or mouse
;; actions or when the position or size of a windows or dialog has changed.
;; <pre>
;;    (gs:key-event <sym-id> <sym-action>)
;;    (gs:mouse-clicked <sym-canvas> <sym-action> [<boolean-tags>])
;;    (gs:mouse-dragged <sym-canvas> <sym-action>)
;;    (gs:mouse-event <sym-id> <sym-action>)
;;    (gs:mouse-moved <sym-canvas> <sym-action> [<boolean-tags>])
;;    (gs:mouse-pressed <sym-canvas> <sym-action> [<boolean-tags>])
;;    (gs:mouse-released <sym-canvas> <sym-action> [<boolean-tags>])
;;    (gs:mouse-wheel <sym-canvas> <sym-action>)
;;    (gs:window-closed <sym-id> <sym-action>)
;;    (gs:window-moved <sym-id> <sym-action>)
;;    (gs:window-resized <sym-id> <sym-action>)
;; </pre>
;; </li>
;; <li><b>Built-in icons and images</b><br>
;; The 'guiserver.jar' file has the following icons and images built in. Each of
;; them is prefixed with the path '/local/'. For example '/local/newLISP128.png'
;; addresses the newLISP logo of size 128x128. To address images outside of
;; 'guiserver.jar', use a normal file path suitable to your platform. On MS Windows
;; use forward slashes instead of backslashes.
;; <pre>
;; Image path
;; ----------
;; clear-down32.png
;; clear32.png
;; copy-down32.png
;; copy32.png
;; cut-down32.png
;; cut32.png
;; dotgray16.png
;; dotgray32.png
;; dotgreen16.png
;; dotgreen32.png
;; dotred16.png
;; dotred32.png
;; dotyellow16.png
;; dotyellow32.png
;; edit-down32.png
;; edit32.png
;; folder-closed-down32.png
;; folder-closed32.png
;; folder-opened-down32.png
;; folder-opened32.png
;; font-book-down32.png
;; font-book32.png
;; green10.png
;; info-down32.png
;; info32.png
;; new-down32.png
;; new32.png
;; newLISP-down32.png
;; newLISP128.png
;; newLISP16.png
;; newLISP20.png
;; newLISP32.png
;; newLISP64.png
;; newLISPsplashWin.png
;; paste-down32.png
;; paste32.png
;; pressedbutton32.png
;; red10.png
;; restart-down32.png
;; restart32.png
;; run-down32.png
;; run32.png
;; save-down32.png
;; save32.png
;; search-down32.png
;; search32.png
;; stop-down32.png
;; stop32.png
;; </pre>
;; <li><b>Predefined colors</b><br>
;; The following colors are predefined:
;; <pre>
;; Name          rgb components
;; ----          -------------- 
;; gs:black      (0.0 0.0 0.0)
;; gs:blue       (0.0 0.0 1.0)
;; gs:cyan       (0.0 1.0 1.0)
;; gs:darkGray   (0.2509804 0.2509804 0.2509804)
;; gs:gray       (0.5019608 0.5019608 0.5019608)
;; gs:green      (0.0 1.0 0.0)
;; gs:lightGray  (0.7529412 0.7529412 0.7529412)
;; gs:magenta    (1.0 0.0 1.0)
;; gs:orange     (1.0 0.78431374 0.0)
;; gs:pink       (1.0 0.6862745 0.6862745)
;; gs:red        (1.0 0.0 0.0)
;; gs:white      (1.0 1.0 1.0)
;; gs:yellow     (1.0 1.0 0.0)
;; </pre>
;; Colors in newLISP-GS can be specified as three (four with alpha component) single
;; numbers or as a list of the three RGB values followed by the alpha channel number.
;; <pre>
;; (gs:set-background 'aPanel gs:magenta
;; (gs:set-background 'aPanel 1.0 0 1
;; (gs:set-background 'aPanel '(1 0 1)
;; </pre>
;; All of the above statements will produce the same background color on the <tt>aPanel</tt>.
;;
;; <li><b>Sound  and MIDI API</b><br>
;; The newLISP-GS sound API uses the default saundbank and sound hardware installed 
;; on the platform newLISP-GS is running on. On Mac OS X nothing additional needs to be
;; installed. On MS Windows and some Unix platforms a soundbank file needs to be installed.
;; Soundbanks for the Java JRE can be obtained at:
;; @link http://java.sun.com/products/java-media/sound/soundbanks.html http://java.sun.com/products/java-media/sound/soundbanks.html
;; The demo files shipped with the newLISP-GS installation use the midsize soundbank. Other soundbanks
;; may require different naming of instruments in the 'gs:midi-patch' statement. 
;;
;; The sound API is capable of playing of mutliple tracks at the same time
;; depending on the sound hardware installed on a specific platform. Mac OS X platforms
;; and most MS Windows platforms should be capable of playing 16 instruments (channels) at the
;; the same time 32 on parallel sounding tracks. newLISP GS supports 128 instruments of the
;; default soundbank installed.
;;
;; On Mac OS X and MS Windows channel 9 is a special channel sounding a different rythm 
;; instrument for all 128 different keys/notes.
;;
;; Basic capabilities of the sound API are shown in the demo files <tt>midi-demo.lsp</tt>
;; and <tt>midi2-demo.lsp</tt> in the <tt>/usr/share/newlisp/guiserver/</tt> or 
;; <tt>c:\Program files\newlisp\guiserver\</tt> directory.
;; <pre>
;;    (gs:add-track <int channel><list-notes>)
;;    (gs:channel-bend <int-channel> <bend>)
;;    (gs:get-instruments)
;;    (gs:instruments)
;;    (gs:midi-bpm <int-bpm> [<int-resolution>])
;;    (gs:midi-close)
;;    (gs:midi-init [<str-file-path>])
;;    (gs:midi-patch <int-instrument> [<int-channel>])
;;    (gs:mute-track <int-track> <bool-on-off>)
;;    (gs:play-note <int-key> [<int-duration> [<int-velocity> [<int-channel> [int-bend]]]])
;;    (gs:play-sequence [<int-start-tick> [<int-loop-count> [<int-start-loop> [<int-end-loop>]]]])
;;    (gs:save-sequence <str-file-path>)
;;    (gs:stop-sequence)
;;    (gs:play-sound <str-file-path>)
;; </pre>
;; </ul>
(context 'gs)

; ======================= preset colors

(set 'gs:black '(0.0 0.0 0.0))
(set 'gs:blue '(0.0 0.0 1.0))
(set 'gs:cyan '(0.0 1.0 1.0))
(set 'gs:darkGray '(0.2509804 0.2509804 0.2509804))
(set 'gs:gray '(0.5019608 0.5019608 0.5019608))
(set 'gs:green '(0.0 1.0 0.0))
(set 'gs:lightGray '(0.7529412 0.7529412 0.7529412))
(set 'gs:magenta '(1.0 0.0 1.0))
(set 'gs:orange '(1.0 0.78431374 0.0))
(set 'gs:pink '(1.0 0.6862745 0.6862745))
(set 'gs:red '(1.0 0.0 0.0))
(set 'gs:white '(1.0 1.0 1.0))
(set 'gs:yellow '(1.0 1.0 0.0))

; ====================== utility routines

; set server path
(if (= ostype "Windows")
	; on some MS Windows systems the jar -> javaw.exe association my be missing, the use the following:
	;(set 'server-path (string "javaw.exe -jar " "'\"" (env "NEWLISPDIR") "/guiserver.jar\"'")) 
	(set 'server-path (string  "'\"" (env "NEWLISPDIR") "/guiserver.jar\"'"))
	(set 'server-path (string (env "NEWLISPDIR") "/guiserver.jar"))
)

; Not documented gs:begin-cmd and gs:end-cmd
; sends graphics ommands in transactions, to be used in:
;
; draw-arc, draw-circle, draw-ellipse, draw-image, draw-line, 
; draw-path, draw-polyon, draw-rect, draw-round-rect, draw-text,
;
; fill-arc, fill-circle, fill-ellipse, fill-polygon, fill-rect, 
; fill-round-rect, 
;
; color-tag, delete-tag, hide-tag, move-tag, rotate-tag, scale-tag,
; shear-tag, translate-tag
;
; example:
; (gs:begin-cmd)
;   ; andy code containing 
;   ; gs:draw-xxx, gs:fill-xx, gs:xxx-tag
;   ; comands
; (gs:end-cmd)
;
; The whole command sequence between gs:begin-cmd and gs:end-cmd
; will be sent at once to the Java guiserver. This way minimizing
; network overhead. 
; Turns out, the speedup is too small in most of the demos.

(define (gs:begin-cmd)
	(set 'transaction-active true)
	(set 'transaction-buffer "")
)

(define (gs:end-cmd)
	(net-send out transaction-buffer)
	(set 'transaction-buffer "")
	(set 'transaction-active nil)
)
	
(define (gs:send-out str)
	(if transaction-active
		(write-buffer transaction-buffer str)
		(net-send out str);
	)
)

;; <br><br><br>

;; @syntax (gs:add-list-item <sym-list-combo> <str-text> [<str-text> ...])
;; @param <sym-list-combo> The name of the combo box or list box to which text entries are added.
;; @param <str-text> The text of the entry to be added.
;;
;; Items are added in the same sequence as they appear in the 'gs:add-list-item' command and added to the
;; end of the existing list of items.

(define (add-list-item comp)
	(let (s (string "add-list-item " comp " "))
		(doargs (item)
			(write-buffer s (string (base64-enc item) " ")))
		(write-buffer s "\n")
		(net-send out s))
)

;; @syntax (gs:add-separator <sym-menu-tool-bar>)
;; @param <sym-menu-tool-bar> The name of the tool bar or menu bar to which a spacer entry is added.
;;
;; Depending on the OS platform the separator may not be visible and only occupy space before the next 
;; component added to the tool or menu bar.

(define (add-separator bar)
	(net-send out (string "add-separator " bar "\n"))
)


;; @syntax (gs:add-track <int-channel> <list-of-notes>)
;; @param <int-channel> The channel belonging to this track.
;; @param <list-of-notes> A list of notes. Each note is a list of key duration velocity and bend.
;;
;; In case of 'gs:add-track' the duration of a note is given in ticks.
;; 16 ticks are in a quarter note or <em>beat</em>.
;;
;; A note is a list of 4 elements: '(<int-key> <int-duration> <int-velocity> [<int-bend>])'. <key>
;; <duration>, <velocity> and <bend> can be given as variables, which will be evaluated by
;; 'gs:add-track'. The <int-bend>  parameter is optional and assumed as <tt>0</tt> if not
;; present. See the command 'gs:play-note' for more information on the note bend parameter.
;; The following code is a complete example:
;;
;; @example
;; (load (append (env "NEWLISPDIR") "/guiserver.lsp"))
;; (gs:init)
;; (gs:midi-init)
;;
;; (map set '(C C# D D# E F F# G G# A A# B c c# d e f f# g g# a a# b) (sequence 60 82))
;; (set 'pp 30 'p 40 'm 64 'f 127) ; set velocity/volume
;;
;; (gs:midi-patch "Piano" 0)
;; (gs:midi-patch "Pizzicato Strings" 1)
;; (gs:midi-patch "Woodblock" 2)
;;
;; (gs:add-track 0 '( (C 12 m) (C# 4 m) (D 16 m) (c 16 f) (D 16 m)) )
;; (gs:add-track 1 (dup '(d 4 pp) 16))
;; (gs:add-track 2 '( (c 4 p) (c 12 p) (c 4 p) (c 12 p) (c 4 p) (c 12 p)  (c 4 p) (c 12 p)) )
;;
;; (gs:play-sequence)
;;
;; (sleep 5000)
;; (gs:midi-close)
;; (exit)

;; The second example shows the usage of pitch-bend in notes:
;; 
;; @example
;; (load (append (env "NEWLISPDIR") "/guiserver.lsp"))
;; (gs:init)
;; (gs:midi-init)
;; 
;; (gs:midi-patch "Piano" 0)
;; (gs:midi-patch "Gunshot" 1)
;; (gs:midi-patch "Telephone" 2)
;; 
;; (for (n -8192 8191 128) (push (list 64 1 95 n) track0 -1))
;; (for (n 64 96 2) (push (list n 8 76) track1 -1))
;; 
;; (gs:add-track 0 track0)
;; (gs:add-track 1 track1)
;; (gs:add-track 2 '((44 128 127)))
;; 
;; (gs:play-sequence)
;; ;(gs:save-sequence "extremeAlarm.mid")
;; 
;; (sleep 8000)
;; (gs:midi-close)
;; (exit)

(define (add-track channel notes)
	(net-send out (string "add-track System " channel " "))
	(local (bend)
		(dolist (n notes)
			(set 'bend (if (= 4 (length n)) (eval (n 3)) 0))
			(net-send out (string (eval (n 0)) " " (eval (n 1)) " " (eval (n 2)) " " bend " ")))
	)
	(net-send out "\n")
)

;; @syntax (gs:add-to <sym-container> <sym-component> [<sym-componentl> ...])
;; @param <sym-container> The name of the container to which components are added.
;; @param <sym-component> One or more symbols of the components to add.


;; @syntax (gs:add-to <sym-container> <sym-component> <str-orientation> [<sym-component> <str-orientation> ...])
;; @param <sym-container> The name of the container to which components are added.
;; @param <sym-component> The name of a component to add.
;; @param <str-orientation> The orientation of a component to add in border layout, '"north"', '"west"', '"center"', '"east"' or '"south"'.

(define (add-to id)
	(let (s (string "add-to " id " "))
		(doargs (item)
			(write-buffer s (string item " ")))
		(write-buffer s "\n")
		(net-send out s)
	)
)

;; @syntax (gs:append-text <sym-id> <str-text>)
;; @param <sym-id> The name of the text field or text area to which text is appended.
;; @param <str-text> The text to be appended.

(define (append-text id text)
	(net-send out (string "append-text " id " " (base64-enc text) "\n"))
)

;; @syntax (gs:button <sym-id> <sym-action> [<str-text> [<int-width> <int-height>]])
;; @param <sym-id> The name of the button.
;; @param <sym-action> The name of the event handler.
;; @param <str-text> An optional text for the button.
;; @param <int-width> The optional width of the button.
;; @param <int-height> The optional height of the button.

(define (button id action text x y)
	(if text
		(if (and x y)
			(net-send out (string "button " id " " action " " (base64-enc text) " " x " " y "\n"))
			(net-send out (string "button " id " " action " " (base64-enc text) "\n")))
		(net-send out (string "button " id " " action "\n"))
	)
)

;; @syntax (gs:canvas <sym-id>) 
;; @param <sym-id> The name of the canvas.
;;
;; A canvas is a panel for drawing and receiving mouse input events. For most
;; applications a background color should be specified for the canvas using
;; 'gs:set-background' or 'gs:set-color' which call the same function internally. The
;; background forces the canvas to be cleared before redrawing components
;; which have been moved, rotated, scaled or deleted. In applications where
;; this is not desired but a background color is still required, the background
;; color can be specified for the container hosting the canvas. The canvas
;; background will then appear in the color of the container, but erasing
;; the canvas before repainting is not enforced.
;;
;; A canvas can also be used to host widgets like buttons etc.. In this case
;; the canvas is treated like a 'gs:panel', with a flow layout by default.
;; Similar to a panel created with 'gs:panel' other layouts can be set.
;;
;; When widgets are present on a canvas they appear to be floating over
;; the drawing. See the file 'textrot-demo.lsp' for an example.

(define (canvas id parent)
	(set 'gs:currentCanvas id)
	(net-send out (string "canvas " id "\n"))
)

;; @syntax (gs:channel-bend <int-channel> <bend>)
;; @param <int-channel> The channel where the pitch bend is set.
;; @param <int-bend> The channel bend between <tt>0</tt> and <tt>16383</tt>.
;;
;; Numbers upwards of <tt>8192</tt> bend the tone upwards, numbers smaller
;; than <tt>8192</tt> bend the tone downwards. To switch off channel bend set
;; the number to the middle posiotion of <tt>8192</tt>. 'gs:channel-bend' can 
;; be used to bring a channel in tune with an external sound source.

(define (channel-bend channel bend)
	(net-send out (string "channel-bend System " channel " " bend "\n"))
)

; @syntax (gs:channel-reverb <int-channel> <reverb>)
; @param <int-channel> The channel where the reverb is set.
; @param <int-reverb> The channel reverb between <tt>0</tt> and <tt>127</tt>.
;
; Sets the reverberation of the channel for all notes.

(define (channel-reverb channel reverb)
	(net-send out (string "channel-reverb System " channel " " reverb "\n"))
)

;; @syntax (gs:check-box <sym-id> <sym-action> [<str-text> [<bool-selected>]])
;; @param <sym-id> The name of the check box.
;; @param <sym-action> The name of the event handler.
;; @param <str-text> The text of the check box.
;; @param <bool-selected> An optional flag indicating the selection state 'true' or 'nil' (default).
;;

(define (check-box id action text selected)
	(if text
		(net-send out (string "check-box " id " " action " " (base64-enc text) " " selected "\n"))
		(net-send out (string "check-box " id " " action "\n")))
)

;; @syntax (gs:check-event <int-microseconds>)
;; @param <int-microseconds> Wait for an event a maximum of <int-microseconds> and execute it.
;;
;; The function 'gs:check-event' is used as an alternative to 'gs:listen' when the application
;; is performing some activity while waiting for user input from the GUI. Typically
;; 'gs:check-event' is used in a loop, which performs some other task and at the same time
;; checks for events from the GUI part of the application. Like 'gs:listen' the function will 
;; force an exit of the application when communication between the newLISP-GS and the newLISP
;; application process fails. 
;;
;; @example
;; (while (gs:check-event 10000) ; check for 10 milliseconds
;;     (do-myprocess)
;; )


(define (check-event us , event)
	(when (net-select in "read" us)
		(if (net-receive in event 1000000000 "\n")
			(begin 	
			;	(println "check-event: " event)
				(eval-string event))
			(exit)))
true
)

;; @syntax (gs:clear-list <sym-id>)
;; @param <sum-id> The name of the list component in which to clear all entries.

(define (clear-list id)
	(net-send out (string "clear-list " id "\n"))
)

;; @syntax (gs:clear-text <sym-id>)
;; @param <sum-id> The name of the component in which to clear the text.

(define (clear-text id)
	(net-send out (string "clear-text " id "\n"))
)

;; @syntax (gs:copy-text <sym-id>)
;; @param <sum-id> The name of the text component from which to copy the selection to the clipboard.

(define (copy-text id)
	(net-send out (string "copy-text " id "\n"))
)

;; @syntax (gs:cut-text <sym-id>)
;; @param <sym-id-text> The name of the text component from which to cut selected text and place it on the clipboard..

(define (cut-text id)
	(net-send out (string "cut-text " id "\n"))
)


;; @syntax (gs:color-dialog <sym-parent-frame> <sym-action> <str-title> <float-red> <float-green> <float-blue>)
;; @param <sym-parent-frame> The name symbol of the parent frame.
;; @param <sym-action> The symbol of the handler to call when leaving the dialog
;; @param <str-title> The title of the color dialog.
;; @param <float-red> The initial red color component.
;; @param <float-green> The initial green color component.
;; @param <float-blue> The initial blue color component.

(define (color-dialog parent action title red green blue)
	(net-send out (string "color-dialog " parent " " action " " (base64-enc title) " " red " " green " " blue "\n"))
)


;; @syntax (gs:color-tag <sym-tag> <list-color> [<boolean-update>])
;; @param <sym-tag> The name tag of the shape(s) to set a new color.
;; @param <list-color> The new color as a list of 3 numbers for the rgb components.
;; @param <boolean-repaint> An optional flag to indicate if repainting is required (default is 'true').

(define (color-tag tag color (update true))
	(send-out (string "color-tag " gs:currentCanvas " " tag " "
		(color 0) " " (color 1) " " (color 2) " " update "\n"))
)

;; @syntax (gs:combo-box <sym-id> <sym-action> [<str-item-1> ...])
;; @param <sym-id> The name of the combo box.
;; @param <sym-action> The name of the event handler.
;; @param <str-item> Zero, one or more text entries in the combo box.
;; @syntax (gs:combo-box <sym-id> <sym-action> [<list-str-items>])
;; @param <sym-id> The name of the combo box.
;; @param <sym-action> The name of the event handler.
;; @param <list-str-items> Zero, one or more text entries in a list.

(define (combo-box id action)
	(let (	s (string "combo-box " id " " action " ")
					entries (if (list? (args 0)) (args 0) (args)) )
		(dolist (item entries)
			(write-buffer s (string (base64-enc item) " ")))
		(write-buffer s "\n")
		(net-send out s))
)

;; @syntax (gs:confirm-dialog <sym-parent-frame> <sym-action> <str-title> <str-message> [<str-type>])
;; @param <sym-parent-frame> The symbol name of the parent frame.
;; @param <sym-action> The action to perform when the diaog is closed.
;; @param <str-title> The title of the message box.
;; @param <str-message> The message in the message box.
;; @param <str-type> The type of the message box.
;;
;; The type of the message box can be one of: '"yes-no"', '"yes-no-cancel"'
;; On return of the message box <sym-action> carries one of the responses <tt>0</tt> for the yes-,
;; <tt>1</tt> for the no- or <tt>2</tt> for the cancel-button.

(define (confirm-dialog parent action title msg (type "plain"))
	(net-send out (string "confirm-dialog " parent " " action " " 
						(base64-enc title) " " (base64-enc msg) " " type "\n"))
)

;; @syntax (gs:delete-tag <sym-tag> [<boolean-repaint>])
;; @param <sym-tag> The tag group to be deleted.
;; @param <boolean-repaint> An optional flag to indicate if repainting is required (default is 'true').
;;
;; Deletes all 2D objects (lines, shapes, text, images) tagged with <sym-tag>.
;;
;; Each time a 'gs:draw-xxx' or 'gs:fill-xxx' function is called a graphical
;; object is created. The tag used during creation is a way to address one
;; or more of these objects. See the file 'mouse-demo.lsp' for an example.

(define (delete-tag tag (repaint true))
	(send-out (string "delete-tag " gs:currentCanvas " " tag  " " repaint "\n"))
)

;; @syntax (gs:destroy-shell <sym-text-area>)
;; @param <sym-text-area> The name of the text component for which the shell process is destroyed.

(define (destroy-shell id)
	(net-send out (string "destroy-shell " id "\n"))
)

;; @syntax (gs:dialog <sym-id> <sym-parent> <str-title> <int-width> <int-height> [<boolean-visible> [<boolean-modal>]])
;; @param <sym-id> The name of the dialog
;; @param <sym-parent> The name of the parent frame.
;; @param <str-title> The title string of the dialog frame.
;; @param <int-width> The width of the dialog frame.
;; @param <int-height> The height of the dialog frame.
;; @param <boolean-visible> The optional flag with a value of 'true' or 'nil' for visibility of the dialog.
;; @param <boolean-modal> The optional flag with a value of 'true' or 'nil' for modality of the dialog.
;;
;; Initially the dialog should not be visible until all widgets are added to it. 
;; When no flags for visibility and modality are specified, 'nil' is assumed. A modal dialog will
;; prevent input in the parent window. Components can be added to a dialog using 'gs:add-to'.
;; Use the 'gs:set-border-layout', 'ga:set-flow-layout' or 'gs:set-grid-layout' to set a specific
;; layout.

(define (dialog id parent title width height visible modal)
	(net-send out (string "dialog " id " " parent " " (base64-enc title) " " width " " height " " visible " " modal"\n"))
)

;; @syntax (gs:disable <sym-id-1> [sym-id-2 ...])
;; @param <sym-id> The name of the component to disable.
;;
;; Disabled components are grayed and do not accept input.

(define (disable)
	(let (s "disable ")
		(doargs (item)
			(write-buffer s (string item " ")))
		(write-buffer s "\n")
		(net-send out s)
	)
)

;; @syntax (gs:dispose <sym-id>)
;; @param <sym-id> The name of the component to dispose of.
;;
;; Only objects created with 'gs:dialog', 'gs:frame' or 'gs:window' can be
;; deleted with 'gs:dispose'.

(define (dispose id)
	(net-send out (string "dispose " id "\n"))
)

;; @syntax (gs:dispose-splash)
;;
;; A splash screen can be specified when starting the newLISP-GS process before newLISP. 
;; The function 'gs:dispose-splash' is used to turn off the splash image after
;; the newLISP GUI application has started.
;;
;; @example
;; java -jar /usr/share/newlisp/guiserver.jar 64001 program.lsp /local/newLISP128.png
;; ; or on MS Windows
;; java -jar "c:\Program Files\newlisp\guiserver.jar" 64001 "newlisp program.lsp" /local/newLISPsplashWin.png

;; The example starts newLISP-GS with an application 'program.lsp' and a splash
;; screen showing the built-in newLISP logos and using port 64001/2. Instead, the full pathname 
;; of a different image file can be specified. Inside 'program.lsp' the function
;; 'gs:dispose-splash' or a mouse click on the image will turn off the splash screen. 
;; For 'program.lsp' the full pathname may be necessary. On MS Windows quotes are necessary to bracket 
;; arguments to 'guiserver.jar' which contain spaces. 

(define (dispose-splash)
	(net-send out "dispose-splash System\n")
) 

;; @syntax (gs:draw-arc <sym-tag> <int-x> <int-y> <int-width> <int-height> <int-start> <int-angle> [<list-rgb>])
;; @param <sym-tag> The tag group of the arc.
;; @param <int-x> The X position of the arc.
;; @param <int-y> The Y position of the arc.
;; @param <int-width> The width of the arc.
;; @param <int-height> The height of the arc.
;; @param <int-start-angle> The start angle of the arc in 0 to 360 degrees.
;; @param <int-arc-angle> The opening angle of the arc in 0 to 360 degrees.
;; @param <list-rgb> The outline color as a list of 3 numbers for the rgb components
;;
;; The resulting arc begins at <int-start-angle> and extends for <int-arc-angle degrees>, 
;; using the current color. Angles are interpreted such that 0 degrees is at the 3 o'clock 
;; position. A positive value indicates a counter-clockwise rotation while a negative value 
;; indicates a clockwise rotation.
;;
;; The center of the arc is the center of the rectangle whose origin is (x, y) and whose size 
;; is specified by the width and height arguments.
;;
;; The resulting arc covers an area <int-width> pixels wide by <int-height> pixels tall.

(define (draw-arc id x y width height start angle color)
	(if color
		(send-out (string "draw-arc " gs:currentCanvas " " id " " x " " y " "
						width " " height " " start " " angle " " 
						(color 0) " " (color 1) " " (color 2) "\n"))
		(send-out (string "draw-arc " gs:currentCanvas " " id " " x " " y " "
						width " " height " " start " " angle "\n"))
	)
)


;; @syntax (gs:draw-circle <sym-tag> <int-x> <int-y> <int-radius> [<list-rgb>])
;; @param <sym-tag> The tag group of the circle.
;; @param <int-x> The X position of the circle center.
;; @param <int-y> The Y position of the circle center.
;; @param <int-radius> The radius of the circle.
;; @param <list-rgb> The outline color as a list of 3 numbers for the rgb components
;;
;; Creates the outline of a circle. The color numbers must be entered in list form.
;; If no <list-rgb> is used the current paint specified with 'gs:set-paint' is used.

(define (draw-circle tag x y radius color)
	(if color
		(send-out (string "draw-circle " gs:currentCanvas " " 
			tag " " x " " y " " radius " " (color 0) " " (color 1) " " (color 2) "\n"))
		(send-out (string "draw-circle " gs:currentCanvas " " 
			tag " " x " " y " " radius "\n"))
	)
)

;; @syntax (gs:draw-ellipse <sym-tag> <int-x> <int-y> <int-radius-x> <int-radius-y> [<list-rgb>])
;; @param <sym-tag> The tag group of the ellipse.
;; @param <int-x> The X position of the ellipse center.
;; @param <int-y> The Y position of the ellipse center.
;; @param <int-radius-x> The radius of the ellipse.
;; @param <int-radius-y> The radius of the ellipse.
;; @param <list-rgb> The outline color as a list of 3 numbers for the rgb components
;;
;; Creates the outline of an ellipse. The color numbers must be enterd in list form.
;; If no <list-rgb> is used the current paint specified with 'gs:set-paint' is used.

(define (draw-ellipse tag x y radius-x radius-y color)
	(if color
		(send-out (string "draw-ellipse " gs:currentCanvas " " 
			tag " " x " " y " " radius-x " " radius-y " " (color 0) " " (color 1) " " (color 2) "\n"))
		(send-out (string "draw-ellipse " gs:currentCanvas " " 
			tag " " x " " y " " radius-x " " radius-y  "\n"))
	)
)

;; @syntax (gs:draw-image <sym-tag> <str-path> <int-x> <int-y> [<int-width> <int-height>])
;; @param <sym-tag> The tag group of the image.
;; @param <str-path> The file path-name of the image.
;; @param <int-x> The X position of the image top left corner.
;; @param <int-y> The Y position of the image top left corner.
;; @param <int-width> The optional width in which to draw the image.
;; @param <int-height> The optional height in which to draw the image.
;;
;; When <int-width> and <int-height> parameters are specified and they are not
;; equal to the original size of the image (measured in pixels) the image will
;; be displayed in either compressed or zoomed form.

(define (draw-image tag image-path x y width height)
	(if (and width height)
		(send-out (string "draw-image " gs:currentCanvas " " tag " " 
				(base64-enc image-path) " " x " " y " " width " " height "\n"))
		(send-out (string "draw-image " gs:currentCanvas " " tag " " 
				(base64-enc image-path) " " x " " y "\n"))
	)
) 

;; @syntax (gs:draw-line <sym-tag> <int-x1> <int-y1> <int-x2> <int-y2> [<list-rgb>])
;; @param <sym-tag> The tage of the line.
;; @param <int-x1> The X position of the first point.
;; @param <int-y1> The Y position of the first point.
;; @param <int-x2> The X position of the second point.
;; @param <int-y2> The Y position of the second point.
;;
;; Draws a line. The color numbers must be entered in list form.
;; If no <list-rgb> is used the current paint specified with 'gs:set-paint' is used.

(define (draw-line tag x1 y1 x2 y2 color)
	(if color
		(send-out (string "draw-line " gs:currentCanvas " " tag " "
			x1 " " y1 " " x2 " " y2 " " (color 0) " " (color 1) " " (color 2) "\n"))
	)
)

;; @syntax (gs:draw-path <sym-tag> <list-points> [<list-rgb>])
;; @param <sym-tag> The tage group of the path.
;; @param <list-points> The list of x and y coordinates of the points.
;;
;; Draws a path with the points found in <list-points>. 
;;
;; @example 
;; (gs:draw-path 'P '(1 0 2 2 3 0))

;; The example will draw an upwards-pointing triangle without base at the 
;; points '1,0', '2,2' and '3,0' 

(define (draw-path tag points color)
	(if color
		(let (s (string "draw-path " gs:currentCanvas " " tag " " (/ (length points) 2) " "))
			(dolist (p points)
				(write-buffer s (string p " ")))
			(write-buffer s (string (color 0) " " (color 1) " " (color 2) "\n"))
			(send-out s)
		)
		(let (s (string "draw-path " gs:currentCanvas " " tag " " (/ (length points) 2) " "))
			(dolist (p points)
				(write-buffer s (string p " ")))
			(write-buffer s "\n")
			(send-out s)
		)
	)
)

;; @syntax (gs:draw-polygon <sym-tag> <list-points> [<list-rgb>])
;; @param <sym-tag> The tag group of the polygon.
;; @param <list-points> The list of x and y coordinates of the points.
;;
;; Draws a polygon with the points found in <list-points>. 
;;
;; @example 
;; (gs:draw-polygon 'P '(1 0 2 2 3 0))

;; The example will draw an upwards-pointing triangle with the points '1,0', '2,2'
;; and '3,0'.

(define (draw-polygon tag points color)
	(if color
		(let (s (string "draw-polygon " gs:currentCanvas " " tag " " (/ (length points) 2) " "))
			(dolist (p points)
				(write-buffer s (string p " ")))
			(write-buffer s (string (color 0) " " (color 1) " " (color 2) "\n"))
			(send-out s)
		)
		(let (s (string "draw-polygon " gs:currentCanvas " " tag " " (/ (length points) 2) " "))
			(dolist (p points)
				(write-buffer s (string p " ")))
			(write-buffer s "\n")
			(send-out s)
		)
	)
)


;; @syntax (gs:draw-rect <sym-tag> <int-x> <int-y> <int-width> <int-height> [<list-rgb>])
;; @param <sym-tag> The tag group of the rectangle.
;; @param <int-x> The X position of the top left corner.
;; @param <int-y> The Y position of the top left corner.
;; @param <int-width> The width of the rectangle.
;; @param <int-height> The height of the rectangle..
;; @param <list-rgb> The outline color as a list of 3 numbers for the rgb components
;;
;; Creates the outline of a rectangle. The color numbers must be entered in list form.
;; If no <list-rgb> is used the current paint specified with 'gs:set-paint' is used.

(define (draw-rect tag x y width height color)
	(if color
		(send-out (string "draw-rect " gs:currentCanvas " " 
			tag " " x " " y " " width " " height " " (color 0) " " (color 1) " " (color 2) "\n"))
		(send-out (string "draw-rect " gs:currentCanvas " " 
			tag " " x " " y " " width " " height "\n"))
	)
)


;; @syntax (gs:draw-round-rect <sym-tag> <int-x> <int-y> <int-width> <int-height> <int-arc-width> <int-arc-height> [<list-rgb>])
;; @param <sym-tag> The tag group of the round rectangle.
;; @param <int-x> The X position of the top left corner.
;; @param <int-y> The Y position of the top left corner.
;; @param <int-width> The width of the rectangle.
;; @param <int-height> The height of the rectangle..
;; @param <int-arc-width> The width of the corner rectangle.
;; @param <int-arc-height> The height of the corner rectangle..
;; @param <list-rgb> The outline color as a list of 3 numbers for the rgb components
;;
;; Draws a rectangle shape with round corners. The rounding is defined by the rectangle enclosing
;; the rounding arc.

(define (draw-round-rect tag x y width height arcw arch color)
	(if color
		(send-out (string "draw-round-rect " gs:currentCanvas " " 
			tag " " x " " y " " width " " height " "  arcw " " arch " " 
				(color 0) " " (color 1) " " (color 2) "\n"))
		(send-out (string "draw-round-rect " gs:currentCanvas " " 
			tag " " x " " y " " width " " height  " " arcw " " arch "\n"))
	)
)


;; @syntax (gs:draw-text <sym-tag> <str-text> <int-x> <int-y> [<list-rgb> [<float-angle>])
;; @param <sym-tag> The tag group of the text.
;; @param <str-text> The text to be drawn.
;; @param <int-x> The X position of the text.
;; @param <int-y> The Y position of the text.
;; @param <list-rgb> The optonal color for the text.
;; @param <float-angle> The optional angle for text in degrees
;;
;; If no <list-rgb> is used, the current paint specified with 'gs:set-paint' is used.
;; The optional angle is <tt>0</tt> by default for horizontal text. A value of <tt>90</tt>
;; will rotate the text around the <tt>x</tt>, <tt>y</tt> position downwards clockwise.

(define (draw-text tag text x y color angle)
	(if color
		(if angle
			(send-out (string "draw-text " gs:currentCanvas " "
				tag " "  (base64-enc text) " " x " " y " " 
				(color 0) " " (color 1) " " (color 2) " " angle "\n"))
			(send-out (string "draw-text " gs:currentCanvas " "
				tag " "  (base64-enc text) " " x " " y " " 
				(color 0) " " (color 1) " " (color 2) "\n"))
		)
		(send-out (string "draw-text " gs:currentCanvas " "
			tag " "  (base64-enc text) " " x " " y "\n"))
	)
)




;; @syntax (gs:enable <sym-id-1> [sym-id-2 ...])
;; @param <sym-id> The name of a component to enable.
;;
;; Components are enabled by default.

(define (enable)
	(let (s (string "enable "))
		(doargs (item)
			(write-buffer s (string item " ")))
		(write-buffer s "\n")
		(net-send out s)
	)
)

;; @syntax (gs:eval-shell <sym-id-text-area> <str-command>)
;; @param <sym-id-text-area> The name of the text area in which to evaluate text.
;; @param <str-command> The text to evaluate in the shell.

(define (eval-shell id command)
	(net-send out (string "eval-shell " id " " (base64-enc command) "\n"))
)

;; @syntax (gs:export <str-path-file> [<int-width> <int-height>])
;; @param <str-path-file> The path and file name of the image file to write to.
;; @param <int-width> The optional width of the image in pixels.
;; @param <int-height> The optional height of the image in pixels.
;;
;; If no width and height are specified, the current size of the canvas is assumed.
;;
;; @example
;;
;; (gs:export "/usr/home/pictures/mypic.png")

;; This will generate a '.png' file exactly as the image seen on the screen.
;; The color format is RGBA with 8 bit per color and an alpha channel. When
;; no background color is defined for the canvas, the background will be
;; transparent.
;;
;; When specifying width and height, a smaller or bigger portion of the canvas
;; than seen on the screen is printed to the image.

(define (export image-path width height)
	(let (result (if (and width height)
		(net-send out (string "export " gs:currentCanvas " " (base64-enc image-path) " " width " " height "\n"))
		(net-send out (string "export " gs:currentCanvas " " (base64-enc image-path) "\n")) ))
        (if (number? result) true nil)
    )
)
			

;; @syntax (gs:fill-arc <sym-tag> <int-x> <int-y> <int-width> <int-height> <int-start> <int-angle> [<list-rgb>])
;; @param <sym-tag> The tag group of the arc.
;; @param <int-x> The X position of the arc.
;; @param <int-y> The Y position of the arc.
;; @param <int-width> The width of the arc.
;; @param <int-height> The height of the arc.
;; @param <int-start-angle> The start angle of the arc in 0 to 360 degrees.
;; @param <int-arc-angle> The opening angle of the arc in 0 to 360 degrees.
;; @param <list-rgb> The outline color as a list of 3 numbers for the rgb components.
;;
;; The resulting arc begins at <int-start-angle> and extends for <int-arc-angle degrees>, 
;; using the current color. Angles are interpreted such that 0 degrees is at the 3 o'clock 
;; position. A positive value indicates a counter-clockwise rotation while a negative value 
;; indicates a clockwise rotation.
;;
;; The center of the arc is the center of the rectangle whose origin is (x, y) and whose size 
;; is specified by the width and height arguments.
;;
;; The resulting arc covers an area <int-width> pixels wide by <int-height> pixels tall.

(define (fill-arc id x y width height start angle color)
	(if color
		(send-out (string "fill-arc " gs:currentCanvas " " id " " x " " y " "
						width " " height " " start " " angle " " 
						(color 0) " " (color 1) " " (color 2) "\n"))
		(send-out (string "fill-arc " gs:currentCanvas " " id " " x " " y " "
						width " " height " " start " " angle "\n"))
	)
)

;; @syntax (gs:fill-circle <sym-tag> <int-x> <int-y> <int-radius> [<list-rgb>])
;; @param <sym-tag> The tag group of the circle.
;; @param <int-x> The X position of the circle center.
;; @param <int-y> The Y position of the circle center.
;; @param <int-radius> The radius of the circle.
;; @param <list-rgb> The outline color as a list of 3 numbers for the rgb components.
;;
;; Creates a filled  circle. The color numbers must be entered in list form.
;; If no <list-rgb> is used, the current paint specified with 'gs:set-paint' is used.

(define (fill-circle tag x y radius color)
	(if color
		(send-out (string "fill-circle " gs:currentCanvas " " 
			tag " " x " " y " " radius " " (color 0) " " (color 1) " " (color 2) "\n"))
		(send-out (string "fill-circle " gs:currentCanvas " " 
			tag " " x " " y " " radius "\n"))
	)
)

;; @syntax (gs:fill-ellipse <sym-tag> <int-x> <int-y> <int-radius-x> <int-radius-y> [<list-rgb>])
;; @param <sym-tag> The tag group of the ellipse.
;; @param <int-x> The X position of the ellipse center.
;; @param <int-y> The Y position of the ellipse center.
;; @param <int-radius-x> The radius of the ellipse.
;; @param <int-radius-y> The radius of the ellipse.
;; @param <list-rgb> The fill color as a list of 3 numbers for the rgb components.
;;
;; Creates a filled ellipse. The color numbers must be entered in list form.
;; If no <list-rgb> is used, the current paint specified with 'gs:set-paint' is used.

(define (fill-ellipse tag x y radius-x radius-y color)
	(if color
		(send-out (string "fill-ellipse " gs:currentCanvas " " 
			tag " " x " " y " " radius-x " " radius-y " " 
				(color 0) " " (color 1) " " (color 2) "\n"))
		(send-out (string "fill-ellipse " gs:currentCanvas " " 
			tag " " x " " y " " radius-x " " radius-y  "\n"))
	)
)

;; @syntax (gs:fill-polygon <sym-tag> <list-points> [<list-color>])
;; @param <sym-tag> The tag group of the polygon.
;; @param <list-points> The list of x and y coordinates of the points.
;;
;; Draws a polygon with the points found in <list-points> and fills it with
;; the current color set with 'gs:set-paint' or the optional color
;; given in <list-color>.
;;
;; @example 
;; (gs:fill-polygon 'P '(1 0 2 2 3 0))
;;
;; (gs:fill-polygon 'P '(1 0 2 2 3 0) gs:gray)

;; The example will fill an upwards pointing triangle with the points '1,0', '2,2'
;; and '3,0' and fill it with the current color set with 'gs:set-paint'. The second
;; example paints a gray triangle regardless of the current color set with
;; 'gs:set-paint'.

(define (fill-polygon tag points color)
	(if color
		(let (s (string "fill-polygon " gs:currentCanvas " " tag " " (/ (length points) 2) " "))
			(dolist (p points)
				(write-buffer s (string p " ")))
			(write-buffer s (string (color 0) " " (color 1) " " (color 2) "\n"))
			(send-out s)
		)
		(let (s (string "fill-polygon " gs:currentCanvas " " tag " " (/ (length points) 2) " "))
			(dolist (p points)
				(write-buffer s (string p " ")))
			(write-buffer s "\n")
			(send-out s)
		)
	)
)

;; @syntax (gs:fill-rect <sym-tag> <int-x> <int-y> <int-width> <int-height> [<list-rgb>])
;; @param <sym-tag> The tag group of the rectangle.
;; @param <int-x> The X position of the top left corner.
;; @param <int-y> The Y position of the top left corner.
;; @param <int-width> The width of the rectangle.
;; @param <int-height> The height of the rectangle.
;; @param <list-rgb> The fill color as a list of 3 numbers for the rgb components.
;;
;; Creates a filled rectangle. The color numbers must be entered in list form.
;; If no <list-rgb> is used, the current paint specified with 'gs:set-paint' is used.

(define (fill-rect tag x y width height color)
	(if color
		(send-out (string "fill-rect " gs:currentCanvas " " 
			tag " " x " " y " " width " " height " " (color 0) " " (color 1) " " (color 2) "\n"))
		(send-out (string "fill-rect " gs:currentCanvas " " 
			tag " " x " " y " " width " " height "\n"))
	)
)

;; @syntax (gs:fill-round-rect <sym-tag> <int-x> <int-y> <int-width> <int-height> <int-arc-width> <int-arc-height> [<list-rgb>])
;; @param <sym-tag> The tag group of the round rectangle.
;; @param <int-x> The X position of the top left corner.
;; @param <int-y> The Y position of the top left corner.
;; @param <int-width> The width of the rectangle.
;; @param <int-height> The height of the rectangle..
;; @param <int-arc-width> The width of the corner rectangle.
;; @param <int-arc-height> The height of the corner rectangle.
;; @param <list-rgb> The outline color as a list of 3 numbers for the rgb components.
;;
;; Paints a rectangle shape with round corners. The rounding is defined by the rectangle enclosing
;; the rounding arc.

(define (fill-round-rect tag x y width height arcw arch color)
	(if color
		(send-out (string "fill-round-rect " gs:currentCanvas " " 
			tag " " x " " y " " width " " height " "  arcw " " arch " " 
				(color 0) " " (color 1) " " (color 2) "\n"))
		(send-out (string "fill-round-rect " gs:currentCanvas " " 
			tag " " x " " y " " width " " height  " " arcw " " arch "\n"))
	)
)

;; @syntax (gs:find-text <sym-id> <str-text> <sym-action> [<str-direction>])
;; @param <sym-id> The name of the text area or text pane.
;; @param <str-text> The searching text.
;; @param <sym-action> A optional action to peform after find-text.
;; @param <str-direction> The optional direction string '"next"' (default) or '"previous"'.
;;
;; The text area or text pane will be searched starting at the current caret position
;; forward or backwards depending on the optional direction field. After the search
;; the found text is highlighted. If the optional <sym-action> is specified an event
;; containing the id of the text area or pane and found position or '-1' will be fired.

(define (find-text id text action (direction "next"))
	(net-send out (string "find-text " id " " (base64-enc text) " " action " " direction "\n"))
)



;; @syntax (gs:frame <sym-id> <int-x> <int-y> <int-width> <int-height> [<str-title> <boolean-visible>])
;; @param <sym-id> The name of the frame window.
;; @param <int-x> The X position of the top left window corner.
;; @param <int-y> The Y position of the top left windows corner.
;; @param <int-width> The width of the window frame.
;; @param <int-height> The height of the windows frame.
;; @param <str-title> The optional title of the window.
;; @param <boolean-visible> The optional flag with a value of 'true' or 'nil' for the visibility of the window.
;;
;; Initially the frame should not be visible until all widgets are added to it. 
;; When no flag for visibility is specified 'nil' is assumed. The default layout of a frame behaves
;; like a grid layout with one cell. Use the 'set-flow-layout', 'set-border-layout' and
;; 'set-grid-layout' to change the layout.


(define (frame id x y width height text visible)
	(if text
		(net-send out (string "frame " id " " x " " y " " width " " height " " (base64-enc text) " " visible "\n"))
		(net-send out (string "frame " id " " x " " y " " width " " height "\n"))
	)
)

;; @syntax (gs:get-bounds <sym-id>)
;; @param <sym-id> The id of the component for which to get the list of bounding values.
;; @return The bounding list '(<x> <y> <width> <height>)'
 
(define (get-bounds id)
	(set 'gs:bounds nil)
	(net-send out (string "get-bounds " id "\n"))
	(while (not gs:bounds) (check-event 10000))
	gs:bounds
)

;; @syntax (gs:get-fonts)
;; @return A list of family names for fonts on the current system.
;;
;; The function should be called only once because it may take considerable
;; time in a system loaded with many fonts. The variable 'gs:fonts' contains
;; the same list of fonts originally returned by a call to 'gs:get-fonts'.

(define (get-fonts)
	(if (not gs:fonts)
		(begin
    		(net-send out (string "get-fonts System\n"))
			(while (not gs:fonts) (check-event 10000))
			(set 'gs:fonts (map base64-dec gs:fonts))
		)
	)
)

;; @syntax (gs:get-font-metrics <sym-id> <str-text>)
;; @return A list of the two values for width and height in pixels.
;;
;; The font metrics for the currently set font in <sym-id> are returned as a list
;; of width and height in pixels when displaying the string in <str-text>.
;; After the function call the variable 'gs:font-metrics' contains the same list
;; of values as originally returned by the call to 'gs:get-font-metrics'.

(define (get-font-metrics id text)
	(set 'gs:font-metrics nil)
	(net-send out (string "get-font-metrics " id " " (base64-enc text) "\n"))
	(while (not gs:font-metrics) (check-event 10000))
	gs:font-metrics	
)

;; @syntax (gs:get-instruments)
;; @return A list of instrument names in the default MIDI soundbank.
;;
;; The function should be called only once because it may take considerable
;; time in a system loaded with a big soundbank. The variable 'gs:instruments' contains
;; the same list of instruments originally returned by a call to 'gs:get-instruments'.

(define (get-instruments)
	(if (not gs:instruments)
		(begin
    		(net-send out (string "get-instruments System\n"))
			(while (not gs:instruments) (check-event 10000))
			(set 'gs:instruments (map base64-dec gs:instruments))
		)
	)
)


;; @syntax (gs:get-screen)
;; @return A list of screen width, height and resolution of the main computer screen.
;;
;; After calling the 'gs:get-screen' once the screen parameters are also available
;; in the variable 'gs:screen'.

(define (get-screen)
    (net-send out (string "get-screen System\n"))
	(while (not gs:screen) (check-event 10000))
	gs:screen
)


;; @syntax (gs:get-selected-text <sym-id> <sym-action>)
;; @param <sym-id> The name of the component from which to get the selected text.
;; @param <sym-action> The symbol of the event handler which will receive the selected text.

(define (get-selected-text id action)
	(net-send out (string "get-selected-text " id " " action "\n"))
)



;; @syntax (gs:get-text <sym-id> [<sym-action>])
;; @param <sym-id> The name of the component from which to get the text.
;; @param <sym-action> The optional symbol of the event handler which will receive the text.
;;
;; If no <sym-action> is specified the function will block until the text is returned. 
;; After return the text is also available in the variable 'gs:text'.
;; If <sym-action> is specified the function will return immediately and a <sym-action> event 
;; is fired containing the text. 

(define (get-text id action)
	(if action
		(net-send out (string "get-text " id " " action "\n"))
		(begin
			(set 'gs:text nil)
			(net-send out (string "get-text " id "\n"))
			(while (not gs:text) (check-event 10000))
			gs:text)
	)
)


;; @syntax (gs:get-text-position <sym-id>)
;; @param <sym-id> The name of the text component for which the line and column position is returned.
;; @return A list of line and column position of the text caret.
;;

(define (get-text-position id)
	(set 'gs:text-position nil)
	(net-send out (string "get-text-position " id "\n"))
	(while (not gs:text-position) (check-event 10000))
	gs:text-position
)

;; @syntax (gs:get-version)
;; @return The version of newLISP-GS running.
;;
;; After calling the 'gs:get-version' once the version number is also
;; available in 'gs:version'.

(define (get-version)
    (net-send out (string "get-version System\n"))
	(while (not gs:version) (check-event 10000))
	gs:version
)

;; @syntax (gs:goto-text <sym-id> <int-row> <int-column>)
;; @param <sym-id> The name of the text widget.
;; @param <int-row> The row number where to place the cursor.
;; @param <int-column> The column number where to place the cursor.

(define (goto-text id row col)
	(net-send out (string "goto-text " id " " row " " col "\n"))
)



;; @syntax (gs:hide-tag <sym-tag> [<boolean-repaint>])
;; @param <sym-tag> The tag of the group to hide.
;; @param <boolean-repaint> An optional flag to indicate if repainting is required (default is 'true').
;;

(define (hide-tag tag (repaint true))
	(send-out (string "hide-tag " gs:currentCanvas " " tag " " repaint "\n"))
)


;; @syntax (gs:image-button <sym-id> <sym-action> <str-icon-path> [<str-down-icon-path> [<int-width> <int-height>]])
;; @param <sym-id> The name of the image button.
;; @param <sym-action> The name of the event handler.
;; @param <str-icon-path> The path for an image icon.
;; @param <str-down-icon-path> The path for a pressed down image icon.
;; @param <int-width> The optional width of the image button.
;; @param <int-height> The optional height of the image button.

(define (image-button id action icon down-icon x y)
	(if down-icon
		(if (and x y)
			(net-send out (string "image-button " id " " action " " (base64-enc icon) " " (base64-enc down-icon) " " x " " y "\n"))
			(net-send out (string "image-button " id " " action " " (base64-enc icon) " " (base64-enc down-icon) "\n"))
		)
		(net-send out (string "image-button " id " " action " " (base64-enc icon) "\n"))
	)
)

;; @syntax (gs:image-label <sym-id> <str-icon-path> [<str-align>])
;; @param <sym-id> The name of the label.
;; @param <str-ucon-path> A string with the icon file path.
;; @param <str-align> An optional alignment '"left"', '"center"' or '"right"', '"leading"', '"trailing"', '"bottom"' or '"top"'.

(define (image-label id icon align)
	(if align
		(net-send out (string "image-label " id " " (base64-enc icon) " " align "\n"))
		(net-send out (string "image-label " id " " (base64-enc icon) "\n")))
)


;; @syntax (gs:init [<int-port> <str-host> [bool-manual]])
;; @param <int-port> The optional guiserver server port.
;; @param <str-host> The optional remote host of the guiserver.

(define (init (portIn 64001) (host "127.0.0.1") manual)
	; check for server portIn and if this was started by java
	(if (main-args 2) (set 'portIn (int (main-args 2) portIn)))
	; if guiserver.jar did not start this process then guiserver.jar
	; still has to be started, except when manual parameter is true
	(if (and (not (= (main-args 3) "javastart")) (not manual))
		(if (= ostype "Windows")
			(process (string "cmd /c " server-path " " portIn))

			(= ostype "OSX")
			(process (string "/usr/bin/java -jar " server-path " " portIn))

			(env "JAVA_HOME")
			(process (string (env "JAVA_HOME") "/bin/java -jar " server-path " " portIn))

			(process (string "/usr/bin/java -jar " server-path " " portIn))

		)
	)

	(set 'portOut (+ portIn 1))
	(set 'retry 0)
	(set 'out nil)
	(while (not out)
		(if (> retry 200) ; try for 20 seconds
			(begin
				(println "Could not connect to guiserver.jar")
				(when (= ostype "Windows")
					(import "user32.dll" "MessageBoxA")
					(MessageBoxA 0 "Could not connect to guiserver.jar" "Problem connecting" 1)
				)
				(exit))
			(inc retry))
		(set 'out (net-connect host portIn))
		(sleep 100))

	(set 'listenSock (net-listen portOut))
	(set 'in (net-accept listenSock))
	(net-close listenSock)
	(gs:set-utf8 (primitive? MAIN:utf8))
)

;; @syntax (gs:insert-list-item <sym-list-combo> <str-text> <int-index> [<str-text> <int-index>])
;; @param <sym-list-combo> The name of the combo box or list box from which entries are removed.
;; @param <str-text> The text of the list or combo box item to insert.
;; @param <int-index> The index of an entry to add to the list or combo box.
;;
;; When specifying an index of <tt>0</tt> the first item gets inserted at the beginning. 
;; When specifying an index equal or greater to the number of items in the list, the item 
;; is added at the end.

(define (insert-list-item comp)
	(let (	s (string "insert-list-item " comp " ")
			p (args))
		(while p
			(write-buffer s (string (base64-enc (pop p)) " " (pop p) " ")))
		
		(write-buffer s "\n")
		(net-send out s))
)

;; @syntax (gs:insert-tab <sym-tabbed-pane> <sym-component> [<str-text> [<int-index> [<str-icon-path>]]])
;; @param <sym-tabbed-pane> The name of the tabbed pane.
;; @param <sym-component> The name of the component to insert as a tab.
;; @param <str-text> The optional text on the tab.
;; @param <int-index> The optional index where to insert the new tab.
;; @param <str-icon-path> The file path to an optional icon.

(define (insert-tab pane comp text idx icon)
	(if text
		(if idx
			(if icon
				(net-send out (string "insert-tab " pane " " comp " " (base64-enc text) " " idx " " (base64-enc icon) "\n"))
				(net-send out (string "insert-tab " pane " " comp " " (base64-enc text) " " idx "\n")))
			(net-send out (string "insert-tab " pane " " comp " " (base64-enc text) "\n")))
		(net-send out (string "insert-tab " pane " " comp "\n")))
)

;; @syntax (gs:insert-text <sym-id> <str-text> <int-position>)
;; @param <sym-id> The name of the text component.
;; @param <str-text> The text to insert.
;; @param <int-position> The offset position where to insert the text.

(define (insert-text id text position)
	(replace "\r" text "")
	(net-send out (string "insert-text " id " " (base64-enc text) " " position "\n"))
)

;; @syntax (gs:key-event <sym-id> <sym-action>)
;; @param <sym-id> The id of the component to register the action handler.
;; @param <sym-action> The symbol of the action handler.
;;
;; 'gs:key-event' can be used to register a general unspecific key event handler
;; for any component in the system. Since version 1.05 of newLISP-GS this also
;; includes text widgets, which already handle key events using their normal event 
;; handler function. With 'gs:key-event' a second handler function can be registered
;; for text widgets. Both functions will fire on their respective events.
;;
;; Components respond to the following key event types: '"pressed"', '"released"', '"typed"'.
;; 
;; @example
;;
;; (define (key-action id type code modifiers)
;;     (println "id:" id " type:" type " key code:" code " modifiers:" modifiers)
;; )

;; The example shows a handler which prints all key event parameters to the terminal/shell
;; window where the applicaton was started.
;;
;; In order for key events to work, the component for which a key action handler
;; is registered must have the input focus. Use '"gs:request-focus"' to set the
;; input focus for the component.


(define (gs:key-event id action)
	(net-send out (string "key-event " id " " action "\n"))
)

;; @syntax (gs:label <sym-id> <str-text> [<str-align> [<int-width> <int-height>]])
;; @param <sym-id> The name of the label.
;; @param <str-text> The text to appear on the label.
;; @param <str-align> The optional alignment of the text.
;; @param <int-width> The optional width of the label.
;; @param <int-height> The optional height of the label.
;;
;; The following alignment constants can be supplied: '"left"', '"center"', '"right"",
;; '"leading"', '"trailing"', '"bottom"' and "'top'". By default each label text is
;; '"center"' aligned.

(define (label id text align width height)
	(if align
		(if (and width height)
			(net-send out (string "label " id " " (base64-enc text) " " align " " width " " height "\n"))
			(net-send out (string "label " id " " (base64-enc text) " " align "\n")))
		(net-send out (string "label " id " " (base64-enc text) "\n")))
)


;; @syntax (gs:layout <sym-container>)
;; @param <sym-container> The id of the container to lay out.
;;
;; Forces the container to lay out its components again, e.g. after a 'gs:add-to' or 'gs:remove-from'
;; when the container was already visible.

(define (layout id)
	(net-send out (string "layout " id "\n"))
)

;; @syntax (gs:load-text <sym-id> <str-path>)
;; @param <sym-id> The id of the 'gs:text-pane'.
;; @param <str-path> The full path name of the file to load.
;;
;; 'gs:load-text' will load text into a 'gs:text-pane' directly by specifying
;; the path name. During loading, CR-LF line terminators are automatically
;; translated to LF-only line terminators by stripping all CRs from the file. All internal
;; operations of guiserver on text assume LF as a line terminator.

(define (load-text id image-path)
	(net-send out (string "load-text " id " " (base64-enc image-path) "\n"))
)

;; @syntax (gs:listen [<boolean-flag>])
;; @param <boolean-flag> Prevent exit on loss of communication.
;; @return Never returns. Exits the application when the guiserver exits, except when <boolean-flag> is 'true'.

(define (listen flag , event)
	(while (net-receive in event 1000000000 "\n")
		;(println "===>" event "<===")
		(eval-string event))
	(println "server shut down")
	(if (not flag) (exit))
)

;; @syntax (gs:list-box <sym-id> <sym-action> [<str-item-1> ...])
;; @param <sym-id> The name of the list box.
;; @param <sym-action> The name of the event handler.
;; @param <str-item> Zero, one or more text entries in the list box.
;; @syntax (gs:list-box <sym-id> <sym-action> [<list-str-items>])
;; @param <sym-id> The name of the list box.
;; @param <sym-action> The name of the event handler.
;; @param <list-str-items> Zero, one or more text entries in a list.

;; The listbox when clicked with the mouse, or when the [enter] key is
;; presses, will pass the following parameters to the event handler:
;; <br><br>
;; <id> - the id string of the list box<br>
;; <index> - the zero offset index of the highlighted listbox entry<br>
;; <item> - the string of the highlighted listbox entry<br>
;; <click-count> - the number of times the mouse has been clicked<br>

(define (list-box id action)
	(let (	s (string "list-box " id " " action " ")
					entries (if (list? (args 0)) (args 0) (args)) )
		(dolist (item entries)
			(write-buffer s (string (base64-enc item) " ")))
		(write-buffer s "\n")
		(net-send out s))
)


;; @syntax (gs:message-dialog <sym-parent-frame> <str-title> <str-message> [<str-type> [<str-icon-path>]])
;; @param <sym-parent-frame> The symbol name of the parent frame.
;; @param <str-title> The title of the message box.
;; @param <str-message> The message in the message box.
;; @param <str-type> The type of the message box.
;; @param <str-icon-path> The optional path for an icon.
;;
;; The type of the message box can be one of: '"error"', '"information"', '"warning"', '"question"', '"plain"'.
;; The function initiating the message-dialog will return when the dialog is closed.

(define (message-dialog parent title msg (type "plain") icon)
	(if icon
		(net-send out (string "message-dialog " parent " " 
				(base64-enc title) " " (base64-enc msg) " " type  " " (base64-enc icon) "\n"))
		(net-send out (string "message-dialog " parent " " 
				(base64-enc title) " " (base64-enc msg) " " type "\n"))
	)
)

;; @syntax (gs:menu <sym-id> <str-text>)
;; @param <sym-id> The name of the menu.
;; @param <str-text> The title string of the menu.

(define (menu id text)
	(net-send out (string "menu " id " " (base64-enc text) "\n"))
)

;; @syntax (gs:menu-popup <sym-id> <str-text>)
;; @param <sym-id> The name of the menu.
;; @param <str-text> The title string of the menu.

(define (menu-popup id text)
	(net-send out (string "menu-popup " id " " (base64-enc text) "\n"))
)

;; @syntax (gs:menu-bar <sym-frame> [<sym-menu-1> ...])  
;; @param <sym-frame> The name of the frame hosting the menu bar.
;; @param <sym-menu> Zero or more symbol names of menus to be positioned on the menu bar.

(define (menu-bar aframe)
	(let (s (string "menu-bar " aframe " "))
		(doargs (item)
			(write-buffer s (string item " ")))
		(write-buffer s "\n")
		(net-send out s))
)

;; @syntax (gs:menu-item <sym-id> <sym-action> <str-text>)
;; @param <sym-id> The name of the menu item.
;; @param <sym-action> The name of the event handler.
;; @param <str-text> The text to appear for the menu item.

(define (menu-item id action text)
	(net-send out (string "menu-item " id " " action " " (base64-enc text) "\n"))
)

;; @syntax (gs:menu-item-check <sym-id> <sym-action> <str-text> [<bool-selected>])
;; @param <sym-id> The name of the menu item.
;; @param <sym-action> The name of the event handler.
;; @param <str-text> The text to appear for the menu item.
;; @param <bool-selected> An optional flag indicating the selection state 'true' or 'nil' (default).

(define (menu-item-check id action text selected)
	(net-send out (string "menu-item-check " id " " action " " (base64-enc text) " " selected "\n"))
)

;; @syntax (gs:midi-bpm [<int-bpm> [<int-resolution>]]])
;; @param <int-bpm> Beats per minute pay speed. Default is 120 BPM.
;; @param <int-resolution> Ticks per beat. Deafult is 16 ticks per beat;
;;
;; Sets the speed of playing a notes with with either 'gs:play-note' or playing a 
;; sequence with 'gs:play-sequence' in beats per minute (BPM).
;;
;; Before using 'gs:midi-bpm' the default speed is set to 120 BPM, which corresponds
;; to two beats per second, where each beat corresponds to a quarter note of 16 ticks
;; default resolution.
;;
;; While the BPM parameter controls the play-back speed of the sequencer, the
;; resolution is a parameter of the sequence creation itself and must be set before
;; the first 'gs:add-track' call.
;;
;; The preset resolution of 16 ticks per quarter note is the highest which can be set
;; and should be sufficient for all applications.

(define (midi-bpm (bpm 120) (resolution 16))
	(set 'resolution (min resolution 16))
	(net-send out (string "midi-bpm System " (string bpm) " " (string resolution) "\n"))
)
	



;; 
;; @syntax (gs:midi-close)
;;
;; Shut down the MIDI subsystem.

(define (midi-close)
	(net-send out (string "midi-close System\n"))
)


;; @syntax (gs:midi-init [<str-file-path>])
;; @param <str-file-path> The optional file path for a soundbank file.
;;
;; Initialize the MIDI subsystem. If a soundbank file is specified load it,
;; else load the built-in synthesizer's default soundbank.
;;
;; When not using the default soundbank, the function 'gs:get-instruments'
;; should be used first to find out the correct naming of instruments
;; for the 'gs:midi-patch' statements. The soundbank used for testing the
;; demo files 'midi-demo.lsp' and 'midi2-demo.lsp' on Windows is the midsize
;; soundbank available here:
;; @link http://java.sun.com/products/java-media/sound/soundbanks.html http://java.sun.com/products/java-media/sound/soundbanks.html
;; This soundbank has equivalent named instruments to those used in the Mac OS X default JRE installation.
;; Currently only the first 128 instruments in a soundbank are accessed by newLISP-GS.

(define (midi-init soundbank)
	(if soundbank
		(net-send out (string "midi-init System " (base64-enc soundbank) "\n"))
		(net-send out (string "midi-init System\n"))
	)
)

;; @syntax (gs:midi-patch <str-instrument> [<int-channel>])
;; @param <str-instrument> The name of the instrument to attach to a channel.
;; @param <int-channel> The channel for the instrument, default is <tt>0</tt>.
;;
;; An instrument from the current soundbank is attached to a
;; specific channel or to channel <tt>0</tt> if no channel is specified.
;;
;; @example
;; (gs:midi-patch (find "Electric Grand" gs:instruments) 0)

;; In order for the 'gs:instruments' variable to contain a list of instruments,
;; 'gs:get-instruments' must have been called earlier, i.e. after 'gs:midi-init'.

(define (midi-patch program (channel 0))
	(net-send out (string "midi-patch System " (base64-enc program) " " channel "\n"))
)

;; @syntax (gs:mouse-clicked <sym-canvas> <sym-action> [<boolean-tags>])
;; @param <sym-canvas> The id of the canvas to register the action handler.
;; @param <sym-action> The symbol of the action handler.
;; @param <boolean-tags> A 'true' to indicate checking for tags.
;;
;; If <boolean-tags> is 'true', the action event will carry a list of
;; all tags which contained the X,Y coordinates of the mouse.

(define (mouse-clicked cnvs action flag)
	(net-send out (string "mouse-clicked " cnvs " " action " " flag "\n"))
)

;; @syntax (gs:mouse-dragged <sym-canvas> <sym-action>)
;; @param <sym-canvas> The id of the canvas to register the action handler.
;; @param <sym-action> The symbol of the action handler.

(define (gs:mouse-dragged cnvs action)
	(net-send out (string "mouse-dragged " cnvs " " action "\n"))
)

;; @syntax (gs:mouse-event <sym-id> <sym-action>)
;; @param <sym-id> The id of the component to register the action handler.
;; @param <sym-action> The symbol of the action handler.
;;
;; 'gs:mouse-event' can be used to register a general unspecific mouse event handler
;; for any component in the system. Components respond to the following types:
;; '"pressed"', '"released"', '"clicked"',
;; 
;; @example
;;
;; (define (mouse-action id type x y button cnt mods)
;;     (println "id:" id " type:" type " x:" x " y:" y " button:" button " count:" cnt " mods:" mods)
;; )

;; The example shows a handler which prints all mouse event parameters to the terminal/shell
;; window where the applicaton was started.


(define (gs:mouse-event id action)
	(net-send out (string "mouse-event " id " " action "\n"))
)

;; @syntax (gs:mouse-moved <sym-canvas> <sym-action>)
;; @param <sym-canvas> The id of the canvas to register the action handler.
;; @param <sym-action> The symbol of the action handler.
;; @param <boolean-tags> A 'true' to indicate checking for tags.
;;
;; If <boolean-tags> is 'true', the action event will carry a list of
;; all tags which contained the X,Y coordinates of the mouse.

(define (gs:mouse-moved cnvs action flag)
	(net-send out (string "mouse-moved " cnvs " " action " " flag "\n"))
)

;; @syntax (gs:mouse-pressed <sym-canvas> <sym-action> [<boolean-tags>])
;; @param <sym-canvas> The id of the canvas to register the action handler.
;; @param <sym-action> The symbol of the action handler.
;; @param <boolean-tags> A 'true' to indicate checking for tags.
;;
;; If <boolean-tags> is 'true', the action event will carry a list of
;; all tags which contained the X,Y coordinates of the mouse.

(define (gs:mouse-pressed cnvs action flag)
	(net-send out (string "mouse-pressed " cnvs " " action " " flag "\n"))
)

;; @syntax (gs:mouse-released <sym-canvas> <sym-action> [<boolean-tags>])
;; @param <sym-canvas> The id of the canvas to register the action handler.
;; @param <sym-action> The symbol of the action handler.
;; @param <boolean-tags> A 'true' to indicate checking for tags.
;;
;; If <boolean-tags> is 'true', the action event will carry a list of
;; all tags which contained the X,Y coordinates of the mouse.

(define (gs:mouse-released cnvs action flag)
	(net-send out (string "mouse-released " cnvs " " action " " flag "\n"))
)

;; @syntax (gs:mouse-wheel <sym-canvas> <sym-action>)
;; @param <sym-canvas> The id of the canvas to register the action handler.
;; @param <sym-action> The symbol of the action handler.

(define (gs:mouse-wheel cnvs action)
	(net-send out (string "mouse-wheel " cnvs " " action "\n"))
)

;; @syntax (gs:move-tag <sym-tag> <int-dx> <int-dy> [<boolean-repaint>])
;; @param <sym-tag> The tag of the group of objects to move.
;; @param <int-dx> The distance to move on the X-axis.
;; @param <int-dy> The distance to move on the Y-axis.
;; @param <boolean-repaint> An optional flag to indicate if repainting is required (default is 'true').
;;
;; 'gs:move-tag' is the only tag operation which actually changes the 
;; internal data of a drawn object. All other tag operations like
;; 'gs:translate-tag', 'gs:scale-tag', 'gs:rotate-tag' and 'gs:shear-tag'
;; will transform object coordinates only for drawing.

(define (gs:move-tag tag dx dy (repaint true))
	(send-out (string "move-tag " gs:currentCanvas " " tag " " 
		dx " " dy " " repaint "\n"))
)

;; @syntax (gs:mute-track <int-number> [<boolean-on-off>])
;; @param <int-number> The number of the track starting with <tt>0</tt> for the first. Default is <tt>true</tt>.
;; @param <boolean-on-off> The track will be muted with a value of <tt>true</tt>
;;
;; Any other value than <tt>true</tt> will unmute the track again. Muting tracks is practical
;; during music development. The track can only be muted when th sequence has benn started
;; using 'gs:play-sequence'. To completely mute a track the 'gs:mute-track' statement should
;; come right after the 'gs:play-sequece' statement. 

(define (mute-track number (bool true))
	(net-send out (string "mute-track System " number " " (string bool) "\n"))
)

;; @syntax (gs:no-action)
;;
;; Specify as <sym-action> for widgets where no action handler is defined.

(define (no-action))

;; @syntax (gs:open-file-dialog <sym-parent-frame> <sym-action> [<str-directory> [<str-mask> <str-description>]])
;; @param <sym-parent-frame> The parent frame of the file dialog.
;; @param <sym-action> The handler function symbol.
;; @param <str-directory> The initial directory to show.
;; @param <str-mask> An optonal string mask.
;; @param <str-description> An optional mask description.

(define (open-file-dialog parent action dir mask desc)
	(if dir
		(if (and mask desc)
			(net-send out (string "open-file-dialog " parent " " action " " (base64-enc dir) " " 
								(base64-enc mask) " " (base64-enc desc) "\n"))
			(net-send out (string "open-file-dialog " parent " " action " " (base64-enc dir) "\n")))
		(net-send out (string "open-file-dialog " parent " " action "\n")))
)

;; @syntax (gs:panel <sym-id> [<int-width> <int-height>])
;; @param <sym-id> The name of the panel.
;; @param <int-width> The optional width of the panel.
;; @param <int-height> The optional height of the panel.
;;
;; Panels have a flow layout by default. In a flow layout an unsized button will
;; assume the natural size necessary to display the text on it.
;; To change the layout use the 'set-flow-layout' or 'set-grid-layout' functions.

(define (panel id width height)
	(if (and width height)
		(net-send out (string "panel " id " " width " " height "\n"))
		(net-send out (string "panel " id "\n"))
))

;; @syntax (gs:paste-text <sym-id> [<str-text>])
;; @param <sym-id> The name of the text component in which to paste text.
;; @param <str-text> An optional text string to paste instead of the clipboard contents.
;;
;; If the <sym-id> contains selected text, this text gets replaced,
;; otherwise the text is inserted at the current caret position.
;; If no text is given in <str-text>, the text is taken from the clipboard.

(define (paste-text id text)
	(if text
		(net-send out (string "paste-text " id " " (base64-enc text) "\n"))
		(net-send out (string "paste-text " id "\n")))
)

;; @syntax (gs:play-note <int-key> [<int-duration> [<int-velocity> [<int-channel> [<int-bend>]]]])
;; @param <int-key> The note or midi key <tt>0</tt> to <tt>127</tt>.
;; @param <int-duration> The duration of the note in ticks, default is <tt>16</tt> for one beat or quarter note.
;; @param <int-velocity> The velocity/volume of the note between <tt>0</tt> and <tt>127</tt>, default is <tt>64</tt>.
;; @param <int-channel> The channel through which to play the note from <tt>0</tt> to <tt>15</tt>, default is <tt>0</tt>.
;; @param <int-bend> The optional note bend to tune the note lower or higher from '-8192' to '8191'.
;;
;; Before using 'gs:play-note', 'gs:midi-init' should be used to initialize the MIDI system.
;; The key of the note increases in half-tone steps. The key 60 corresponds to a Middle-C.
;; The velocity of the note is usually it's volume and/or brightness, i.e. the speed with which 
;; a key was pressed on an instrument. The channel is <tt>0</tt> by default and assigned to
;; a Piano instrument unless the function 'gs:midi-patch' has been used to change assignment
;; to a different instrument.
;;
;; On Windows and some Linux or other UNIX no MIDI soundbank files are installed by default. Goto 
;; @link http://java.sun.com/products/java-media/sound/soundbanks.html http://java.sun.com/products/java-media/sound/soundbanks.html
;; for instructions how to download and install a soundbank. For the demo files 'mide-demo.lsp' and
;; 'midi2-demo' the midsize quality soundbank was used. On Mac OS X a soundbank is installed by default.
;; The default for the bend parameer is <tt>0</tt> for no bend. Negative values down to '-8192'
;; tune the note lower. Positive values up to '8191' tune the note higher. 
;; The following code is a complete example:
;;
;; @example
;; ; load Guiserver
;; (load (append (env "NEWLISPDIR") "/guiserver.lsp"))
;; (gs:init)
;;
;; ; play a chromatic scale on the default instrument (piano)
;; ; each note a 16th note of 4 ticks and a moderate volume 
;;
;; (gs:midi-init)
;; (gs:midi-patch "Piano" 0)
;; (for (key 24 95) (gs:play-note key 4 95 0))
;; (sleep 2000) ; wait until playing has finished
;; (gs:midi-close)
;;

;; The second example demonstrated usage of the <int-bend> parameter:
;;
;; @example
;; ; play the same note but with different bends below and above the note
;; (gs:midi-patch "Violin" 0)
;; (for (bend -2024 2024 128)
;;     (gs:play-note 80 1 95 0 bend))

;; To play polyphone music of multiple parallel tracks see the function 'gs:add-track' for
;; a complete code example.

(define (play-note key (duration 4) (velocity 64) (channel 0) (bend 0))
	(net-send out (string "play-note System " 
			key " " duration " " velocity " " channel " " bend "\n"))
)

;; @syntax (gs:play-sequence [<int-start> [<int-loop-count> [<int-start-loop> [<int-end-loop>]]])
;; @param <int-start> The starting point in the sequence in ticks. Default is <tt>0</tt> for the beginning.
;; @param <int-loop-count> The number of repetitions for looping. Default is <tt>0</tt> for no looping.
;; @param <int-start-loop> The start of the loop to play in ticks. Default is <tt>0</tt>.
;; @param <int-end-loop> The end of the loop in ticks. Default is <tt>-1</tt> for the end.
;;
;; All parameters are optional. When no parameters are given all tracks in the sequence are
;; sequenced from start to end with no repetiton (loop count of 0). Note that the start-loop and
;; end-loop positions refer only to loop played after playing the full track. After the sequence
;; started playing 'gs:stop-sequence' can be used to stop it at any time. The midi system
;; should not be closed using 'gs:midi-close' before playing has finished or playing will
;; be cut off.
;;
;; See the function 'gs:add-track' for complete code example.

(define (play-sequence (ticks 0) (loop 0) (start 0) (end -1))
	(net-send out (string "play-sequence System " ticks " " loop " " start " " end "\n"))
)


;; @syntax (gs:play-sound <str-file-path>)
;; @param <str-file-path> The path and file name of the sound file.
;;
;; On most OS platforms '.au' and '.wav' sound file formats are supported.

(define (play-sound file-name)
	(net-send out (string "play-sound System " (base64-enc file-name) "\n"))
)


;; @syntax (gs:progress-bar <sym-id> <int-min> <in-max> <int-initial-value>)
;; @param <sym-id> The symbols of the progress bar.
;; @param <int-min> The minimum value of the slider.
;; @param <int-max> The maximum value of the slider.
;; @param <int-initial-value> The initial value of the slider.

(define (progress-bar id posmin posmax posinit)
	(net-send out (string "progress-bar " id " " posmin " " posmax " " posinit "\n"))
)

;; @syntax (gs:radio-button <sym-id> <sym-action> [<str-text> [<bool-selected>]])
;; @param <sym-id> The name of the radio button.
;; @param <sym-action> The name of the event handler.
;; @param <str-text> The optional text of the radio button.
;; @param <bool-seected> An optional flag 'true' or 'nil' (default) indicating the initial state of the radio button.

(define (radio-button id action text selected)
	(if text
		(net-send out (string "radio-button " id " " action " " (base64-enc text) " " selected "\n"))
		(net-send out (string "radio-button " id " " action "\n")))
)

;; @syntax (gs:redo-text <sym-id>)
;; @param <sym-id> The id of the 'gs:text-pane' where to perform a redo operation.

(define (redo-text id)
	(net-send out (string "redo-text " id "\n"))
)

;; @syntax (gs:remove-from <sym-container> <sym-component> [<sym-component> ...])
;; @param <sym-container> The container from which to remove a component.
;; @param <sym-component> One or more optional components to remove.
;;

(define (remove-from id)
	(let (s (string "remove-from " id " "))
        (doargs (item)
            (write-buffer s (string item " ")))
        (write-buffer s "\n")
        (net-send out s))
)

;; @syntax (gs:remove-list-item <sym-list-combo> <int-index> [<int-index> ...])
;; @param <sym-list-combo> The name of the combo box or list box from which entries are removed.
;; @param <int-index> The index of an entry to remove from the list or combo box.
;;
;; When specifying an index of <tt>0</tt>, the first item gets removed. When specifying an
;; index equal or greater to the number of items in the list, the item is removed at the end.

(define (remove-list-item comp)
	(let (s (string "remove-list-item " comp " "))
		(doargs (item)
			(write-buffer s (string item " ")))
		(write-buffer s "\n")
		(net-send out s))
)

;; @syntax (gs:remove-tab <sym-tabbed-pane> [<int-index>])
;; @param <sym-tabbed-pane> The name of the tabbed pane.
;; @param <int-index> The optional index of the tab to remove. The default is <tt>0</tt> for the first tab.

(define (remove-tab pane idx)
	(if idx
		(net-send out (string "remove-tab " pane " " idx "\n"))
		(net-send out (string "remove-tab " pane "\n")))
)

;; @syntax (gs:request-focus <sym-id> [<int-tab-index>])
;; @param <sym-id> The name of the component for which to request focus.
;; @param <int-tab-index> The index of a tab for which focus is requested.

(define (request-focus id idx)
	(if idx
		(net-send out (string "request-focus " id " " idx "\n"))
		(net-send out (string "request-focus " id "\n")))
)

;; @syntax (gs:reorder-tags <list-tags>)
;; @param <list-tags> The list of tag symbols or tag string names in the new order of display.
;;
;; The re-ordering itself will not repaint the canvas use 'gs:update' to repaint the current 
;; canvas after using 'gs:reorder-tags'. The list of tags can be given as either a list of
;; tags symbols or name strings. Tags not appearing in <list-tags> will be deleted.

(define (reorder-tags tags)
	(let (s (string "reorder-tags " gs:currentCanvas " "))
		(dolist (tag tags)
			(write-buffer s (string tag " ")))
		(write-buffer s "\n")
		(net-send out s)
	)
)

;; @syntax (gs:rotate-tag <sym-tag> <float theta> <int-x> <int-y> [<boolean-repaint>])
;; @param <sym-tag> The tag group to rotate.
;; @param <float-theta> The rotation angle in degrees (0 - 360).
;; @param <int-x> The X-coordinate of the rotation center.
;; @param <int-y> The Y-coordinate of the rotation center.
;; @param <boolean-repaint> An optional flag to indicate if repainting is required (default is 'true').
;;
;; Like all tag operations, multiple 'gs:rotate-tag' operations are cumulative.

(define (rotate-tag tag angle x y (repaint true))
	(send-out (string "rotate-tag " gs:currentCanvas " " tag " " angle " " 
		x " " y " " repaint "\n"))
)

;; @syntax (gs:run-shell <id-text-area> <str-command> <str-args>)
;; @param <idx-text-area> The id of the text area to wich a shell process will be attached.
;; @param <str-command> The command string to start the shell process.
;; @param <str-args> The arguments of the command (max 8 arguments).

(define (run-shell id command arguments)
	(net-send out (string "run-shell " id " " (base64-enc command)  " " (base64-enc arguments) "\n"))
)

;; @syntax (gs:save-file-dialog <sym-parent-frame> <sym-action> [<str-directory> [<str-initial-file> [<str-mask> <str-description>]]])
;; @param <sym-parent-frame> The parent frame of the file dialog.
;; @param <sym-action> The handler function symbol.
;; @param <str-directory> The initial directory to show.
;; @param <str-file> The initial file name.
;; @param <str-mask> An optional string mask.
;; @param <str-description> An optional mask description.

(define (save-file-dialog parent action dir file mask desc)
	(if dir
		(if file
			(if (and mask desc)
				(net-send out (string "save-file-dialog " parent " " action " " (base64-enc dir) " " (base64-enc file)
																		" " (base64-enc mask) " " (base64-enc desc) "\n"))
				(net-send out (string "save-file-dialog " parent " " action " " (base64-enc dir) " " (base64-enc file) "\n")))
			(net-send out (string "save-file-dialog " parent " " action " " (base64-enc dir) "\n")))
		(net-send out (string "save-file-dialog " parent " " action "\n")))
)

;; @syntax (gs:save-sequence <str-file-path>)
;; @param <str-file-name> The name of the MIDI file to save to.
;;
;; Save the contents of a sequence created with 'gs:add-track' to a MIDI file.
;; The file always should have the extension <tt>.mid</tt>.
;;
;; Note that all MIDI files created with 'gs:save-sequence' will play back at a fixed
;; speed of 120 BPM. Therefore, when creating sequences for recording using 'gs:add-track',
;; they should be timed for a play-back speed of 120 BPM.
;;
;; To change the speed for replay from a saved MIDI file the resolution parameter can
;; be chaged from it's default of 16 tick per beat using the second optional parameter
;; of 'gs:midi-bpm'. In this case the resolution parameter should be adjusted before
;; calling 'gs:add-track' the first time.

(define (save-sequence file-path)
	(net-send out (string "save-sequence System " (base64-enc file-path) "\n"))
)

;; @syntax (gs:save-text <sym-id> <str-path>)
;; @param <sym-id> The id of the 'gs:text-pane'.
;; @param <str-path> The full path name of the file to save.
;;
;; This function will write text back from a 'gs:text-pane' directly
;; by specifying a path name only. Line feed characters (ASCII 10)
;; are used as line terminators. If this behavior is not desired,
;; as is the case with Windows text files, then 'gs:get-text' should
;; be used instead. A program can then add CR characters using a 
;; newLISP 'replace', i.e. '(replace "\n" text "\r\n")' before
;; saving the text to a file.

(define (save-text id image-path)
	(net-send out (string "save-text " id " " (base64-enc image-path) "\n"))
)


;; @syntax (gs:scale-tag <sym-tag> <float-x> <float-y> [<boolean-repaint>])
;; @param <sym-tag> The tag group to scale.
;; @param <float-x> The X scaling factor.
;; @param <float-y> The Y scaling factor.
;; @param <boolean-repaint> An optional flag to indicate if repainting is required (default is 'true').
;;
;; 'gs:scale' scales the object to draw relative to the '0,0' point of
;; the coordinate system. This means if a object is not at the center it
;; will not only change in size when scaled but also change the distance
;; to the center point of the coordinate system, moving away when scaling
;; up with scale factor bigger '1.0' and moving closer to the center
;; when scaling down using factors smaller than '1.0'.
;;
;; This means that objects which will be scaled should be defined in
;; coordinates relative to their center point. Then a 'gs:translate-tag'
;; command should be used to place the object to correct place:
;;
;; @example
;; (gs:circle 'C 0 0 50)
;; (gs:gs:translate-tag 'C 200 100)
;; ...
;; (gs:scale-tag 'C 1.1 1.1)

;; In the example the circle, although defined for '0,0', will be displayed
;; at the '200,200' position because of the 'gs:translate-tag' statement. When
;; later scaling the circle will get bigger but stay in place.
;; Like all tag operations, multiple 'gs:scale-tag' operations are cumulative.

(define (scale-tag tag sx sy (repaint true))
	(send-out (string "scale-tag " gs:currentCanvas " " tag " " 
		sx " " sy " " repaint "\n"))
)

;; @syntax (gs:select-list-item <sym-id> <str-item> [<boolean-flag>])
;; @param <sym-id> The name of the list or combo box.
;; @param <str-item> The item to select.
;; @param <boolean-flag> An optional flag only for list boxes to force scrolling to the selected entry.
;;
;; On combo boxes the optional <boolean-flag> has no effect. The selected entry will always
;; appear as the visible text of the combo box. The flag has either the value 'true' or 'nil'.

(define (select-list-item id item flag)
	(net-send out (string "select-list-item " id " " (base64-enc item) " " flag "\n")))


;; @syntax (gs:select-text <sym-id> <int-from> [<int-to>])
;; @param <sym-id> The ame of the text component.
;; @param <int-from> Start offset of selection.
;; @param <int-to> Optional end offset of selection.
;;
;; If no <int-to> end offset is given, 'gs:select-text' will
;; select to the end of the text.

(define (select-text id from to)
	(if to
		(net-send out (string "select-text " id " " from " " to "\n"))
		(net-send out (string "select-text " id " " from "\n")))
)

;; @syntax (gs:scroll-pane <sym-id> <sym-widget> [<int-width> <int-height> <sym-w-col> <sum-w-row> <sym-w-corner>])
;; @param <sym-id> The name of the scroll pane.
;; @param <sym-widget> The component in the scroll pane to be scrolled.
;; @param <int-width> The optional width of the scroll pane.
;; @param <int-height> The optional height of the scroll pane.
;; @param <sym-col> The optional table widget for a custom column header.
;; @param <sym-row> The optional table widget for a custom row header
;; @param <sym-corner> The optional widget component in the upper left corner.
;;
;; @example
;; (gs:scroll-pane 'scroll 'data-table 700 600 'col-table 'row-table 'Canvas)
;;

(define (scroll-pane id widget width height col-table row-table upper-left-corner )
	(if (and col-table row-table upper-left-corner)
		(net-send out (string "scroll-pane " id " " widget " " width " " height " " col-table " " row-table " " upper-left-corner "\n"))
		(if (and width height)
			(net-send out (string "scroll-pane " id " " widget " " width " " height "\n"))
			(net-send out (string "scroll-pane " id " " widget "\n"))))
)

;; @syntax (gs:set-accelerator <sym-menu-item> <str-keystroke>)
;; @param <sym-menu-item> The name of the menu item for which an accelerator key is set.
;; @param <str-keystroke> A text string identifying the keystroke.
;;
;; The following rules are used to create keystroke strings:
;;
;; Syntax:
;; <blockquote><pre>
;; modifiers* (typedID | pressedReleasedID)
;; modifiers := shift | control | ctrl | meta | alt | button1 | button2 | button3
;; typedID := typed typedKey
;; typedKey := string of length 1 giving Unicode character.
;; pressedReleasedID := (pressed | released) key
;; key := KeyEvent key code name, i.e. the name following "VK_".
;; </pre></blockquote>
;; Examples:
;; <blockquote><pre>
;; "INSERT"
;; "control DELETE"
;; "alt shift X"
;; "alt shift released X"
;; "typed a"
;; </pre></blockquote>
;; Note that the <i>apple</i> key on MacOS X is the 'meta' key.
;; The 'alt' on MacOS X is the <i>option</i> key.
;; For letters use uppercase. 
;; Keys are added to the menu item display automatically on all platforms.

(define (set-accelerator item key)
	(net-send out (string "set-accelerator " item " " (base64-enc key) "\n"))
)

;; @syntax (gs:set-anti-aliasing <boolean-flag>)
;; @param <boolean-flag> The anti aliasing setting for the current canvas 'true' or 'nil'.
;;
;; The default setting is 'true'.

(define (gs:set-anti-aliasing flag)
	(net-send out (string "set-anti-aliasing " gs:currentCanvas " " flag "\n"))
)

;; @syntax (gs:set-background <sym-id> <float-red> <float-green> <float-blue> [<float-alpha>])
;; @param <sym-id> The name of the component for which to set the color.
;; @param <float-red> The red color component expressed as a number between 0.0 and 1.0.
;; @param <float-green> The green color component expressed as a number between 0.0 and 1.0.
;; @param <float-blue> The blue color component expressed as a number between 0.0 and 1.0.
;; @param <float-alpha> The transparency of the color expressed as a number between 0.0 (fully transparent)and 1.0 (completely opaque).
;;
;; @syntax (gs:set-background <sym-id> <list-rgb> [<float-alpha>])
;; @param <sym-id> The name of the component for which to set the color.
;; @param <list-rgb> The rgb color can be given as a list of three numbers.
;; @param <float-alpha> The transparency of the color expressed as a number between 0.0 (fully transparent)and 1.0 (completely opaque).
;;
;; Note 'set-background' is the same as 'set-color'.

(define (set-background id red green blue alpha)
	(if (list? red) 
		(begin
			(set 'alpha (or green 1.0)) 
			(map set '(red green blue) red))
		(set 'alpha (or alpha 1.0)))
	(net-send out (string "set-color " id " " red " " green " " blue " " alpha "\n"))
)

;; @syntax (gs:set-bevel-border <sym-id> <str-type>)
;; @param <sym-id> The name of the component.
;; @param <str-type> The type of the bevel '"raised"' or '"lowered"'.

(define (set-bevel-border id type)
	(net-send out (string "set-bevel-border " id " " type "\n"))
)

;; @syntax (gs:set-border-layout <sym-container>  [<int-hgap> <int-vgap>])
;; @param <sym-container> The name of the container for which border layout is set.
;; @param <int-hgap> The horizontal gap between components in the border layout.
;; @param <int-vgap> The vertical gap between components in the border layout.
;;
;; Border layout divides the layout into 5 zones labeled '"north"', '"west"',
;; '"center"', '"east"' and '"south"'. These string constants are used in
;; the 'gs:add-to' command when adding components to a border layout.
;;
;; In a border layout each component will take the maximum size if components
;; are not sized. If components are sized only some dimensions will be honored.
;; The '"north"' and '"south"' components will stretch to maximum width and
;; assume the height given in a size parameter of the component. The '"east"'
;; and '"west"' components will stretch to the maximum height available assuming
;; their width specified earlier. The '"center"' component will take the left over
;; maximum space.

(define (set-border-layout container hgap vgap)
	(if (and hgap vgap)
		(net-send out (string "set-border-layout " container " "  hgap " " vgap "\n"))
		(net-send out (string "set-border-layout " container "\n")))
)

;; @syntax (gs:set-canvas <sym-id>)
;; @param <sym-id> The id of the canvas to switch to.
;;
;; The canvas in <sym-id> must have been created earlier with a 'gs:canvas' 
;; statement. All graphics operations which do not take a canvas as argument
;; will automatically refer to this current canvas. If no 'gs:set-canvas' is
;; used, the current canvas is assumed to be the last one created.

(define (gs:set-canvas id)
	(set 'gs:currentCanvas id)
	(net-send out (string "set-canvas " id "\n"))
)


;; @syntax (gs:set-caret <sym-id> <int-offset>)
;; @param <sym-id> The name of the component for which to set the cursor caret.
;;
;; The functions has the same effect as calling 'gs:select-text' with the same 
;; offset for the dot and mark position.

(define (gs:set-caret id offset)
	(net-send out (string "set-caret " id " " offset "\n"))
)

;; @syntax (gs:set-caret-color <sym-id> <float-red> <float-green> <float-blue>)
;; @param <sym-id> The name of the component for which to set the color.
;; @param <float-red> The red color component expressed as a number between 0.0 and 1.0.
;; @param <float-green> The green color component expressed as a  number between 0.0 and 1.0.
;; @param <float-blue> The blue color component expressed as a  number between 0.0 and 1.0.
;; @syntax (gs:set-caret-color <sym-id> <list-rgb> [<float-alpha>])
;; @param <sym-id> The name of the component for which to set the color.
;; @param <list-rgb> The rgb color can be given as a list of three numbers.

(define (set-caret-color id red green blue)
	(if (list? red) 
		(map set '(red green blue) red))
	(net-send out (string "set-caret-color " id " " red " " green " " blue "\n"))
)

;; @syntax (gs:set-color <sym-id> <float-red> <float-green> <float-blue> [<float-alpha>])
;; @param <sym-id> The name of the component for which to set the color.
;; @param <float-red> The red color component expressed as a number between 0.0 and 1.0.
;; @param <float-green> The green color component expressed as a number between 0.0 and 1.0.
;; @param <float-blue> The blue color component expressed as a number between 0.0 and 1.0.
;; @param <float-alpha> The transparency of the color expressed as a number between 0.0 (fully transparent)and 1.0 (completely opaque).
;;
;; @syntax (gs:set-color <sym-id> <list-rgb> [<float-alpha>])
;; @param <sym-id> The name of the component for which to set the color.
;; @param <list-rgb> The rgb color can be given as a list of three numbers.
;; @param <float-alpha> The transparency of the color expressed as a number between 0.0 (fully transparent)and 1.0 (completely opaque).
;;
;; Note that 'set-color' is the same as 'set-background'.

(define set-color set-background)

;; @syntax (gs:set-cursor <sym-id> <str-shape>)
;; @param <sym-id> The name of the frame, dialog or window.
;; @param <str-shape> The string describing the cursor shape.
;;
;; The cursor shape can be one of the following:
;; <pre>
;;  "default"
;;  "crosshair"
;;  "text"
;;  "wait"
;;  "sw-resize"
;;  "se-resize"
;;  "nw-resize"
;;  "ne-resize"
;;  "n-resize"
;;  "s-resize"
;;  "w-resize"
;;  "e-resize"
;;  "hand"
;;  "move"
;; </pre>

(define (set-cursor id shape)
	(net-send out (string "set-cursor " id " " shape "\n"))
)

;; @syntax (gs:set-echo-char <sym-id> [<str-cover-char>])
;; @param <sym-id> The name of the component for which text is set.
;; @param <str-cover-char> Cover character for password entry.
;; @example
;; (gs:set-echo-char 'TheTextField "*")
;; (gs:set-echo-char 'TheTextField)   ; no echo, behave as normal text field
 
(define (set-echo-char id text)
  (if (and text (> (length text) 0))
  	(net-send out (string "set-echo-char " id " " (base64-enc text) "\n"))
  	(net-send out (string "set-echo-char " id "\n")))
)

;; If no <str-cover-char> is specyfied or the string in <str-cover-char> is of
;; 0 length, then the text field behaves as a normal text field.


;; @syntax (gs:set-editable <sym-id> <boolean-editable>)
;; @param <sym-id> The name of the text widget.
;; @param <boolean-editable> The flag 'true' or 'nil' to indicate if this text widget can be edited.

(define (set-editable id flag)
	(net-send out (string "set-editable " id " " flag "\n"))
)

;; @syntax (gs:set-flow-layout <sym-container> [<str-alignment> [<int-hgap> <int-vgap>]])
;; @param <sym-container> The name of the container for which flow layout is set.
;; @param <sym-alignment> The alignment of the flow layout '"left"', '"center"' or '"right"'.
;; @param <int-hgap> The horizontal gap between components in the flow layout.
;; @param <int-vgap> The vertical gap between components in the flow layout.
;;
;; The flow layout lets components appear in their natural or preferred size. The preferred
;; size of a component is set using the function 'gs:set-size'. Button-type widgets and
;; combo boxes will take as much space as necessary to show the included text.

(define (set-flow-layout id (direction "left") hGap vGap)
	(if (and hGap vGap)
		(net-send out (string "set-flow-layout " id " " direction  " " hGap " " vGap "\n"))
		(net-send out (string "set-flow-layout " id " " direction "\n"))
	)
)

;; @syntax (gs:set-font <sym-id> <str-family> <int-size> <str-type>)
;; @param <sym-id> The name of the component for which to set the text font.
;; @param <str-familiy> The family of the font, e.g.: '"Monospaced"', '"Serif"', '"Sans Serif"'.
;; @param <int-size> The font size in points.
;; @param <str-type> The type of the font, one or more of '"plain"', '"bold"', '"italic"'.
;;
;; More than the above noted families are available depending on the platform.

(define (set-font id family size type)
	(net-send out (string "set-font " id " " (base64-enc family) " " size " " type "\n"))
)

;; @syntax (gs:set-foreground <sym-id> <float-red> <float-green> <float-blue> [<float-alpha>])
;; @param <sym-id> The name of the component for which to set the color.
;; @param <float-red> The red color component expressed as a number between 0.0 and 1.0.
;; @param <float-green> The green color component expressed as a  number between 0.0 and 1.0.
;; @param <float-blue> The blue color component expressed as a  number between 0.0 and 1.0.
;; @param <float-alpha> The transparency of the color expressed as a number between 0.0 (fully transparent)and 1.0 (completely opaque).
;;
;; @syntax (gs:set-foreground <sym-id> <list-rgb> [<float-alpha>])
;; @param <sym-id> The name of the component for which to set the color.
;; @param <list-rgb> The rgb color can be given as a list of three numbers.
;; @param <float-alpha> The transparency of the color expressed as a number between 0.0 (fully transparent)and 1.0 (completely opaque).
;;
;; The foreground color is the color of the text in a component.

(define (set-foreground id red green blue alpha)
	(if (list? red) 
		(begin
			(set 'alpha (or green 1.0)) 
			(map set '(red green blue) red))
		(set 'alpha (or alpha 1.0)))
	(net-send out (string "set-foreground " id " " red " " green " " blue " " alpha "\n"))
)


;; @syntax (gs:set-grid-layout <sym-container> <int-rows> <int-columns> [<int-hgap> <int-vgap>])
;; @param <sym-container> The name of the container for which grid layout is set.
;; @param <int-rows> The number of rows in the layout grid.
;; @param <int-columns> The number of columns in the layout grid.
;; @param <int-hgap> The horizontal gap between components in the grid layout.
;; @param <int-vgap> The vertical gap between components in the grid layout.
;;
;; In a grid layout each component will assume the maximum size the grid cell allows
;; regardless of sizes preset using 'gs:set-size' Because of this grid layout cells are
;; frequently filled with panels using 'gs:panel' which have flow layout by default
;; and allow deliberate sizing of components using 'gs:set-size'.

(define (set-grid-layout container rows cols hgap vgap)
	(if (and hgap vgap)
		(net-send out (string "set-grid-layout " container " " rows " " cols " " hgap " " vgap "\n"))
		(net-send out (string "set-grid-layout " container " " rows " " cols "\n")))
)

;; @syntax (gs:set-icon <sym-id> <str-icon-path> [<int-index>])
;; @param <sym-id> The name of a button or label or menu-item for which to set an icon.
;; @param <str-icon-path> The file path of the icon to be set.
;; @param <int-index> If <sym-id> is a tabbed pane <int-index> is the index of the tab.

(define (set-icon comp text idx)
	(if idx
    	(net-send out (string "set-icon " comp " " (base64-enc text) " " idx "\n"))
    	(net-send out (string "set-icon " comp " " (base64-enc text) "\n"))
	)
)

;; @syntax (gs:set-look-and-feel <str-look>)
;; @param <str-look> The class description string for the look and feel of the application.
;;
;; The following strings can be tried in <str-look>, but not all will work on a specific
;; platform. On the Mac the default look-and-feel is built-in to the JVM as the default
;; style. The '"MacLookAndFeel"' is not available as an explicit flavor here, but may be
;; on other platforms.
;; <pre>
;;    '"com.sun.java.swing.plaf.motif.MotifLookAndFeel"'<br>
;;    '"javax.swing.plaf.metal.MetalLookAndFeel"'<br>
;;    '"com.sun.java.swing.plaf.windows.WindowsLookAndFeel"'<br>
;;    '"javax.swing.plaf.mac.MacLookAndFeel"'<br>
;;    '"com.sun.java.swing.plaf.gtk.GTKLookAndFeel"'
;; </pre>

(define (set-look-and-feel look)
	(net-send out (string "set-look-and-feel System " look "\n"))
)

;; @syntax (gs:set-paint <list-rgb>)
;; @param <list-rgb> The current paint used for outlines, text and fill color.

(define (gs:set-paint color)
	(net-send out (string "set-paint " gs:currentCanvas " " (color 0) " " (color 1) " " (color 2) "\n"))
)

;; @syntax (gs:set-pressed-icon <sym-id> <str-icon-path>)
;; @param <sym-id> The name of the button, image button or toggle button.
;; @param <str-icon-path> The file path of the icon or image to be set to the button in pressed state.
;;
;; By default a small grey dot is shown on image buttons when in a pressed state.

(define (set-pressed-icon comp text)
    (net-send out (string "set-pressed-icon " comp " " (base64-enc text) "\n"))
)

;; @syntax (gs:set-resizable <sym-frame> <boolean-resizable>)
;; @param <sym-frame> The name of the frame window.
;; @param <bbolean-resizable> The flag 'true' or 'nil' to indicate if a frame can be resized by the user.

(define (set-resizable id flag)
	(net-send out (string "set-resizable " id " " flag "\n"))
true
)

;; @syntax (gs:set-rotation <float-angle>)
;; @param <float-angle> The angle in degrees (0 - 360) of the canvas rotation.
;;
;; Unlike the 'gs:rotate-tag' operation which is cumulative, 'gs:set-rotation'
;; will set an absolute rotation value each time it is called.

(define (gs:set-rotation angle)
	(net-send out (string "set-rotation " gs:currentCanvas " " angle "\n"))
)

;; @syntax (gs:set-scale <float-x> <float-y>)
;; @param <float-x> The X-scale value of the current canvas.
;; @param <float-y> The Y-scale value of the current canvas.
;;
;; Unlike the 'gs:scale-tag' operation which is cumulative, 'gs:set-scale'
;; will set an absolute scale value each time it is called.

(define (gs:set-scale x y)
	(net-send out (string "set-scale " gs:currentCanvas " " x " " y "\n"))
)

;; @syntax (gs:set-selected <sym-id> <boolean-selected> [<sym-id> <boolean-selected>])
;; @param <sym-id> The name of the toggle or radio button or check box or menu item.
;; @param <boolean-selected> A flag of 'true' or 'nil' to indicated the selection state.
;;
;; More then one toggle control may be set selected or unselected.

(define (set-selected id flag)
	(let (s (string "set-selected " id " " flag " "))
		(doargs (item)
			(write-buffer s (string item " ")))
		(write-buffer s "\n")
		(net-send out s)
	)
)

;; @syntax (gs:set-selection-color <sym-id> <float-red> <float-green> <float-blue> [<float-alpha>])
;; @param <sym-id> The name of the component for which to set the text selection color.
;; @param <float-red> The red color component expressed as a number between 0.0 and 1.0.
;; @param <float-green> The green color component expressed as a number between 0.0 and 1.0.
;; @param <float-blue> The blue color component expressed as a number between 0.0 and 1.0.
;; @param <float-alpha> The transparency of the color expressed as a number between 0.0 (fully transparent)and 1.0 (completely opaque).
;;
;; @syntax (gs:set-selection-color <sym-id> <list-rgb> [<float-alpha>])
;; @param <sym-id> The name of the component for which to set the text selection color.
;; @param <list-rgb> The rgb color can be given as a list of three numbers.
;; @param <float-alpha> The transparency of the color expressed as a number between 0.0 (fully transparent)and 1.0 (completely opaque).
;;
;; Note 'set-background' is the same as 'set-color'.

(define (set-selection-color id red green blue alpha)
	(if (list? red) 
		(begin
			(set 'alpha (or green 1.0)) 
			(map set '(red green blue) red))
		(set 'alpha (or alpha 1.0)))
	(net-send out (string "set-selection-color " id " " red " " green " " blue " " alpha "\n"))
)


;; @syntax (gs:set-size <sym-id> <int-width> <int-height>)
;; @param <sym-id> The name of the component of which a preferred size is set.
;; @param <int-width> The preferred width of the component.
;; @param <int-height> The preferred height of the component.
;;
;; Note that not all layouts allow setting the size of a component. The grid and
;; border layouts will size the component to its maximum possible in the layout.

(define (set-size id width height)
	(net-send out (string "set-size " id " " width " " height "\n"))
)

;; @syntax (gs:set-stroke <float-width> [<str-cap> [<str-join> [<float-miterlimit>]]])
;; @param <float-width> The width for drawing lines and outlines in shapes.
;; @param <str-cap> One of optional '"butt"' (default), '"round"' or '"sqare"'.
;; @param <str-join> One of optional '"miter"' (default), '"bevel"' or '"round"'
;;
;; For a <float-width> 0f 0.0 the thinnest possible line width be chosen. 
;; Join is the decoration applied at the intersection of two path segments and at the 
;; intersection of the endpoints.
;; Cap is the decoration applied to the ends of unclosed subpaths and dash segments.
;; The <float-miterlimit> should be greater or equal 1.0.

(define (gs:set-stroke width cap jn limit)
	(if cap
		(if join
			(if limit
				(net-send out (string "set-stroke " gs:currentCanvas " " width " " cap " " jn " " limit "\n"))
				(net-send out (string "set-stroke " gs:currentCanvas " " width " " cap " " jn "\n"))
			)
			(net-send out (string "set-stroke " gs:currentCanvas " " width " " cap "\n"))
		)
		(net-send out (string "set-stroke " gs:currentCanvas " " width "\n"))
	)
)

;; @syntax (gs:set-syntax <sym-id> <str-type>)
;; @param <sym-id> The name of the text pane for syntax coloring is enabled or disabled.
;; @param <str-type> A string '"lsp"', '"c"', '"cpp"', '"java"' or '"php"' to indicate the 
;; syntax desired, or 'nil' to switch off syntax highlighting.
;;
;; Colors for syntax highlighting are preselected for a white background, but can be changed using
;; the following functions: 'gs:set-background', 'gs:set-foreground', 'gs:set-caret', 'gs:set-selection-color'
;; and 'gs:set-syntax-colors'.

(define (set-syntax id type)
	(net-send out (string "set-syntax " id " " type "\n"))
)

;; @syntax (gs:set-syntax-colors <list-rgb-comment> <list-rgb-keyword> <list-rgb-string> <list-rgb-number> <list-rgb-quoted> <list-rgb-parentheses>)
;; @param <list-rgb-comment> The color for comments.
;; @param <list-rgb-keyword> The color for reserved keywords.
;; @param <list-rgb-string> The color for strings.
;; @param <list-rgb-number> The color for numbers.
;; @param <list-rgb-quoted> The color for the quote and quoted symbols.
;; @param <list-rgb-parentheses> The color for parenthesis.
;;
;; Syntax highlighting colors are given as lists of red, green and blue values between 0.0 and 1.0. 
;; Depending on the syntax colors and the foreground and background colors set for the text pane, 
;; the caret color and color for selected text should also be changed. Only text widgets created 
;; using 'gs:text-pane' feature syntax highlighting.

(define (set-syntax-colors comment keyword text number quoted parentheses)
	(let (	components (append comment keyword text number quoted parentheses)
			str "set-syntax-colors System ")
		(dolist (c components)
			(write-buffer str (string c " ")))
		(write-buffer str "\n")
		(net-send out str))
)

;; @syntax (gs:set-tab-size <sym-id> <int-size>)
;; @param <sym-id> The name of the text area component.
;; @param <int-size> The tabulator size.
;;
;; Note that 'gs:set-tab-size' will only work with fixed spaced fonts.

(define (set-tab-size id size)
	(net-send out (string "set-tab-size " id " " size "\n"))
)

;; @syntax (gs:set-text <sym-id> <str-text> [<int-index>])
;; @param <sym-id> The name of the component for which text is set.
;; @param <str-text> The text to be set in the component.
;; @param <int-index> The index for a tab if the <sym-id> is a tabbed pane.

(define (set-text id text idx)
	(replace "\r" text "")
	(if idx
		(net-send out (string "set-text " id " " (base64-enc text) " " idx "\n"))
		(net-send out (string "set-text " id " " (base64-enc text) "\n"))
	)
)

;; @syntax (gs:set-titled-border <sym-component> <str-title>)
;; @param <sym-component> The name of the component.
;; @param <str-title> The text in the titled border around the component.
;;
;; The component is usually a 'panel'.

(define (set-titled-border id text)
	(net-send out (string "set-titled-border " id " " (base64-enc text) "\n"))
)

;; @syntax (gs:set-tool-tip <sym-id> <str-text>)
;; @param <sym-id> The name of the widget for which to supply a tool tip.
;; @param <str-text> The text of the tool tip.
;;
;; The tool tip text is shown when leaving the mouse over the widget for certain
;; amount of time.

(define (set-tool-tip id text)
    (net-send out (string "set-tool-tip " id " " (base64-enc text) "\n"))
)

;; @syntax (gs:set-trace <boolean-flag>)
;; @param <boolean-flag> The flag 'true' or 'nil'.

(define (set-trace flag)
	(net-send out (string "set-trace System " flag "\n"))
)

;; @syntax (gs:set-translation <int-x> <int-y>)
;; @param <int-x> The X-translation value of the current canvas.
;; @param <int-y> The Y-translation value of the current canvas.
;;
;; Translates the current origin of the current canvas to the point in <int-x> <int-y>.
;; Unlike the 'gs:translate-tag' operation which is cumulative, 'gs:set-translation'
;; will set an absolute translation value each time it is called.

(define (gs:set-translation x y)
	(net-send out (string "set-translation " gs:currentCanvas " " x " " y "\n"))
)

;; @syntax (gs:set-utf8 <boolean-flag>)
;; @param <boolean> The flag 'true' or 'nil' to indicate if in UTF-8 mode.
;;
;; When set in UTF-8 mode, guiserver will convert files to UTF-8 encoding
;; when loading and saving files. On Mac OS X UTF-8 mode is by default enabled.
;; On startup guiserver.lsp will detect if newLISP is UTF-8 enabled and
;; switch the mode in Guiserver accordingly using 'gs:set-utf8'.

(define (gs:set-utf8 flag)
	(net-send out (string "set-utf8 System " flag "\n"))
)

;; @syntax (gs:set-value <sym-id> <int-value>)
;; @param <sym-id> The name of a slider or progress bar for which to set the value.
;; @param <int-value> The integer value of the name to be set.
;;
;; The value should not be bigger or smaller than the minimum or maximum values set
;; when creating the slider or progress bar, otherwise the setting will default to either
;; the minimum or maximum preset value.

(define (set-value id value)
	(net-send out (string "set-value " id " " value "\n"))
)

;; @syntax (gs:set-visible <sym-id> <boolean-visible>)
;; @param <sym-id> The component which is hidden or made visible.
;; @param <boolean-visible> A flag indicating if the component is visible '"true"', '"nil"'.
;;
;; Except for frames and dialog windows, components are visible by default. Normally
;; frames and dialogs are not set visible before all other components are placed inside.

(define (set-visible id flag)
	(net-send out (string "set-visible " id " " flag "\n"))
)

;; @syntax (gs:shear-tag <sym-tag> <int-x> <int-y> [<boolean-repaint>])
;; @param <sym-tag> The tag group to shear.
;; @param <float-x> The X shearing factor.
;; @param <float-y> The Y shearing factor.
;; @param <boolean-repaint> An optional flag to indicate if repainting is required (default is 'true').

(define (shear-tag tag sx sy (repaint true))
	(send-out (string "shear-tag " gs:currentCanvas " " tag " " 
		sx " " sy " " repaint "\n"))
)

;; @syntax (gs:show-popup <sym-tag> <sym-host> <int-x> <int-y>)
;; @param <sym-tag> The id of the popup menu.
;; @param <sym-host> The host container where to pop up the menu.
;; @param <int-x> The X coordinate of the menu popup position.
;; @param <int-y> The Y coordinate of the menu popup position.

(define (gs:show-popup id host x y)
	(net-send out (string "show-popup " id " " host " " x " " y "\n"))
)

;; @syntax (gs:show-tag <sym-tag> [<boolean-repaint>])
;; @param <sym-tag> The tag of the group to show.
;; @param <boolean-repaint> An optional flag to indicate if repainting is required (default is 'true').
;;

(define (show-tag tag (repaint true))
	(net-send out (string "show-tag " gs:currentCanvas " " tag " " repaint "\n"))
)

;; @syntax (gs:slider <sym-id> <sym-action> <str-orientation> <int-min> <int-max> <int-initial-value>)
;; @param <sym-id> The name of the slider.
;; @param <sym-action> The name of the event handler.
;; @param <str-orientation> The orientation of the slider '"horizontal"' or '"vertical"'
;; @param <int-min> The minimum value of the slider.
;; @param <int-max> The maximum value of the slider.
;; @param <int-initial-value> The initial value of the slider.

(define (slider id action orient posmin posmax posinit)
	(net-send out (string "slider " id " " action " " orient " " posmin " " posmax " " posinit "\n"))
)

;; @syntax (gs:split-pane <sym-id> <str-orientation> [<float-weight> [<float-location> [<int-divider-size>]]])
;; @param <sym-id> The name of the split-pane.
;; @param <str-orientation> The orientation '"horizontal"' or '"vertical"'.
;; @param <float-weight> The optional weight distribution between '0.0' and '1.0' when re-sizing the window. The default is '0.0'.
;; @param <float-location> The optional initial divider location between '0.0' and '1.0'.
;; @param <int-divider-size> The optional size of the draggable divider in pixels.

(define (split-pane id orient (weight 0.0)  (pos 0.5)  (dvdr 5))
	(net-send out (string "split-pane " id " " orient " " weight " " pos " " dvdr "\n"))
)

;; @syntax (gs:stop-sequence)
;;
;; Stops playing tracks, as started with 'gs:play-sequence'.

(define (gs:stop-sequence)
	(net-send out (string "stop-sequence System\n"))
)

;; @syntax (gs:tabbed-pane <sym-id> <sym-action> <str-orientation> [<sym-tab> <str-title> ...])
;; @param <sym-id> The name of the tabbed pane.
;; @param <str-orientation> The position of the tabs; either '"top"' (default), '"bottom"','"left"' or '"right"'.
;; @param <sym-tab> The id symbol name of a tab
;; @param <str-title> The title of the tab.

(define (tabbed-pane id action orient)
	(let (s (string "tabbed-pane " id " " action " " orient " ")
		  t (args))
		(while t
			(write-buffer s (string (pop t) " " (base64-enc (pop t)) " "))
		) 
		(write-buffer s "\n")
		(net-send out s))
)


;; @syntax (gs:table <sym-id> <sym-action> [<str-column-header-name> ...])
;; @param <sym-id> The name of the table.
;; @param <sym-action> The handler function symbol when a cell is selected.
;; @param <str-column-header-name> The optional column header name. 
;;
;; Creates a table with <str-column-header-name> specified column and empty row.
;; For empty strings specified as column headers, the header will be left empty.
;; If all header in a table are specified as empty, the table will be created
;; without a header row. If there are no columns at all, an empty table (0 x 0) 
;; is created. 
;;
;; When a cell is selected, the function in <sym-action> gets called with the table 
;; id, row, column and cell-contents. See the file 'table-demo.lsp' for an example. 
;; Cells can be edited  by either selecting or double clicking a cell.


(define (table id action)
  (let (s (string "table " id " " action)
	  columns (if (null? (args)) '()
		      (if (list? (args 0)) (args 0) (args))))
    (dolist (col columns)
      (write-buffer s (string " " (base64-enc col))))
    ;(println "gs:table " s)
    (write-buffer s "\n")
    (net-send out s) ) )


;; @syntax (gs:table-add-column <sym-id> <str-column-header-name> ...)
;; @param <sym-id> The name of the table.
;; @param <str-column-header-name> Add column header name(s). 
;;
;; More than one <str-column-header-name> can be specified to add more
;; than one column. A column header can be set empty using and empty string <tt>""</tt>.
;; When all headers in a table are empty, the
;; table will be displayed without a header row.

(define (table-add-column id)
  (let (s (string "table-add-column " id)
	  columns (if (null? (args)) '()
		      (if (list? (args 0)) (args 0) (args))))
    (dolist (col columns)
      (write-buffer s (string " " (base64-enc col))))
    ;(println "gs:table-add-column " s)
    (write-buffer s "\n")
    (net-send out s) ) )


;; @syntax (gs:table-add-row <sym-id> [<str-columns> ... ])
;; @syntax (gs:table-add-row <sym-id> ([<str-columns> ...))
;; @param <sym-id> The name of the table.
;; @param <str-columns> Add a row with contents in <str-columns>
;;
;; Add row with each column value. If necessary a scrollbar will appear.
;; If no contents is defined in <str-columns>, or if contents for less
;; columns is defined than available, column contents is left empty.
;; Multiple column content can be specified as either a list
;; of strings or as additional parameters of 'gs:table-add-row'. 

(define (table-add-row id)
  (let (s (string "table-add-row " id)
	  columns (if (null? (args)) '()
		      (if (list? (args 0)) (args 0) (args))))
    (dolist (col columns)
      (write-buffer s (string " " (base64-enc col))))
    ;(println "gs:table-add-row " s)
    (write-buffer s "\n")
    (net-send out s) ) )


;; @syntax (gs:table-get-cell <sym-id> <int-row> <int-column>)
;; @param <sym-id> The name of the table.
;; @param <int-row> The row of the cell.
;; @param <int-column> The column of the cell.
;; @return cell value. stored in gs:table-cell.
;;
;; Get the cell contents as a string at sepcifed <int-row> and <int-column>.

(define (table-get-cell id row col)
  (set 'gs:table-cell nil)
  (let (s (string "table-get-cell " id
		  " " row
		  " " col))
    ;(println "gs:table-get-cell " s)
    (write-buffer s "\n")
    (net-send out s)
    (while (not gs:table-cell) (check-event 10000))
    gs:table-cell)
  )


;; @syntax (gs:table-get-size <sym-id>)
;; @param <sym-id> The name of the table.
;; @return table size list (row-size, column-size)
;;
;; Get table size, stored in 'gs:table-size'.
;; Note, that adding columns or row will not automatically update
;; the 'gs:table-size' variable. Use 'gs:table-get-size' to update
;; this variable.

(define (table-get-size id)
  (set 'gs:table-size nil)
  (let (s (string "table-get-size " id))
    ;(println "gs:table-get-size " s)
    (write-buffer s "\n")
    (net-send out s)
    (while (not gs:table-size) (check-event 10000))
    gs:table-size
    ) )

; FdB
;; @syntax (gs:table-remove-row <sym-id> <int-rownumber>)
;; @param <sym-id> The name of the table.
;; @param <int-row> The row to remove
;; 
;; Removes a row See also 'gs:table-set-row-count'.

	(define (table-remove-row id row)
	  (let (s (string "table-remove-row " id " " row))
	    ;(println "gs:table-remove-row " s)
	    (write-buffer s "\n")
	    (net-send out s) ) )


;; @syntax (gs:table-set-cell <sym-id> <int-row> <int-column> <str-value>)
;; @param <sym-id> The name of the table.
;; @param <int-row> The row of the cell set.
;; @param <int-column> The column of the cell set.
;; @param <str-value> The cell value.
;; @return The previous contents of the cell; also stored in 'gs:table-cell'.
;;
;; Sets a new table cell contents and returns the old cell contents. Row and
;; column numbering starts with '0' (zero). The cell contents is passed
;; as a string.

(define (table-set-cell id row col value)
  (set 'gs:table-cell nil)
  (let (s (string "table-set-cell " id
		  " " row
		  " " col
		  " " (base64-enc value)))
    ;(println "gs:table-set-cell " s)
    (write-buffer s "\n")
    (net-send out s)
    (while (not gs:table-cell) (check-event 10000))
    gs:table-cell
 ) )


;; @syntax (gs:table-set-column <sym-id> <int-column-number> <int-width> [<str-justification>])
;; @param <sym-id> The name of the table.
;; @param <int-column-number> The column number of align.
;; @param <int-width> The column width.
;; @param <str-justification> The column align property, "left", "center", "right".
;;
;; A table column property is changed, adjusting the column width and alignment of cell
;; contents. The <str-justification> parameter is optional and alignment is "left"
;; by default.

(define (table-set-column id columnNum width (justification "left"))
  (let (s (string "table-set-column " id
		  " " columnNum
		  " " width
		  " " justification))
    ;(println "gs:table-set-column " s)
    (write-buffer s "\n")
    (net-send out s) ) )

; FdB
;; @syntax (gs:table-set-column-name <sym-id> [<str-columns> ... ])
;; @syntax (gs:table-set-column-name <sym-id> ([<str-columns> ...))
;; @param <sym-id> The name of the table.
;; @param <str-columns> Set column names with contents in <str-columns>	
;; 
;; Replaces the column names in the table. If the number of names 
;; is greater than the current number of columns, new columns are added to the end 
;; of each row in the table. If the number of columnnames is less than the current
;; number of columns, all the extra columns at the end of a row are discarded.
	
	(define (table-set-column-name id)
	   (let (s (string "table-set-column-name " id)
		  columns (if (null? (args)) '()
			      (if (list? (args 0)) (args 0) (args))))
		  (dolist (col columns)
		    (write-buffer s (string " " (base64-enc col))))
		  ;(println "gs:table-set-column-name " s)
		  (write-buffer s "\n")
		  (net-send out s)))
	  
; FdB
;; @syntax (gs:table-set-row-count <sym-id> <int-row-count>)
;; @param <sym-id> The name of the table.
;; @param <int-row> Set the numbers of rows in the table with <int-row-count>	  
;;
;; Sets the number of rows in the table. If the new size is greater than the 
;; current size, new rows are added to the end of the table. If the new size is 
;; less than the current size, all rows at index rownumber and greater are discarded.
	  
	(define (table-set-row-count id num)
	  (let (s (string "table-set-row-count " id " " num))
		(write-buffer s " \n")
		(net-send out s)) )

;; @syntax (gs:table-get <sym-id>)
;; @return table cells. stored in 'gs:table-full'.
;;
;; Get full table as a list of row lists.
;; <pre>
;; ( ("column0" "column1" ... ) ; 1'st row
;;   ("column0" "column1" ... ) ; 2'nd row
;;   ...
;;   ... )
;; </pre>
;;
;; The entire table contents is stored as a list of row lists in the 
;; return value of 'gs:table-get', and is also stored in the variable 
;; 'gs:table-full'. 

(define (table-get id)
	(set 'gs:table-full nil)
	(let (s (string "table-get " id))
		(write-buffer s "\n")
		;(println "gs:table-get " s)
		(net-send out s)
		(while (not gs:table-full) (check-event 10000))
		; decode base64 cell contents for each row in each cell
		(set 'gs:table-full
			(map (lambda (x) (map (lambda (s) 
					(if (nil? s) nil (base64-dec s))
						) x ) ) gs:table-full))
	)
)

;; @syntax (gs:table-set-row-number <sym-id> <bool-row-number>) DEPRECATED
;;
;; Use 'gs:table-show-row-number'. The old naming is deprecated but will 
;; still work.


;; @syntax (gs:table-show-row-number <sym-id> <bool-row-number>)
;; @param <sym-id> The name of the table.
;; @param <bool-row-number> 'true' if rows should carry a row number; default 'nil'.
;;
;; Show or hide the row number headers. The default is hiding row numbers.

(define (table-show-row-number id boolRowheader)
  (let (s (string "table-show-row-number " id
		  " " boolRowheader))
    ;(println "gs:table-show-row-number " s)
    (write-buffer s "\n")
    (net-send out s) ) )

(define table-set-row-number table-show-row-number)

;; @syntax (gs:text-area <sym-id> <sym-action> <int-width> <int-height>)
;; @param <symid> The name of the text area.
;; @param <sym-action> The name of the event handler.
;; @param <int-width> The optional width of the text area..
;; @param <int-height> The optional height of the text area.
;;
;; @example
;; (gs:text-area 'TheText 'textarea-event 10 8)
;;
;; (define (textarea-event id code dot mark) ...)

;; 'gs:text-area' transmits the following parameters in its event:
;; <pre>
;; id   - name of the widget 
;; code - key code equals ASCII code. Only for text keys
;; dot  - position of text caret in the text 
;; mark - extended (selection) position of caret 
;; </pre>


(define (text-area id action width height)
	(if (and width height)
		(net-send out (string "text-area " id " " action " " width " " height "\n"))
		(net-send out (string "text-area " id " " action "\n")))
)

;; @syntax (gs:text-field <sym-id> <sym-action> <int-columns> [<str-cover-char>])
;; @param <sym-id> The name of the text field.
;; @param <sym-action> The name of the event handler.
;; @param <int-columns> The number of columns in the text field.
;; @param <str-cover-char> Cover character for password entry.
;; @example
;; (gs:text-field 'TheTextField 'textfield-event)
;; (gs:text-field 'PasswordTextField 'textfield-event "*")

;; The 'textfield-event' is fired when the enter key is pressed in the
;; text field. As an alternative the cover character for passwords can be
;; set with 'gs:set-echo-char'.

(define (text-field id action columns text)
  (if text
      (net-send out (string "text-field " id " " action " " columns " " (base64-enc text) "\n"))
      (net-send out (string "text-field " id " " action " " columns "\n")))
)

;; @syntax (gs:text-pane <sym-id> <sym-action> <str-style> [<int-width> <int-height>])
;; @param <sym-id> The name of the text pane.
;; @param <sym-action> The key action handler for the html pane.
;; @param <sym-style> The content type of the text pane.
;; @param <int-width> The optional width of the pane.
;; @param <int-height> The optional height of the pane.
;;
;; The 'gs:text-pane' is used similar to 'gs:text-area. The following styles
;; are supported in <sym-style>:
;; <pre>
;;    "text/plain"
;;    "text/html"
;; </pre>
;;
;; The 'gs:text-pane' widget will automatically display scroll bars when
;; text does not fit in the visible space of the pane. When entering parentheses
;; they are automatically matched with their opening or closing counterparts, if they exist.
;; If this is undesired behavior, the simpler 'gs:text-area' control should
;; be used instead. 
;;
;; On each change of the caret or selection in the text pane
;; an event is fired containing several parameters about the caret and selection
;; positions, the last character typed, and the modifier keys used. See the
;; the file 'newlisp-edit.lsp' for a complex application using all features
;; available in this widget.
;;
;; To make hyperlinks in 'HTML' formatted text clickable, editing must
;; be disabled using the 'gs:set-editable' function. The functions 'gs:set-font'
;; and 'gs:append-text' will work only on the 'text/plain' content style.
;;
;; @example
;; (gs:text-pane 'TheTextPane 'textpane-event "text/plain")
;;
;; (define (textpane-event id code mods dot mark len undo redo) ...)

;; 'gs:text-pane' transmits the following parameters in its event:
;; <pre>
;; id   - name of the widget 
;; code - key code equals ASCII code. Only for text keys 
;; mods - keys pressed together with the previous, like shift, ctrl etc.
;; dot  - position of the text caret in the text 
;; mark - extended (selection) position of the caret 
;; len  - length of the text in the textarea 
;; undo - undo enabled/disabled 
;; redo - redo enabled/disabled 
;; </pre>

(define (text-pane id action style width height)
	(if (and width height)
		(net-send out (string "text-pane " id " " action " " style " " width " " height "\n"))
		(net-send out (string "text-pane " id " " action " " style "\n"))
	)
)

;; @syntax (gs:toggle-button <sym-id> <sym-action> <str-text> [<bool-selected>])
;; @param <sym-id> The name of the toggle button.
;; @param <sym-action> The name of the event handler.
;; @param <str-text> The optional text of the toggle button.
;; @param <bool-selected> An optional flag 'true' or 'nil' (default) indicating the initial state of the toggle button.

(define (toggle-button id action text selected)
	(if text
		(net-send out (string "toggle-button " id " " action " " (base64-enc text) " " selected "\n"))
		(net-send out (string "toggle-button " id " " action "\n")))
)

;; @syntax (gs:tool-bar <sym-frame> [<bool-floatable> <int-hgap> <int-vgap>])
;; @param <sym-frame> The name of the frame hosting the toolbar.
;; @param <bool-floatable> The optional flag 'true' or 'nil' to indicate if the toolbar can be detached.
;; @param <int-hgap> The horizontal gap between components on the toolbar.
;; @param <int-vgap> The vertical gap between the components on the toolbar.

(define (tool-bar aframe floatable width height)
	(if (and width height)
		(net-send out (string "tool-bar " aframe " " floatable " " width " " height "\n"))
		(net-send out (string "tool-bar " aframe " " floatable "\n")))
)

;; @syntax (gs:translate-tag <sym-tag> <int-x> <int-y> [<boolean-repaint>])
;; @param <sym-tag> The name tag of the group to translate.
;; @param <int-x> The X-coordinate translation value.
;; @param <int-y> The Y-coordinate translation value.
;; @param <boolean-repaint> An optional flag to indicate if repainting is required (default is 'true').
;;
;; Moves the origin of the coordinate system of all objects tagged with <sym-tag>.
;; Like all tag operations multiple 'gs:translate-tag' operations are cumulative.

(define (translate-tag tag x y (repaint true))
	(send-out (string "translate-tag " gs:currentCanvas " " tag " " x " " y " " repaint "\n"))
)

;; @syntax (gs:undo-text <sym-id>)
;; @param <sym-id> The id of the 'gs:text-pane' where to perform an undo operation.

(define (undo-text id)
	(net-send out (string "undo-text " id "\n"))
)

;; @syntax (gs:undo-enable <sym-id> <boolean-enabled>)
;; @param <sym-id> The id of the 'gs:text-pane' for which to enabe/disable undo.
;; @param <boolean-enabled> 'true' or 'nil' to enable or disable undo.  

(define (undo-enable id enabled)
	(net-send out (string "undo-enable " id  " " enabled "\n"))
)


;; @syntax (gs:update)
;; 
;; Forces a repaint of the current canvas, e.g. after changing the scale or translation of a visible
;; canvas. This function is rarely used, as most screen updates are performed automatically.
;; All tag operations can carry an additional parameter to force update after they have been
;; draw.

(define (update)
	(net-send out (string "update " gs:currentCanvas "\n"))
)

;; @syntax (gs:window <sym-id> <int-x> <int-y> <int-width> <int-height>)
;; @param <sym-id> The name of the invisible window.
;; @param <int-x> The x-coordinate of the screen position.
;; @param <int-y> The y-coordinate of the screen position.
;; @param <int-width> The width of the window.
;; @param <int-height> The height of the window.
;;
;; Creates a borderless window. Note that a borderless window may treat
;; some hosted components differently from normal frames and dialogs.

(define (window id x y width height)
	(net-send out (string "window " id " " x " " y " " width " " height "\n"))
)

;; @syntax (gs:window-closed <sym-id> <sym-action>)
;; @param <sym-id> The name of the frame or dialog.
;; @param <sym-action> The action to perform when the frame or dialog closes.
;;
;; A window or dialog window can be closed using the system close button in
;; one of the corners of the window. In this case it is useful to specify
;; a handler function which is called upon closing.

(define (frame-closed id action)
	(net-send out (string "frame-closed " id " " action "\n"))
)

(define (window-closed id action)
	(net-send out (string "frame-closed " id " " action "\n"))
)


;; @syntax (gs:window-moved <sym-id> <sym-action>)
;; @param <sym-id> The name of the frame or dialog.
;; @param <sym-action> The action to perform when the frame or dialog moves.
;;
;; The event will carry the <sym-id> of the window or dialog and current <tt>X</tt> and <tt>Y</tt>
;; coordinates on the screen.
;;

(define (frame-moved id action)
	(net-send out (string "frame-moved " id " " action "\n"))
)

(define (window-moved id action)
	(net-send out (string "frame-moved " id " " action "\n"))
)


;; @syntax (gs:window-resized <sym-id> <sym-action>)
;; @param <sym-id> The name of the frame or dialog.
;; @param <sym-action> The action to perform when the frame or dialog is resized.
;;
;; The event will carry the <sym-id> of the window or dialog and current width and
;; height.

(define (frame-resized id action)
	(net-send out (string "frame-resized " id " " action "\n"))
)

(define (window-resized id action)
	(net-send out (string "frame-resized " id " " action "\n"))
)


; eof
