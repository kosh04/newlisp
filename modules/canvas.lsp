;; @module canvas.lsp
;; @description Routines for creating HTML canvas tag graphics
;; @version 1.0 initial release
;; @version 1.1 doc changes only
;; @version 1.21 doc changes only
;; @version 1.3 compatibility pre and post 10.2.0 for new extend
;; @version 1.32 doc formatting, spelling
;; @version 1.33 took out license
;; @version 1.4 cv:petal was broken (JavaScript change?)
;; @version 1.44 doc corrections
;; @version 1.45 cv:render works on Emscripten newLISP
;; @version 1.52 changed Win32 to Windows and spelling
;; @version 1.6 sign error in shape caused incompatibility with postscript.lsp
;; @version 1.61 eliminated canvas-15.tgz link
;; @version 1.7 fixed ellipse, petal, pie, polygon, shape fill first the stroke
;; @version 1.71 fixed documentation for cv:clip
;; @author Lutz Mueller, March 2009, June 2012, January 2014, 2015
;; <h2>Turtle graphics for the HTML-5 canvas tag</h2>
;; This module generates HTML pages suited for browsers which recognize
;; the HTML-5 canvas tag. 

;; <h3>Example program</h3>
;; The following is a simple example how to create an HTML page
;; with one or more embedded graphics. 
;;
;; @example
;; (module "canvas.lsp") ; does a load from standard location
;;
;; (cv:html "&lt;h2&gt;First canvas&lt;/h2&gt;") ; optional mixed in HTML
;; (cv:canvas "FirstCanvas" 300 100) ; required canvas creation
;; (cv:fill-color 1.0 0.0 0.0)
;; (cv:rectangle 100 80 true) ; one or more drawing statements
;; (cv:html "&lt;h2&gt;Second canvas&lt;/h2&gt;")
;;
;; (cv:canvas "SecondCanvas" 300 100)
;; (cv:fill-color 0.0 1.0 0.0)
;; (cv:rectangle 200 80 true)
;; (cv:render "page.html") ; required rendering of the page
;; (exit)

;; All canvas functions are prefixed with 'cv' and emit the JavaScript
;; code necessary to the &lt;script&gt;&lt/script&gt; section in the
;; header section of the HTML page. Only the function 'html' is not
;; prefixed and outputs strings directoy to the &lt;body&gt;&lt/body&gt;
;; tagged section of the HTML page.
;;
;; @example
;; (html "&lt;p&gt;This is a parapgraph of text.&lt;/p&gt;")

;; Sometimes it is necessary to output JavaScript explicity to the
;; script section of the page, e.g. to use canvas features not implemented
;; in this module. This can be done via the 'cv' default functor:
;;
;; @example
;; (cv "document.write('this is a test');")

;; Most functions in the canvas 'cv' module work the same as in the older
;; postscript 'ps' modul, and some functions in the postscript module
;; have been changed to achieve more compatibility between both modules. 
;; The following example shows how name-space prefix switching can be used to
;; to run a <tt>postscript.lsp</tt> based program with <tt>canvas</tt>:
;;
;; @example
;; (module "canvas.lsp")
;; 
;; (html "<center>")
;; (cv:canvas "TheCanvas" 200 200)
;; ; reuse postrcipt.lsp code
;; (set 'ps cv) ; switch prefix
;;
;; (ps:goto 120 132)
;; (ps:line-color  1 0 0.6)
;; (ps:line-width 3.0)
;; (ps:angle 15)
;; (dotimes (i 12)
;;     (cv:turn 30)
;;     (cv:bezier -20 75 40 75 20 0))
;; ; end postscript snippet
;;
;; (html "</center>")
;; (cv:render "rose.html")
;; (exit)

;; A package with more demo files can be downloaded from 
;; @link http://www.newlisp.org/canvas/canvas-15.tgz www.newlisp.org/canvas/canvas-15.tgz .
;; The demo files assume the Safari (4.0 beta) browser on Mac OS X and Windows or
;; The Firefox browser (3.1. beta 3) on Linux and start the browser automatically.
;; Change the last 'cv:render' line in each of the demo file to manually load
;; the generated page-file spciefied in the 'cv:render' statement.
 
;; <h2>Using JavaScript directly</h2>
;; Complex graphics with many looping statements can generate huge HTML files which
;; may be slow to load over an internet connection. The newLISP program itself
;; may be small, but the JavaScript it generates may fill hundreds of kilobytes
;; because of repeated output of JavaScript statements. The <tt>cv</tt> function 
;; can be used to to emit JavaScript directly. For most <tt>cv:xxx</tt> (but not all) 
;; -functions a similar named JavaScript function exists in <tt>canvas.lsp</tt>.
;; The following example generates the same graphic twice, first as a looping
;; newLISP script, then as an explicitly written JavaScript script:
;;
;; @example
;; (module "canvas.lsp")
;; 
;; (html "<h3>Indirect draw</h3>")
;; 
;; (cv:canvas "CanvasOne" 400 200)
;; (cv:line-color 0 0.5 0.5)
;; 
;; (cv:angle -90)
;; (dotimes (i 180)
;;     (cv:goto 200 0)
;;     (cv:draw 300)
;;     (cv:turn 1))
;; 
;; (html "<h3>Direct draw</h3>")
;; 
;; (cv:canvas "CanvasTwo" 400 200)
;; (cv:line-color 0 0.5 0.5)
;; 
;; (cv [text]
;; Angle(-90);
;; for(var i = 0; i < 180; i++) {
;;     Goto(200, 0);
;;     Draw(300);
;;     Turn(1);
;;     }
;; [/text])
;;
;; (cv:render) ; render page automatically in browser on OS X (Safari 4.0)
;; ; as an alternative specify the HTML filename
;; (cv:render "direct.html") ; renders to file specified
;; (exit)

;; <h2>Differences to the postscript module</h2>
;; Differences between the <tt>canvas.lsp</tt> and <tt>postscript.lsp</tt> modules
;; are mainly in the treatment of text colors and fonts.
;; <ul>
;; <li>The text color for 'cv:text' is set by 'cv:fill-color' with 'canvas.lsp', but
;; with 'ps:line-color' when using 'postscript.lsp' and 'ps:text', except for outlined
;; text, which is drawn with 'line-color' in both modules.</li>
;; <li>The 'cv:font' parameters are different from the 'ps:font'
;; parameters. See the function reference for details.</li>
;; <li>Canvas graphics need the 'cv:canvas' statement at the beginning
;; to specify the canvas size. In Postscript the drawing area depends
;; on the printer driver settings. To port 'postscript.lsp' graphics
;; to 'canvas.lsp' use a width of 612 and height of 792 in the 'cv:canvas'
;; statement. The numbers correspond to a 8 1/2 by 11 inch US paper page
;; with translation of points to pixels.</li>
;; <li>Cipping with outlined text is not available yet in 'canvas.lsp'.
;; Current implementations are browser-specific and not part of the HTML-5
;; canvas tag specification.</li>
;; <li>Text drawn with 'cv:textoutline' or 'cv:textarcoutline' cannot be filled
;; with color as possible with 'ps:textoutline' and 'ps:textarcoutline' in the postscript
;; module.
;; <li>'canvas.lsp' has a 'cv:header' function to specify tags in the 
;; HTML header section.</li>
;; </ul>
 

;; <h2>Summary of functions</h2>
;; Return values from functions are not used when programming with canvas functions
;; and are not mentioned in the documentation.
;; <h3>Turtle coordinate positioning and turning</h3>
;; <pre>
;; cv:goto  - move turtle to position x, y
;; cv:move  - move turtle a distance s forward from the current position
;; cv:turn  - turn the turtle degress dg left (negative) or right (positive)
;; cv:angle - set the turtle orientation to dg degrees
;; </pre>
;; <h3>Line drawing</h3>
;; <pre>
;; cv:draw   - draw distance s forward from current position
;; cv:drawto - draw to the absolute position x,y from the current position
;; cv:line   - draw a multipart line consisting of line and bezier curve segments
;; cv:bezier - draw a Bezier curve 
;; </pre>
;; <h3>Closed shapes, filling and clipping</h3>
;; <pre>
;; cv:rectangle - draw a rectangle
;; cv:polygon   - draw a polygon with radius rad and n number of sides
;; cv:circle    - draw a circle
;; cv:ellipse   - draw an open or closed ellipse with x-rad and y-rad radius
;; cv:pie       - draw a pie piece with radius rad and width
;; cv:petal     - draw a petal shape from Bezier curves
;; cv:shape     - draw a shape defined by a list of line and bezier segments
;; cv:clip      - define a clipping path using line and Bezier segments
;; </pre>
;; <h3>Text output and clipping</h3>
;; <pre>
;; cv:text           - draw a solid text string
;; cv:textoutline    - draw text in outline shape
;; cv:textarc        - draw text around an arc
;; cv:textarcoutline - draw text in outline shape around an arc
;; </pre>
;; <h3>Global settings</h3>
;; <pre>
;; cv:translate  - move coordinate origin
;; cv:scale      - scale output
;; cv:rotate     - rotate output
;; cv:gsave      - save current graphics state (translation, scale, rotation)
;; cv:grestore   - restore current graphics state
;; cv:font       - set font specifications
;; cv:line-witdh - set line width in pixels
;; cv:line-cap   - set line termination shape
;; cv:line-join  - set line join mode
;; cv:line-color - set line color
;; cv:fill-color - set fill color
;; </pre>
;; <h3>Rendering and output</h3>
;; <pre>
;; cv:render     - render HTML output to CGI or a file
;; </pre>


;; @syntax (cv:angle <num-dg>) 
;; @param <num-dg> Angle degrees from 0 to 360.
;; <br>
;; Set the turtle angle to <num-dg> degrees.
;; Upwards is 0, right 90, downwards 180 and left 270 degrees.
;; The turtle position is saved on the graphics state stack when using 
;; '(cv:gsave)'.

;; @syntax (cv:bezier <num-x1> <num-y1> <num-x2> <num-y2> <num-x3> <num-y3>) 
;; @param  <num-x1,num-y1> Bezier coordinates of <p1> relative to <p0> = 0,0
;; @param  <num-x2,num-y2> Bezier coordinates of <p2> relative to <p0> = 0,0
;; @param  <num-x3,num-y3> Bezier coordinates of <p3> relative to <p0> = 0,0
;; <br>
;; Draw a Bezier curve.
;; The Bezier curve starts at point <p0> which is the current
;; turtle position and stops at point <p3> which is at offset
;; <num-x3> and <num-y3> relative to starting point. The turtle orientation
;; after the drawing the Bezier curve is perpendicular
;; to the Bezier curve baseline <p0> to <p3> and the position is <p3>.

;; @syntax (cv:circle <num-rad> [<bool-fill>])
;; @param <num-rad> Radius of the circle.
;; @param <bool-fill> Optional fill flag.
;; <br>
;; Draw a circle with radius <num-rad>. The optional <num-fill> flag 
;; with either 'true' or 'nil' (default) indicates if the circle
;; is filled with the fill color specified by 'cv:fill-color'.
;; The circle is drawn around the current turtle position.
;; The turtle position or orientation is not changed.

;; @syntax (cv:clip <list-of-lists>) 
;; @param <list-of-lists> A list of turtle movements and/or Bezier curves.
;; <br>
;; Define a clipping path using turtle movements (<degree> <distance>) and
;; Bezier curves (<x1> <y1> <x2> <y2> <x3> <y3>) starting from the 
;; last turtle coordinates <x0>, <y0> and finishing at <x3>, <y3>. 
;; All Bezier coordinates are relative to the previous turtle position and
;; orientation.
;;
;; Before redefining the clipping area '(cv:gsave)' should
;; be used to save the old graphics state parameters, after
;; clipping and drawing in the clipped area the graphics
;; state should be restored using '(cv:grestore)'.
;; The turtle position or orientation is not changed.

;; @syntax (cv:draw <num-s>) 
;; @param <num-s> Distance to draw.
;; <br>
;; Draw going forward distance <num-s>. Moves the turtle forward by 
;; the amount of pixels specified in <num-s> and draws with the current 
;; line color set by 'cv:line-color'.
;;

;; @syntax (cv:drawto <x> <y>) 
;; @param <x> The x coordinate to draw to.
;; @param <y> The y coordinate to draw to.
;; <br>
;; Draw a line to point <x>, <y>. Moves the turtle to point 
;; <x>, <y> like '(cv:goto x y)', but also draws a line from 
;; the old to the new position. The turtle position is changed to the
;; new point <x>, <y> and the orientation is changed to the orientaion of 
;; the line drawn.

;; @syntax (cv:ellipse <num-x-rad> <num-y-rad> <num-start> <num-end> [<bool-fill>]) 
;; @param <num-x-rad> The x axis radius.
;; @param <num-y-rad> The y axis radius.
;; @param <num-start> The start angle in 0 to 360 degrees.
;; @param <end-num> The end angle in 0 to 360 degrees.
;; <br>
;; Draw an ellipse with optional <bool-fill> either 'true' or 'nil' (default).
;; The ellipse is drawn around the current turtle position
;; with the Y axis oriented like the turtle.
;; For <num-start>, <num-end> set to 0, 360 an ellipse is drawn.
;; For a partial radius the opening is closed by a line
;; resulting in segment shape, i.e. -90, 90  would result
;; in a half circle from the left to the right of the turtle.
;; When <num-x-rad> and <num-y-rad> are of equal size a full circle
;; can be drawn. The turtle position or orientation is not changed.

;; @syntax (cv:fill-color <float-R> <float-G> <float-B>) 
;; @param <float-R> The red color value.
;; @param <float-G> The green color value.
;; @param <float-B> The blue color value.
;;
;; Set color for shape filling.
;; Color values assume the following value:
;;
;; <pre>
;;    R - value for red 0.0 to 1.0
;;    B - value for green 0.0 to 1.0
;;    G - value for blue 0.0 to 1.0
;; </pre>
;;
;; @syntax (cv:fill-color <str-hex>)
;; @param <str-hex> A hex string specifying the line color.
;; <br>
;; In an alternative syntax color values can be specified in a
;; hex string:
;;
;; <str-hex> is a hex string constant '"000000"' to '"FFFFFF"'
;; Colors are specified as usual in HTML coding.
;; Each two hex digits define a color: 'rrggbb'.  

;; @syntax (cv:font <str-name>) 
;; @param <str-name> The font name.
;; <br>
;; The current font is set for all subsequent text operations.
;; Depending on the browser and OS installed, different fonts are available.
;;
;; @example
;; (cv:font "normal 14px sans-serif") ; Helvetica
;; (cv:font "bold 20px serif") ; Times
;; (cv:font "italic 32px sans-serif") ; Cursive

;; @syntax (cv:goto <num-x> <num-y>)
;; @param <num-x> The new x coordinate.
;; @param <num-y> The new y coordinate.
;; <br>
;; Moves to position <num-x>, <num-y>. 
;; The turtle position can be
;; saved on the graphics state stack when using '(cv:gsave)'.

;; @syntax (cv:grestore)
;; Restores the graphics state from the stack.

;; @syntax (cv:gsave)
;; Saves the current graphics state. The function pushes the 
;; current graphics state on a special stack, from where it
;; can be resored using '(cv:grestore)'. States saved are:
;; The turtle position X, Y and  orientation, the transformation
;; scaling and rotation factors, the line cap and join value and
;; the colors set.

;; @syntax (cv:line <list-of-lists>) 
;; @param <list-of-lists> A list of turtle movements or Bezier curves.
;; <br>
;; Draw a multipart line.  <lists> are turtle movements (<num-dg> <num-s>),
;; or Bezier curves (<x1> <y1> <x2> <y2> <x3> <y3>) starting
;; from the last turtle coordinates <x0>, <y0> and
;; finishing at <x3>, <y3>. All Bezier coordinates are
;; relative to the previous turtle position and
;; orientation.
;;
;; The turtle position and orientation are changed after
;; drawing the line.

;; @syntax (cv:line-cap <num-mode | str-mode>) 
;; @param <mode> The line termination shape mode as a string or number
;; <br>
;; Sets the line termination shape as either a number or string:
;; <pre>
;;    0 or "butt"
;;    1 or "round"
;;    2 or "square"
;; </pre>

;; @syntax (cv:line-color <float-R> <float-G> <float-B>) 
;; @param <float-R> The red color value.
;; @param <float-G> The green color value.
;; @param <float-B> The blue color value.
;; <br>
;; Set color for line drawing.
;; Color values assume the following value:
;; <pre>
;;    R - value for red 0.0 to 1.0
;;    G - value for green 0.0 to 1.0
;;    B - value for blue 0.0 to 1.0
;; </pre>
;;
;; @syntax (cv:line-color <str-hex>)
;; @param <str-hex> A hex string specifying the line color.
;; <br>
;; In an alternative syntax color values can be specified in a
;; hex string:
;;
;; <str-hex> is a hex string constant '"000000"' to '"FFFFFF"'
;; Colors are specified as usual in HTML coding.
;; Each to two hex digits define a color: 'rrggbb'.  

;; @syntax (cv:line-join <num-mode> | <str-mode>)
;; @param <mode> The line join mode.
;; <br>
;; Sets the line join mode as either a number or string:
;; <pre>
;;    0 or "miter"
;;    1 or "round"
;;    2 or "bevel"
;; </pre>

;; @syntax (cv:line-width <int-pixels>)
;; @param <int-pixels> The line width in pixels.
;; <br>
;; Sets the line width in pixels for line drawing and the
;; outlines drawn by shapes and text outlines.

;; @syntax (cv:move <num-s>) 
;; @param <num-s> The distance to move the pen.
;; <br>
;; Move turtle the forward distance <s> without drawing.

;; @syntax (cv:petal <num-width> <num-height> [<bool-fill>]) 
;; @param <num-width> The 'x1' coordinate of the  underlying Bezier curve <p0> to <p1> <p2> <p3>.
;; @param <num-height> The 'y1' coordinate of the  underlying Bezier curve <p0> to <p1> <p2> <p3>.
;; @param <bool-fill> An optional fill flag for color fill.
;; <br>
;; Draws a petal using a Bezier curve with optional <num-fill> either 'true' or 'nil' (default).
;; The <num-height> and <num-width> parameters are relative to to the current position.
;; The petal is drawn with the tip at the current turtle
;; position and oriented in the direction of the turtle.
;; The turtle position or orientation is not changed.

;; @syntax (cv:pie <num-rad> <num-width> [<bool-fill>])
;; @param <num-rad> The radius of the pie.
;; @param <num-width> The width of the pie slice as an angle.
;; @param <bool-fill> An optional fill flag for color fill, 'true' or 'nil' (default).
;; <br>
;; Draw a pie slice with optional <bool-fill> either 'true' or 'nil' (default).
;; The left edge of the pie is in turtle orientation.
;; The width angle spreads clockwise. The pie is drawn around the current
;; turtle position. The turtle position or orientation is not changed.

;; @syntax (cv:polygon <num-rad> <num-n> [<bool-fill>])
;; @param <num-rad> Radius.
;; @param <num-n> Number of sides.
;; @param <fill> Optional fill flag.
;; <br>
;; Draw a polygon with radius <num-rad> and <num-n> sides.
;; <num-fill> is 'true' or 'nil' (default) for optional color fill
;; The polygon is drawn around the current turtle position.
;; The turtle position or orientation is not changed.

;; @syntax (cv:rectangle <num-width> <num-height> [<bool-fill>])
;; @param <num-width> The width of the rectangle.
;; @param <num-height> The height of the rectangle.
;; @param <bool-fill> An optional flag to draw a filled rectangle.
;; <br>
;; A rectangle is drawn at the current turtle position.
;; The width of the rectangle will be perpendicular to
;; the turtle orientation. If the turtle never turned or
;; the turtle angle never was set then the width of the
;; rectangle will lie horizontally.
;;
;; The position or orientation of the turtle will not change.

;; @syntax (cv:render [<HTML-file-name>])
;; @param  <HTML-file-name> Optionam HTML file-name to save to.
;; <br>
;; On Mac OX X system when using the function without a file-name,
;; the default HTML browser is opened automatically and a temporary
;; file <tt>/tmp/noname.html</tt> is shown. This is the best mode for
;; interactive development.
;;
;; On Windows 'cv:render' tries to open 'c:\Program Files\Safari\Safari.exe'.
;; The function 'cv:render' at the end of the source in 'canvas.lsp'
;; can be modified for a different browser.
;;
;; When a file-name is supplied, then 'cv:render' generates a HTML
;; file. When the file-name is specified as <tt>"cgi"</tt>, then
;; output is directed to standard out. This is useful for writing CGI
;; programs. The CGI program must take care to emit a content-type header 
;; first using:
;;
;; <pre>(print "Content-Type: text/html\r\n\r\n")</pre>

;; @syntax (cv:rotate <num-deg>) 
;; @param <num-deg> The degrees of rotation: -360 to 0 to 360.
;; <br>
;; Rotate the coordinate space. 
;; The coordinate space is rotated to the right for
;; positive angles and to the left for negative angles.
;; The current rotation angle is 0 (upwards) by default.
;; The rotation angle is part of the graphics state saved by
;; the 'cv:gsave' function and restored by 'cv:grestore'.

;; @syntax (cv:scale <num-x> <num-y>) 
;; @param <num-x> The new x scale factor.
;; @param <num-y> The new y scale factor.
;; <br>
;; Scale the coordinate space.
;; Scaling factors are 1.0 by default and compress for
;; factors less 1.0 or expand for factors bigger than 1.0.
;; With a scaling factor for x = 2.0 each point position
;; specified would cover the double of horizontal distance
;; on the page. Previou scaling factors can be saved on the graphics
;; state stack using the function 'cv:gsave' and restored using 'cv:grestore'.

;; @syntax (cv:shape <list-of-num-lists> [<bool-fill>])
;; @param <list-of-num-lists> A list of turtle movements and/or Bezier curves.
;; @param <bool-fill> An optional fill flag for color fill.
;; <br>
;; Draws a shape with optional <bool-fill> or eiher 'true' or 'nil' (default).
;; <num-lists> is either a turtle movement  (<degree> <distance>) or a Bezier curve 
;; (<x1> <y1> <x2> <y2> <x3> <y3>) starting from the last turtle coordinates 
;; <x0>, <y0> and finishing at <x3>, <y3>. All Bezier coordinates
;; are relative to the previous turtle position and orientation
;; The turtle position or orientation is not changed.

;; @syntax (cv:text <str-text>)
;; @param <str-text> The text to draw.
;; <br>
;; Draws text. Before drawing, a font can be specified, the default font after loading 
;; the 'canvas.lsp' modules is the default font of the canvas tag. The text color
;; is the current 'cv:fill-color'.
;;
;; The turtle position is changed to the baseline after the last character.
;; The turtle orientation stays the same.

;; @syntax (cv:textarc <str-text> <num-rad>)
;; @param <str-text> The text to draw. 
;; @param <num-rad> The radius of an imaginary circle path for <str-text>.
;; <br>
;; Draw text around a circle.
;; The text is drawn out side of an imaginary circle starting at 
;; turtle position and orientation and drawing at the current tangent.
;; For a positive radius text goes outside
;; the circle and clockwise. For a negative radius text goes inside the
;; circle and counter lock wise. The turtle position and orientation
;; move along the radius.

;; @syntax (cv:textarcoutline <str-text> <num-rad>) 
;; @param <str-text> The text to draw.
;; @param <num-rad> The radius of imaginary circle path for text.
;; <br>
;; Draw text around a circle.
;; Same as 'cv:textarc' but the text is drawn as ane outline.
;; The color of the text outline is the current 'cv:line-color'.
;; The turtle position and orientation move along the radius.

;; @syntax (cv:textoutline <str-text>)
;; @param <str-text> The text to draw. 
;; <br>
;; Draw a text outline
;; Before drawing a font can be specified
;; the default font after loading 'canvas.lsp' is the font
;; of the HTML canvas tag.
;;
;; The turtle position is changed to the baseline after the last character.
;; The turtle orientation stays the same.

;; @syntax (cv:translate <num-dx> <num-dy>) 
;; @syntax (cv:translate) 
;; @param <num-dx> Move the current x-origin  by 'dx'.
;; @param <num-y> Move the current y-origin by 'dy'.
;; <br>
;; Move the coordinate origin.
;; By default the origin 0,0 is in the bottom left corner
;; of the page. The <num-dx> and <num-dy> values extend to the right and top. 
;; When no <num-x>, <num-y> values are specified, the coordinate origin
;; is moved to the current position of the turtle. Previous translation
;; offsets can be saved on the graphics state stack using the
;; function 'cv:gsave' and restored using 'cv:grestore'.

;; @syntax (cv:turn <num-dg>)
;; @param <num-dg>  The degrees to turn: -360 to 0 to 360.
;; <br>
;; Turn the turtle pen by <num-dg> degrees. The degrees are specified in angles
;; from -380 to 0 to 360. For turning clockwise specifiy positive values.
;; Negative degrees turn the turtle pen counter clockwise. The turtle
;; position is aved on the graphics state stack when using '(cv:gsave)'.

(when (< (sys-info -2) 10110)
	(constant (global 'extend) write-buffer))

(define (html:html str) (extend cv:body-html str))

(context 'cv)

; global values and constants

(set 'pi (mul 2 (acos 0)))
(set 'pi/2 (acos 0))

(set 'line-feed (if (> (& 0xF (sys-info -1)) 5) "\r\n" "\n"))
(set 'header-tags "") ; header tags from cv:header go here
(set 'canvas-script "") ; graphics statements go here
(set 'body-html "") ; body html written with cv:html goes here

(set 'canvas-width 1000) ; current canvas
(set 'canvas-height 1000) ; current canvas

(set 'turtle-x 0)
(set 'turtle-y canvas-height)
(set 'turtle-orient (add pi 0.0000001))

(set 'line-color "#000000") ; for strokeStyle()
(set 'line-width 1) ; 
(set 'fill-color '(0xff 0 0 0)) ; for fillStyle()

(set 'template-header [text]<!-- newLISP generated page -->
<html>
<head>
<!-- header -->
%s[/text])

(set 'script-header [text]<script type="text/javascript">

var canvasWidth, canvasHeight;
var xpos, ypos, orient;
var turtleStack = new Array();
var xsave, ysave, orientsave;
var ctx;

function Ctx(c) { ctx = c; }

function Gsave() { 
	PushTurtle(); ctx.save(); 
	}

function Grestore() { 
	ctx.restore(); PopTurtle(); 
	}

function PushTurtle() {
	turtleStack.push(canvasHeight, xpos, ypos, orient);
	}

function PopTurtle() {
	orient = turtleStack.pop();
	ypos = turtleStack.pop();
	xpos = turtleStack.pop();
	canvasHeight = turtleStack.pop();
	}

function Scale(x, y) {
	ctx.scale(x, y);
	canvasHeight = canvasHeight / y;
	}
	
function Turn(dg) {
	orient = orient - Math.PI * dg / 180;
	}

function Goto(x, y) { xpos = x; ypos = canvasHeight - y; }

function Angle(dg) {
	orient = Math.PI - Math.PI * dg / 180;
	}

function Move(s) {
	xpos = xpos + s *  Math.sin(orient);
	ypos = ypos + s *  Math.cos(orient);
	}

function Draw(s) {
	ctx.beginPath();
	ctx.moveTo(xpos, ypos);
	xpos = xpos + s *  Math.sin(orient);
	ypos = ypos + s *  Math.cos(orient);
	ctx.lineTo(xpos, ypos);
	ctx.stroke();
	}

function Drawto(x, y) {
	ctx.beginPath(); ctx.moveTo(xpos, ypos);
	y = canvasHeight - y;
	ctx.lineTo(x, y); ctx.stroke();
	orient = Math.PI / 2 - Math.atan2(y - ypos, x - xpos); 
	xpos = x; ypos = y;
	}

function Line(lst) {
	Path(lst); ctx.stroke();
	}
	
function Shape(lst, flag) {
	turtleStack.push(xpos, ypos, orient);
	Path(lst);
	ctx.closePath(); 
	if(flag == true) { ctx.fill(); }
    ctx.stroke();
	orient = turtleStack.pop();
	ypos = turtleStack.pop();
	xpos = turtleStack.pop();
	}

function Clip(lst, flag) {
	turtleStack.push(xpos, ypos, orient);
	Path(lst); ctx.closePath();
	if(flag == true) ctx.stroke(); 
	ctx.clip();
	orient = turtleStack.pop();
	ypos = turtleStack.pop();
	xpos = turtleStack.pop();
	}

function Path(lst) {
	var x1, y1, x2, y2, x3, y3;
	var type, dg, dist, phi;
	
	ctx.beginPath(); ctx.moveTo(xpos, ypos);

	for (var i = 0; i < lst.length; ) {
		if(lst[i] == "B")
			{
			x1 = lst[i + 1]; y1 = -lst[i + 2]; 
			x2 = lst[i + 3]; y2 = -lst[i + 4]; 
			x3 = lst[i + 5]; y3 = -lst[i + 6]; 
			PathBezier(x1, y1, x2, y2, x3, y3);
			phi = Math.atan2(y3, x3);
			orient = orient - 0.5 * Math.PI - phi; // turn 90 right + phi
			if(phi == 0) Move(x3); else Move(y3 / Math.sin(phi)); 
			orient = orient + 0.5 * Math.PI; // turn 90 left
			i += 7;
			}
		else {
			if(lst[i] == "L") {
				dg = lst[i + 1];
				dist = lst[i + 2];
				orient = orient - Math.PI * dg / 180;
				xpos = xpos + dist *  Math.sin(orient);
				ypos = ypos + dist *  Math.cos(orient);
				ctx.lineTo(xpos, ypos);
				i += 3;
				}
			// else invalid format
			}
		}
	}
	
function Bezier(x1, y1, x2, y2, x3, y3) {
	var phi = Math.atan2(y3, x3);
	ctx.beginPath(); 
	PathBezier(x1, -y1, x2, -y2, x3, -y3); 
	orient += phi;
	orient = orient - Math.PI * 0.5; // turn 90
	if (phi == 0) { Move(x3); } else { Move(y3 / Math.sin(phi));}
	orient = orient + Math.PI * 0.5; // turn -90
	}
		
function PathBezier(x1, y1, x2, y2, x3, y3) {
	ctx.save(); 
	ctx.translate(xpos, ypos); ctx.rotate(Math.PI - orient);
	ctx.moveTo(0, 0); ctx.bezierCurveTo(x1, y1, x2, y2, x3, y3);
	ctx.restore();
	}

function Polygon(rad, n, flag) {
	var orientinc = 2 * Math.PI / n;
	ctx.save();
	ctx.translate(xpos, ypos); ctx.rotate(-orient);
	ctx.beginPath(); ctx.moveTo(0, rad);
	for (var angle = 0; angle < 2 * Math.PI; angle += orientinc) {
		ctx.lineTo(Math.sin(angle) * rad, Math.cos(angle) * rad);
		}
	ctx.closePath(); 
	if (flag == true) ctx.fill();
    ctx.stroke();
	ctx.restore();
	}

function Pie(rad, height, flag) {
	var start = Math.PI / 2 - orient;
	var end = start + (Math.PI / 2) * (height / 90);
	ctx.beginPath(); ctx.moveTo(xpos, ypos);
	ctx.arc(xpos, ypos, rad, start, end, false); 
	ctx.closePath();
	if (flag == true) ctx.fill();
    ctx.stroke();
	}

function Circle(rad, flag) {
	ctx.beginPath(); ctx.moveTo(xpos + rad, ypos);
	ctx.arc(xpos, ypos, rad, 0, 6.39, 0);
	if (flag == true) ctx.fill();
	ctx.stroke();
	}

function Ellipse(xrad, yrad, start, end, flag) {
	ctx.save();
	ctx.translate(xpos, ypos);
	ctx.rotate(Math.PI - orient);
	ctx.beginPath();
	ctx.scale(1, yrad/xrad);
	start = Math.PI * start / 180 - Math.PI/2;
	end =  Math.PI * end / 180 - Math.PI/2;
	ctx.arc(0, 0, xrad, start, end, 0);
	ctx.closePath();
	if (flag == true) ctx.fill();
    ctx.stroke();
	ctx.restore();
	}   

function Text(str) {
	ctx.save(); ctx.translate(xpos, ypos); ctx.rotate(Math.PI / 2 - orient);
	ctx.fillText(str, 0, 0);
	var txt = ctx.measureText(str);	 
	ctx.restore();
	Move(txt.width);
	}

function TextOutline(str) {
	ctx.save(); ctx.translate(xpos, ypos); ctx.rotate(Math.PI / 2 - orient);
	ctx.strokeText(str, 0, 0);
	var txt = ctx.measureText(str);	
	ctx.restore();
	Move(txt.width);
	}

function TextArc(str, radius) {
	var ainc;
	for(var i = 0; i < str.length; i++) {
		ainc =  ctx.measureText(str[i]).width / (2 * radius);
		orient -= ainc; Text(str[i]); orient -= ainc;
		}
	}

function TextArcOutline(str, radius) {
	var ainc;
	for(var i = 0; i < str.length; i++) {
		ainc =  ctx.measureText(str[i]).width / (2 * radius);
		orient -= ainc; TextOutline(str[i]); orient -= ainc;
		}
	}

function drawAllCanvas() { try
	{
	// if(!ctx.fillText) document.write('function fillText() not suppported on this browser');
[/text])

(set 'script-template [text]
	var canvas=document.getElementById('%s');
	var ctx=canvas.getContext('2d'); Ctx(ctx);
	canvasWidth = %g; canvasHeight = %g;
	xpos = 0; ypos = canvasHeight;
	orient = Math.PI;
	turtleStack = new Array();
	ctx.lineWidth = 1;
	ctx.strokeStyle = 'rgb(0, 0, 0)';
	ctx.fillStyle = 'rgb(0, 0, 0)';

<!-- start generated JavaScript -->
[/text])

(define (cv:cv str) (write-line canvas-script str))

(set 'script-close [text]
<!-- end generated JavaScript -->

	} catch (er) {    }
} 
</script></head><body onload="drawAllCanvas();">
[/text])

; same definition as html:html
(define (cv:html str) (write-line body-html str))

(set 'canvas-template [text]<canvas id="%s" width="%d" height="%d"></canvas>[/text])

(set 'body-close "</body></html>\n")

; user functions

(define (cv:header tags )
	(set 'header-tags tags)
)

(define (cv:canvas canvas-name (width 300)  (height 200))
	(cv (format script-template canvas-name width height))
	(html (format canvas-template canvas-name width height))
)

(define (cv:goto x y)
	(cv (format "Goto(%g, %g);" x y))
)


(define (cv:move s)
	(cv (format "Move(%g);" s))
)


(define (cv:turn dg)
	(cv (format "Turn(%g);" dg))
)


(define (cv:angle dg)
	(cv (format "Angle(%g);" dg))
)


(define (cv:draw s)
	(cv (format "Draw(%g);" s))
)


(define (cv:drawto x y)
	(cv (format "Drawto(%g, %g);" x y))
)

	
(define (cv:line lst)
	(cv (format "Line(new Array(%s));" (list-path lst)))
)


(define (list-path lst)
	(let ( (buff "") (rec nil))
		(while (set 'rec (pop lst))
			(if (= (length rec) 6)
				(extend buff (string ",'B'," (join (map string rec) ","))))
			(if (= (length rec) 2)
				(extend buff (string ",'L'," (rec 0) "," (rec 1)))))
		(1 buff)
) )
	

(define (cv:bezier x1 y1 x2 y2 x3 y3)
	(cv (format "Bezier(%g, %g, %g, %g, %g, %g); ctx.stroke();"
			 x1 y1 x2 y2 x3 y3))
)


(define (cv:polygon rad n flag)
	(cv (format "Polygon(%g, %g, %s);"
		rad n (if flag "true" "false")))
)


(define (cv:circle rad flag)
	(cv (format "Circle(%g, %s);" rad (if flag "true" "false")))
)


(define (cv:ellipse xr yr start end mode)
	(cv (format "Ellipse(%g, %g, %g, %g, %s);"
			xr yr start end (if mode "true" "false")))	
)


(define (cv:rectangle width height flag)
	(shape (list (list 0 height) (list 90 width) (list 90 height) (list 90 width)) flag)
)


(define (cv:pie rad width flag)
	(cv (format "Pie(%g, %g, %s);" rad width (if flag "true" "false")))
)


(define (cv:petal width height flag)
    ; x3 (offset from x1) cannot be 0 or the Bezier does not get drawn
	(bezier (sub width) height width height 0.001 0 flag)
	(cv (format "Bezier(%g, %g, %g, %g, %g, %g);"
			 (sub width) height width height 0.001 0))
	(cv "ctx.closPath")
	(if flag (cv "ctx.fill();"))
    (cv "ctx.stroke();")
)


(define (cv:shape lst flag) 
	(cv (format "Shape(new Array(%s), %s);" 
			(list-path lst)
			(if flag "true" "false")))
)


(define (cv:clip lst flag) 
	(cv (format "Clip(new Array(%s), %s);" 
		(list-path lst)  (if flag "true" "false")))
)


(define (cv:text str)
	(cv (format {Text("%s");} str))
)

(define (textoutline str)
	(cv (format {TextOutline("%s");} str))
)


(define (cv:textarc str radius)
	(cv (format {TextArc("%s", %g);} str radius))
) 


(define (cv:textarcoutline str radius)
	(cv (format {TextArcOutline("%s", %g);} str radius))
) 


(define (cv:translate x y)
	(if (and x y)
		(cv (format "ctx.translate(%g, -%g);" x y))
		(cv "ctx.translate(xpos, - (canvasHeight - ypos));")
	)
)


(define (cv:scale x y)
	(cv (format "Scale(%g, %g);" x y))
)


(define (cv:rotate dg)
	(cv (format "ctx.rotate(%g);" (mul pi/2 (div dg 90))))
)


(define (cv:gsave) (cv "Gsave();"))


(define (cv:grestore) (cv "Grestore();"))


(define (cv:font spec) (cv (format {ctx.font = "%s";} spec)))


(define (cv:line-cap mode) 
	(if (number? mode) (set 'mode (nth mode '("but" "round" "square"))))
	(cv (format "ctx.lineCap = '%s';" mode)))


(define (cv:line-join mode)
	(if (number? mode) (set 'mode (nth mode '("miter" "round" "bevel")))) 
	(cv (format "ctx.lineJoin = '%s';" mode)))


(define (line-color red green blue alpha)
    (if (string? red)
        (let (color red)
          (set 'red (div (int (append "0x" (0 2 color)) 0 16) 255))
          (set 'green (div (int (append "0x" (2 2 color)) 0) 255))
          (set 'blue (div (int (append "0x" (4 2 color)) 0) 255))))

	(if alpha
   	  (cv (format "ctx.strokeStyle = 'rgba(%d, %d, %d, %g)';" 
			(mul red 255) (mul green 255) (mul blue 255) alpha))
   	  (cv (format "ctx.strokeStyle = 'rgb(%d, %d, %d)';" 
			(mul red 255) (mul green 255) (mul blue 255) )))
)


(define (cv:line-width width)
	(cv (format "ctx.lineWidth = %g;" width))
)

(define (fill-color red green blue alpha)
    (if (string? red)
        (let (color red)
          (set 'red (div (int (append "0x" (0 2 color)) 0 16) 255))
          (set 'green (div (int (append "0x" (2 2 color)) 0) 255))
          (set 'blue (div (int (append "0x" (4 2 color)) 0) 255))))

	(if alpha
   	  (cv (format "ctx.fillStyle = 'rgba(%d, %d, %d, %g)';" 
			(mul red 255) (mul green 255) (mul blue 255) alpha))
   	  (cv (format "ctx.fillStyle = 'rgb(%d, %d, %d)';" 
			(mul red 255) (mul green 255) (mul blue 255) )))
)

(define (cv:render mode)
	(let (page (append (format template-header header-tags)
				script-header
				canvas-script
				script-close
				body-html
				body-close)) 
		(cond 	
			( 	(nil? mode) 
                ; on Emscripten open tab 
                (if eval-string-js 
                    (display-html page true)
                    (show-in-browser)))

	 		( 	(= (upper-case mode) "CGI") 
				(println page))

			( true 
				(write-file mode page))
		)
	)
)

(define cv:save cv:render) ; compatibility with older postscript.lsp code
				
(define (show-in-browser)
(write-file "/tmp/noname.html" page)
  (cond
    ( (= ostype "OSX")
      (exec "open /tmp/noname.html"))
    ( (= ostype "Windows")
      (set 'prog (string "cmd /c \"" (env "PROGRAMFILES") 
;                 Use either Firefox or Safari 4.0
;                 "/Mozilla Firefox 3.1 Beta 2/firefox.exe\""))
                  "/Safari/Safari.exe\""))
;     (println "->" prog "<-")
      (exec (string prog " file://c:/tmp/noname.html")))
    ( true // all Linux and other unix
      (set 'files '(
            "/usr/bin/sensible-browser"
            "/usr/bin/x-www-browser"
            "/usr/bin/mozilla"
            "/usr/bin/firefox"
            "/usr/bin/konqueror"
            ))
      (set 'prog (find true (map file? files)))
      (if prog
        (exec (string (files prog) " file:///tmp/noname.html"))
        (println "Cannot find browser to display documentation" "warning")) )
  ) ; cond
) ; define

(context MAIN)
; eof
