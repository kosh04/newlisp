;; @module postscript.lsp
;; @description Routines for creating postscript files
;; @version 2.0 - bugfixes and documentation overhaul.
;; @version 2.1 - doc changes
;; @version 2.2 - formatting
;; @version 2.3 - documentation
;; @version 2.4 - replaced if-not with unless
;; @version 2.45 - doc corrections
;; @version 2.5 - eliminated link to postscript-24.tgz
;; @author Lutz Mueller, July 2006, 2009, 2010, 2012, 2013, 2015
;;
;; <h2>Routines for creating postscript files</h2>
;; To use this module include the following 'load' statement at
;; the beginning of the program file:
;; <pre>
;; (load "/usr/share/newlisp/modules/postscript.lsp")
;; ; or shorter
;; (module "postscript.lsp")
;; </pre>
;;
;; See @link http://newlisp.org/index.cgi?Postscript http://newlisp.org/index.cgi?Postscript
;; for many examples with source code.
;;
;; Postscript files can be viewed using: 'open filename.ps' on Mac OS X
;; or using the Ghostscript program on Unix's or Win32 to convert
;; to PDF or any graphics file format. Best quality is achieved
;; on Mac OS X when using the Preview.app viewer for loading the 
;; postscript files and converting to PDF or bitmapped formats like 
;; JPEG, PNG, GIF by re-saving.
;;
;; If not using Mac OS X look for Ghostscript here:
;;    @link http://www.ghostscript.com/ www.ghostscript.com and
;;    here: @link http://www.cs.wisc.edu/~ghost/ www.cs.wisc.edu/~ghost/
;;
;; NOTE! on some Mac OS X installations it is necessary to quit out of
;; the Preview.app completely before viewing a '.ps' file for the first
;; time. Subsequent views of '.ps' documents are fine.
;;
;; On Linux/UNIX systems the following command can be used to convert
;; a '.ps' file to a '.pdf' file:
;; <pre>
;;   gs -sDEVICE=pdfwrite -dBATCH -sOutputFile=aFile.pdf -r300 aFile.ps
;; </pre>
;; Most functions work like in <turtle graphics> relative to the
;; current position of an imaginary drawing pen with an
;; orientation of 0 to 360 degree starting streight up: 0 and
;; moving clockwise right 90, down 180, left 270, up and 360 degrees.
;; Other functions work on absolute X,Y coordinates.
;;
;; The coordinate system starts on the bottom left with 0,0 and
;; extends on a 8 1/2 x 11 inch letter page to 'x': 612, 'y': 792,
;; 72 points for each inch. The functions 'ps:transpose' and 'ps:scale'
;; can be used to move the origin point '<x=0, y=0>' or scale from points to
;; other measurements.
;;
;; Return value from 'ps:xxx' functions are not used and not mentioned in
;; the documentation.
;;
;; <h2>Summary of functions</h2>
;; <h3>Turtle coordinate positioning and turning</h3>
;; <pre>
;; ps:goto  - move turtle to position x, y
;; ps:move  - move turtle a distance s forward from the current position
;; ps:turn  - turn the turtle degress dg left (negative) or right (positive)
;; ps:angle - set the turtle orientation to dg degrees
;; </pre>
;; <h3>Line drawing</h3>
;; <pre>
;; ps:draw   - draw distance s forward from current position
;; ps:drawto - draw to the absolute position x,y from the current position
;; ps:line   - draw a multipart line consisting of line and bezier curve segments
;; ps:bezier - draw a Bezier curve 
;; </pre>
;; <h3>Closed shapes, filling and clipping</h3>
;; <pre>
;; ps:rectangle - draw a rectangle
;; ps:polygon   - draw a polygon with radius rad and n number of sides
;; ps:circle    - draw a circle
;; ps:ellipse   - draw an open or closed ellipse with x-rad and y-rad radius
;; ps:pie       - draw a pie piece with radius rad and width
;; ps:petal     - draw a petal shape from Bezier curves
;; ps:shape     - draw a shape defined by a list of line and Bezier segments
;; ps:clip      - define a clipping path using line and Bezier segments
;; </pre>
;; <h3>Text output and clipping</h3>
;; <pre>
;; ps:text           - draw a solid text string
;; ps:textoutline    - draw text in outline shape
;; ps:textarc        - draw text around an arc
;; ps:textarcoutline - draw text in outline shape around an arc
;; ps:textclip       - use a textoutline as clipping path
;; </pre>
;; <h3>Global settings</h3>
;; <pre>
;; ps:translate  - move coordinate origin
;; ps:scale      - scale postscript output
;; ps:rotate     - rotate postscript output
;; ps:gsave      - save current graphics state (X, Y, orientation, translation, scale, rotation)
;; ps:grestore   - restore current graphics state
;; ps:font       - set font family and size
;; ps:line-witdh - set line width in points
;; ps:line-cap   - set line termination shape
;; ps:line-join  - set line join mode
;; ps:line-color - set line color
;; ps:fill-color - set fill color
;; </pre>
;; <h3>Rendering and output</h3>
;; <pre>
;; ps:render     - render output to a monitor
;; ps:save       - deprecated, use ps:render with file-name instead
;; </pre>

;; @syntax (ps:angle <num-dg>) 
;; @param <num-dg> Angle degrees from 0 to 360.
;; <br>
;; Set the turtle angle to <num-dg> degrees.
;; Upwards is 0, right 90, downwards 180 and left 270 degrees.
;; The turtle position is saved on the graphics state stack when using 
;; '(ps:gsave)'.

;; @syntax (ps:bezier <num-x1> <num-y1> <num-x2> <num-y2> <num-x3> <num-y3>) 
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

;; @syntax (ps:circle <num-rad> [<bool-fill>])
;; @param <num-rad> Radius of the circle.
;; @param <bool-fill> Optional fill flag.
;; <br>
;; Draw a circle with radius <num-rad>. The optional <num-fill> flag 
;; with either 'true' or 'nil' (default) indicates if the circle
;; is filled with the fill color specified by 'ps:fill-color'.
;; The circle is drawn around the current turtle position.
;; The turtle position or orientation is not changed.

;; @syntax (ps:clip <list-of-num-lists>) 
;; @param <list-of-num-lists> A list of turtle movements and/or Bezier curves.
;; Define a clipping path using turtle movements (<degree> <distance>) and
;; <br>
;; Bezier curves (<x1> <y1> <x2> <y2> <x3> <y3>) starting from the 
;; last turtle coordinates <x0>, <y0> and finishing at <x3>, <y3>. 
;; All Bezier coordinates are relative to the previous turtle position and
;; orientation.
;;
;; Before redefining the clipping area '(ps:gsave)' should
;; be used to save the old graphics state parameters, after
;; clipping and drawing in the clipped area the graphics
;; state should be restored using '(ps:grestore)'.
;; The turtle position or orientation is not changed.

;; @syntax (ps:draw <num-s>) 
;; @param <num-s> Distance to draw.
;; <br>
;; Draw going forward distance <num-s>. Moves the turtle forward by 
;; the amount of points specified in <num-s> and draws with the current 
;; line color set by 'ps:line-color'.
;;

;; @syntax (ps:drawto <x> <y>) 
;; @param <x> The x coordinate to draw to.
;; @param <y> The y coordinate to draw to.
;; <br>
;; Draw a line to point <x>, <y>. Moves the turtle to point 
;; <x>, <y> like '(ps:goto x y)', but also draws a line from 
;; the old to the new position. The turtle position is changed to the
;; new point <x>, <y> and the orientation is changed to the orientaion of 
;; the line drawn.

;; @syntax (ps:ellipse <num-x-rad> <num-y-rad> <num-start> <num-end> [<bool-fill>]) 
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

;; @syntax (ps:fill-color <float-R> <float-G> <float-B>) 
;; @param <float-R> The red color value.
;; @param <float-G> The green color value.
;; @param <float-B> The blue color value.
;; <br>
;; Set color for shape filling.
;; Color values assume the following value:
;; <pre>
;;    R - value for red 0.0 to 1.0
;;    B - value for green 0.0 to 1.0
;;    G - value for blue 0.0 to 1.0
;; </pre>
;;
;; @syntax (ps:fill-color <str-hex>)
;; @param <str-hex> A hex string specifying the line color.
;; <br>
;; In an alternative syntax color values can be specified in a
;; hex string:
;;
;; <str-hex> is a hex string constant '"000000"' to '"FFFFFF"'
;; Colors are specified as usual in HTML coding.
;; Each two hex digits define a color: 'rrggbb'.  

;; @syntax (ps:font <str-name> <num-size>) 
;; @param <str-name> The font name.
;; @param <num-size> The size of the font in points.
;; <br>
;; The current font is set for all subsequent text operations.
;; Depending on the version of the Postsrcipt viewer or device
;; installed different fonts are available.
  

;; @syntax (ps:goto <num-x> <num-y>)
;; @param <num-x> The new x coordinate.
;; @param <num-y> The new y coordinate.
;; <br>
;; Moves to position <num-x>, <num-y>. On a US letter page of 612 by 792 
;; point positions are defined with 72 points per inch. The turtle position
;; is saved on the graphics state stack when using '(ps:gsave)'.

;; @syntax (ps:grestore)
;; <br>
;; Restores the graphics state from the stack.

;; @syntax (ps:gsave)
;; <br>
;; Saves the current graphics state. The function pushes the 
;; current graphics state on a special stack, from where it
;; can be resored using '(ps:grestore)'. States saved are:
;; The turtle position X, Y and  orientation and transformation
;; scaling and rotation factors.

;; @syntax (ps:line <list-of-lists>) 
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

;; @syntax (ps:line-cap <num-mode | str-mode>) 
;; @param <mode> The line termination shape mode as a string or number
;; <br>
;; Sets the line termination shape as either a number or string:
;; <pre>
;;    0 or "butt"
;;    1 or "round"
;;    2 or "square"
;; </pre>

;; @syntax (ps:line-color <float-R> <float-G> <float-B>) 
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
;; @syntax (ps:line-color <str-hex>)
;; @param <str-hex> A hex string specifying the line color.
;; <br>
;; In an alternative syntax color values can be specified in a
;; hex string:
;;
;; <str-hex> is a hex string constant '"000000"' to '"FFFFFF"'
;; Colors are specified as usual in HTML coding.
;; Each to two hex digits define a color: 'rrggbb'.  

;; @syntax (ps:line-join <num-mode> | <str-mod>)
;; @param <mode> The line join mode.
;; <br>
;; Sets the line join mode as either a number or string:
;; <pre>
;;    0 or "miter"
;;    1 or "round"
;;    2 or "bevel"
;; </pre>

;; @syntax (ps:line-width <points>)
;; @param <points> The line width in points.
;; <br>
;; Sets the line width in points for line drawing and the
;; outlines drawn by shapes and text outlines.

;; @syntax (ps:move <num-s>) 
;; @param <num-s> The distance to move the pen.
;; <br>
;; Move the turtle forward distance <s> without drawing.

;; @syntax (ps:petal <num-width> <num-height> [<bool-fill>]) 
;; @param <num-width> The 'x1' coordinate of the  underlying Bezier curve <p0> to <p1> <p2> <p3>.
;; @param <num-height> The 'y1' coordinate of the  underlying Bezier curve <p0> to <p1> <p2> <p3>.
;; @param <bool-fill> An optional fill flag for color fill.
;; <br>
;; Draws a petal using a Bezier curve with optional <num-fill> either 'true' or 'nil' (default).
;; The <num-height> and <num-width> parameters are relative to to the current position.
;; The petal is drawn with the tip at the current turtle
;; position and oriented in the direction of the turtle.
;; The turtle position or orientation is not changed.

;; @syntax (ps:pie <num-rad> <num-width> [<bool-fill>])
;; @param <num-rad> The radius of the pie.
;; @param <num-width> The width of the pie slice as an angle.
;; @param <bool-fill> An optional fill flag for color fill, 'true' or 'nil' (default).
;; <br>
;; Draw a pie slice with optional <bool-fill> either 'true' or 'nil' (default).
;; The left edge of the pie is in turtle orientation.
;; The width angle spreads clockwise. The pie is drawn around the current
;; turtle position. The turtle position or orientation is not changed.


;; @syntax (ps:polygon <num-rad> <num-n> [<bool-fill>])
;; @param <num-rad> Radius.
;; @param <num-n> Number of sides.
;; @param <fill> Optional fill flag.
;; <br>
;; Draw a polygon with radius <num-rad> and <num-n> sides.
;; <num-fill> is 'true' or 'nil' (default) for optional color fill
;; The polygon is drawn around the current turtle position.
;; The turtle position or orientation is not changed.

;; @syntax (ps:rectangle <num-width> <num-height> [<bool-fill>])
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

;; @syntax (ps:render [<filename>]) 
;; <br>
;; Without the filename parameter, <tt>render</tt> creates a file <tt>noname.ps</tt>
;; and on Mac OS X the file is shown on the monitor using the Mac OS X Preview
;; application. 
;;
;; Specify the <filename> parameter
;; to save the postscript file and convert and view
;; it using ghostscript from @link http://www.ghostscript.com/ www.ghostscript.com/
;; and @link http://www.cs.wisc.edu/~ghost/ www.cs.wisc.edu/~ghost .
;;

;; @syntax (ps:rotate <num-deg>) 
;; @param <num-deg> The degrees of rotation: -360 to 0 to 360.
;; <br>
;; Rotate the coordinate space. 
;; The coordinate space is rotated to the right for
;; positive angles and to the left for negative angles.
;; The current rotation angle is 0 by default.
;; The rotation angle is part of the graphics state saved by
;; the 'ps:gsave' function and restored by 'ps:grestore'.

;; @syntax (ps:save <str-filename>) 
;; @param <str-filename> The filename. 
;; <br>
;; Save to <str-filename>. This function is deprecated use 'ps:render'
;; instead.

;; @syntax (ps:scale <num-x> <num-y>) 
;; @param <num-x> The new x scale factor.
;; @param <num-y> The new y scale factor.
;; <br>
;; Scale the coordinate space.
;; Scaling factors are 1.0 by default and compress for
;; factors less 1.0 or expand for factors bigger than 1.0.
;; With a scaling factor for x = 2.0 each point position
;; specified would cover the double of horizontal distance
;; on the page. Previous scaling factors can be saved on the graphics
;; state stack using the function 'ps:gsave' and restored using 'ps:grestore'.

;; @syntax (ps:shape <list-of-num-lists> [<bool-fill>])
;; @param <list-of-num-lists> A list of turtle movements and/or Bezier curves.
;; @param <bool-fill> An optional fill flag for color fill.
;; <br>
;; Draws a shape with optional <bool-fill> or eiher 'true' or 'nil' (default).
;; <num-lists> is either a turtle movement  (<degree> <distance>) or a Bezier curve 
;; (<x1> <y1> <x2> <y2> <x3> <y3>) starting from the last turtle coordinates 
;; <x0>, <y0> and finishing at <x3>, <y3>. All Bezier coordinates
;; are relative to the previous turtle position and orientation
;; The turtle position or orientation is not changed.

;; @syntax (ps:text <str-text>)
;; @param <str-text> The text to draw.
;; <br>
;; Draws text. <tt>(...)</tt> parenthesis in text should be escaped with
;; double <tt>\\</tt> characters as in in <tt>\\(</tt> or <tt>\\)</tt>, when limiting the string
;; with double quotes <tt>"</tt>. When limiting the string with <tt>{,}</tt> braces
;; a single <tt>\</tt> character is enough as in <tt>\(</tt> and <tt>\)</tt>.
;; Before drawing, a font can be specified, the default font after loading 
;; the 'postscript.lsp' modules is Helvetica 12 points and using
;; the current 'ps:line-color' for drawing.
;;
;; The turtle position is changed to the baseline after the last character.
;; The turtle orientation stays the same.

;; @syntax (ps:textarc <str-text> <num-rad>)
;; @param <str-text> The text to draw. 
;; @param <num-rad> The radius of imaginary circle path for text.
;; <br>
;; Draw text around a circle.
;; The text is drawn out side of an imaginary circle starting at 
;; turtle position and orientation and drawing at the current tangent.
;; For a positive radius text goes outside
;; the circle and clockwise. For a negative radius text goes inside the
;; circle and counter lock wise. The turtle position and orientation
;; move along the radius.

;; @syntax (ps:textarcoutline <str-text> <num-rad> [<bool-fill>]) 
;; @param <str-text> The text to draw.
;; @param <num-rad> The radius of imaginary circle path for text.
;; @param <bool-fill> An optional fill flag for color fill.
;; <br>
;; Draw text around a circle.
;; Same as 'ps:textarc' but the text is drawn as ane outline
;; and can be filled with ps:fill-color when specifying the optional
;; fill flag. The turtle position and orientation move along the radius.

;; @syntax (ps:textoutline <str-text> [<bool-fill>])
;; @param <str-text> The text to draw. 
;; @param <bool-fill> An optional fill flag for color fill.
;; <br>
;; Draw a text outline with optional color <bool-fill> specified by
;; either 'true' or 'nil' (default).
;; Before drawing a font can be specified
;; the default font after loading 'postscript.lsp' is
;; Helvetica 12 points, the text is drawn using the current
;; line color.
;;
;; The turtle position is changed to the baseline after the last character.
;; The turtle orientation stays the same.

;; @syntax (ps:textclip <str-text>)
;; @param <str-text> The text used as a clipping shape.
;; <br>
;; A text outline is used as a clipping path.
;; Before redefining the clipping area '(ps:gsave)' should
;; be used to save the old graphics state parameters, after
;; clipping and drawing in the clipped area the graphics
;; state should be restored using '(ps:grestore)'.
;; The turtle moves with the text shape clipped.

;; @syntax (ps:translate <num-dx> <num-dy>) 
;; @syntax (ps:translate) 
;; @param <num-dx> Moves the 'x' origin  by 'dx'.
;; @param <num-y> Move the 'y' origin by 'dy'.
;; <br>
;; Move the coordinate origin.
;; By default the origin 0,0 is in the bottom left corner
;; of the page. The <num-dx> and <num-dy> values extend to the right and top. 
;; When no <num-x>, <num-y> values are specified the coordinate origin
;; is moved to the current position of the turtle. Previous translation
;; offsets can be saved on the graphics state stack using the
;; function 'ps:gsave' and restored using 'ps:grestore'.

;; @syntax (ps:turn <num-dg>)
;; @param <num-dg>  The degrees to turn: -360 to 0 to 360.
;; <br>
;; Turn the turtle pen by <num-dg> degrees. The degrees are specified in angles
;; from 0 to 360. For turning clockwise specifiy positive values.
;; Negative degrees turn the turtle pen counter clockwise. The turtle
;; position is aved on the graphics state stack when using '(ps:gsave)'.

(context 'ps)

(set 'prolog [text]%!PS-Adobe-2.0
%%Creator: newLISP

%% ---------- SETUP ----------

/orient 0 def
/xpos 0 def
/ypos 0 def
/pi 3.141592654 def

/fillcolor {0.8 0.8 0.8} def
/Helvetica findfont 12 scalefont setfont

/turtlestack [0 0 0] def

/pushturtle
	{	
	turtlestack length /len exch def 
	turtlestack aload pop 
	xpos ypos orient len 3 add array astore
	/turtlestack exch def
	} def

/popturtle
	{
	turtlestack length /len exch def
	len 3 gt {
		turtlestack aload pop
		/orient exch def
		/ypos exch def
		/xpos exch def
		len 3 sub array astore
		/turtlestack exch def
		} if
	} def

%% ---------- NAVIGATION ----------

% x y -
/goto
  {
  /ypos exch def
  /xpos exch def
  xpos ypos moveto
  } def

% points -
/move
  {
  /len exch def
  /xpos xpos orient sin len mul add def
  /ypos ypos orient cos len mul add def
  xpos ypos moveto
  } def
  
% degree -
/turn
  {
  /orient exch orient add def
  } def
  
% degree -
/angle
  {
  /orient exch def
  } def
  
%% ---------- LINE DRAWING ----------

% turtle position is changed
  
% points -
/draw
  {
  /len exch def
  newpath
  xpos ypos moveto
  /xpos xpos orient sin len mul add def
  /ypos ypos orient cos len mul add def
  xpos ypos lineto stroke
  } def  

% points -
/drawtolen
  {
  /len exch def
  /xpos xpos orient sin len mul add def
  /ypos ypos orient cos len mul add def
  xpos ypos lineto
  } def  
  
% x y
/drawto
  {
  /newy exch def
  /newx exch def
  newpath
  xpos ypos moveto
  newx newy lineto
  stroke
  newy ypos sub newx xpos sub atan neg 90 add /orient exch def
  /xpos newx def
  /ypos newy def
  } def
  
% x1 y1 x2 y2 x3 y3
/bezier
  {
  newpath
  curve
  stroke
  } def

/curve
  {
  /y3 exch def
  /x3 exch def
  /y2 exch def
  /x2 exch def
  /y1 exch def
  /x1 exch def
  matrix currentmatrix
  x1 y1 x2 y2 x3 y3
  xpos ypos translate
  orient neg rotate
  0 0 moveto
  rcurveto
  setmatrix
  y3 x3 atan neg /angleinc exch def
  /len x3 angleinc cos div def
  /orient orient angleinc add def
  /xpos xpos orient 90 add sin len mul add def
  /ypos ypos orient 90 add cos len mul add def
  } def

% save turtle position and orientation

/turtlesave
  {
  /xpossave xpos def
  /ypossave ypos def
  /orientsave orient def
  } def
  
% restore turtle position and orientation

/turtlerestore
  {
  /xpos xpossave def
  /ypos ypossave def
  /orient orientsave def
  xpos ypos moveto
  } def

% x1 y1 x2 y2 -
/fromto
  {
  /ypos exch def
  /xpos exch def
  newpath
  moveto
  xpos ypos lineto
  stroke
  } def

%% ---------- SHAPES ----------

% shapes are closed and do not change the turtle position

% radius sides fillflag -
/polygon
  {
  /fillflag exch def
  360 exch div /orientinc exch def
  /radius exch def
  gsave
  newpath
  xpos ypos translate
  orient neg rotate
  % 0 sin radius mul
  % 0 cos radius mul moveto
  0 radius moveto
  0 orientinc 360
   {
   dup
   sin radius mul exch
   cos radius mul
   lineto
   } for
  closepath
  fillflag {fillshape} if
  stroke
  grestore
  } def
  
% radius fillflag -
/circle
  {
  /fillflag exch def
  /radius exch def
  newpath
  xpos ypos radius 0 360 arc
  fillflag {fillshape} if
  stroke
  } def
  
  
% radius width fillflag
/pie
  {
  /fillflag exch def
  /width exch def
  90 orient sub width sub /start exch def
  start width add /end exch def
  /radius exch def
  newpath
  xpos ypos moveto
  xpos ypos radius start end arc
  fillflag {fillshape} if
  closepath
  stroke
  } def
  
% width height fill
/petal
  {
  /fillflag exch def
  /y exch def
  /x exch def
  gsave
  xpos ypos translate
  orient neg rotate
  newpath
  0 0 moveto
  x neg y x y 0 0 
  rcurveto
  fillflag {fillshape} if
  closepath
  stroke
  grestore
  } def
  
% xradius yradius start end flag -
/ellipse
  {
  /fillflag exch def
  % swap start/end and x/y
  neg /startangle exch def
  neg /endangle exch def
  /xrad exch def
  /yrad exch def
 
  gsave
  xpos ypos translate
  orient 90 sub neg rotate
  newpath
  xrad yrad scale
  0 0 1 startangle endangle arc
  fillflag {fillshape} if
  1 xrad div 1 yrad div scale
  closepath
  stroke
  grestore
  } def

/fillshape
  {
  gsave
  fillcolor setrgbcolor
  fill 
  grestore
  } def
   
%% ---------- text ----------

/text
  {
  /str exch def
  gsave
  xpos ypos translate
  newpath
  0 0 moveto
  orient 89.9999 sub neg rotate
  str show
  grestore
  str stringwidth pop move
  } def
  
/textoutline
  {
  /fillflag exch def
  /str exch def
  gsave
  xpos ypos translate
  newpath
  0 0 moveto
  orient 89.9999 sub neg rotate
  str true charpath
  fillflag {fillshape} if
  stroke
  grestore
  str stringwidth pop move
  } def
  
/textclip
  {
  /str exch def
  matrix currentmatrix
  xpos ypos translate
  newpath
  0 0 moveto
  orient 89.9999 sub neg rotate
  str true charpath
  clip
  setmatrix
  } def

/textarc
  {
  /str exch def
  2 mul pi mul /circum exch def
  
  str stringwidth pop /len exch def
  circum len div 360 exch div turn
  str text
  } def
  
/textarcoutline
  {
  /fillflag exch def
  /str exch def
  2 mul pi mul /circum exch def
  
  str stringwidth pop /len exch def
  circum len div 360 exch div turn
  str fillflag textoutline
  } def
  
% --------------------------
[/text])

; ---------- setup ---------- 

(set 'buffer "")
(set 'line-feed (if (> (& 0xF (sys-info -1)) 5) "\r\n" "\n"))

; ---------- USER FUNCTIONS ----------

; ---------- output pure postscript ---------- 

(define (ps:ps str) (write-line buffer str) true)
	
; navigation - changes position or orient of the turtle
	
(define (goto x y)
	(ps (format "%g %g goto" x y)))
	
(define (turn deg)
	(ps (format "%g turn" deg)))
	
(define (move dist)
	(ps (format "%g move" dist)))
	
(define (angle deg)
	(ps (format "%g angle" deg)))
	
; line graphics changes position and/or orient of the turtle
	
(define (draw dist)
	(ps (format "%g draw" dist)))
	
(define (drawto x y)
	(ps (format "%g %g drawto" x y)))
	
(define (bezier x1 y1 x2 y2 x3 y3)
	(if (zero? x3) (set 'x3 1e-7))
	(ps (format "%g %g %g %g %g %g bezier" x1 y1 x2 y2 x3 y3)))
	
(define (line lst)
	(let (rec nil)
		(ps "% new shape")
		(ps "newpath")
		(ps "xpos ypos moveto")
		(while (set 'rec (pop lst))
			(if (= (length rec) 6)
				(ps (format "%g %g %g %g %g %g curve"
						(rec 0) (rec 1) (rec 2)
						(rec 3) (rec 4) (rec 5)))
				(begin
					(ps (format "%g turn" (rec 0)))
					(ps (format "%g drawtolen" (rec 1))))))
		(ps "stroke")))
	
; shapes do not change the position or orient of the turtle
; which stays in the curcle center of the shape

(define (polygon radius sides flag)
	(set 'flag (if flag "true" "false"))
	(ps (format "%g %g %s polygon" radius sides flag)))

(define (rectangle width height flag)
	(shape (list (list 0 height) (list 90 width) (list 90 height) (list 90 width)) flag))

(define (circle radius flag)
	(set 'flag (if flag "true" "false"))
	(ps (format "%g %s circle" radius flag)))
	
(define (ellipse xradius yradius start end flag)
	(set 'flag (if flag "true" "false"))
	(ps (format "%g %g %g %g %s ellipse" xradius yradius start end flag)))
	
(define (pie radius width flag)
	(set 'flag (if flag "true" "false"))
	(ps (format "%g %g %s pie" radius width flag)))
	
(define (petal width height flag)
	(set 'flag (if flag "true" "false"))
	(ps (format "%g %g %s petal" width height flag)))
	
(define (shape lst flag)
	(let (rec nil)
		(ps "% new shape")
		(ps "turtlesave")
		(ps "newpath")
		(ps "xpos ypos moveto")
		(while (set 'rec (pop lst))
			(if (= (length rec) 6)
				(ps (format "%g %g %g %g %g %g curve"
						(rec 0) (rec 1) (rec 2)
						(rec 3) (rec 4) (rec 5)))
				(begin
					(ps (format "%g turn" (rec 0)))
					(ps (format "%g drawtolen" (rec 1))))))
		(ps "closepath")
		(if flag (ps "fillshape")) 
		(ps "stroke")
		(ps "turtlerestore")))

(define (clip lst)
	(let (rec nil)
		(ps "% new clipping shape")
		(ps "turtlesave")
		(ps "newpath")
		(ps "xpos ypos moveto")
		(while (set 'rec (pop lst))
			(if (= (length rec) 6)
				(ps (format "%g %g %g %g %g %g curve"
						(rec 0) (rec 1) (rec 2)
						(rec 3) (rec 4) (rec 5)))
				(begin
					(ps (format "%g turn" (rec 0)))
					(ps (format "%g drawtolen" (rec 1))))))
		(ps "closepath")
		(ps "clip"))
		(ps "turtlerestore"))
		
; text output 

(define (text str)
	(ps (format "(%s) text" str)))
	
(define (textoutline str flag)
	(set 'flag (if flag "true" "false"))
	(ps (format "(%s) %s textoutline" str flag)))
	
(define (textclip str)
	(ps (format "(%s) textclip" str)))
		
(define (textarc str radius)
	(dotimes (i (length str))
		(ps (format "%g (%s) textarc" radius (str i)))))
	
(define (textarcoutline str radius flag)
	(set 'flag (if flag "true" "false"))
	(dotimes (i (length str))
		(ps (format "%g (%s) %s textarcoutline" radius (str i) flag))))
	
; rendering and saving

(define (render filename)
	(let (page (append prolog buffer "showpage" line-feed))
		(if filename
			(write-file filename page)
			(begin
				(write-file "/tmp/noname.ps" page)
				(exec "open /tmp/noname.ps")))))

(define ps:save render)
	
(define (clear)
	(set 'buffer ""))

; replaced by ps:render with filename	
(define (ps:save file-name)
	(write-file file-name (append prolog buffer "showpage" line-feed)))
	
; global parameters 

(define (translate x y)
	(if (and x y)
		(ps (format "%g %g translate" x y))
		(ps "xpos ypos translate 0 0 moveto")))
	
(define (scale x y)
	(ps (format "%g %g scale" x y)))
	
(define (ps:rotate deg)
	(ps (format "%g rotate" deg)))

(define (gsave)
	(ps "pushturtle gsave"))
	
(define (grestore)
	(ps "grestore popturtle"))
	
(define (line-width points)
	(ps (format "%g setlinewidth" points)))
	
(define (line-cap mode)
	(if (string? mode) (set 'mode (find mode '("butt" "round" "square"))))
	(unless mode (throw-error "Not a valid line-cap mode"))
	(ps (format "%g setlinecap" mode)))

(define (line-join mode)
	(if (string? mode) (set 'mode (find mode '("miter" "round" "bevel"))))
	(ps (format "%g setlinejoin" mode)))
	
(define (line-color red green blue)
    (if (string? red)
    	(let (color red)
    	  (set 'red (div (int (append "0x" (0 2 color)) 0 16) 255))
    	  (set 'green (div (int (append "0x" (2 2 color)) 0) 255))
    	  (set 'blue (div (int (append "0x" (4 2 color)) 0) 255))))
	(ps (format "%g %g %g setrgbcolor" red green blue)))
	
(define (fill-color red green blue)
    (if (string? red)
    	(let (color red)
    	  (set 'red (div (int (append "0x" (0 2 color)) 0 16) 255))
    	  (set 'green (div (int (append "0x" (2 2 color)) 0) 255))
    	  (set 'blue (div (int (append "0x" (4 2 color)) 0) 255))))
	(ps (format "/fillcolor {%g %g %g} def" red green blue)))
	
(define (font fname size)
	(ps (format "/%s findfont %g scalefont setfont" fname size)))
	
(context MAIN)

; eof ;
