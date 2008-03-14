;; @module postscript.lsp
;; @description Routines for creating postscript files
;; @version 1.0 - comments redone for automatic documentation
;; @author Lutz Mueller, July 2006
;;
;; <h2>Routines for creating postscript files</h2>
;; To use this module include the following 'load' statement at
;; the beginning of the program file:
;; <pre>
;; (load "/usr/share/newlisp/modules/postscript.lsp")
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
;; 12 points for each inch. The functions 'ps:transpose' and 'ps:scale'
;; can be used to move the origin point '<x=0, y=0>' or scale from points to
;; other measurements.
;;
;; <br/>
;; <center><h2>Turtle coordinate positioning and turning</h2></center>
;;
;; All commands in this group change the turtle position or 
;; orientation.
;;
;; @syntax (ps:goto <num-x> <num-y>)
;; @param <num-x> The new x coordinate.
;; @param <num-y> The new y coordinate.
;; @return The postscript output.
;; Moves to position <num-x>, <num-y>. On a US letter page of 612 by 792 
;; point positions are defined with 72 points per inch.
;;
;; @syntax (ps:move <num-s>) 
;; @param <num-s> The distance to move the pen.
;; @return The postscript output.
;; Move turtle forward distance <s> without drawing.
;;
;; @syntax (ps:turn <num-dg>)
;; @param <num-dg>  The degrees to turn: -360 to 0 to 360.
;; @return The postscript output.
;; Turn the turtle pen by <dg> degrees. The degrees are specified in angles
;; from 0 to 360. For turning clockwise specifiy positive values.
;; Negative degrees turn the turtle pen counter clockwise.
;;
;; @syntax (ps:angle <num-dg>) 
;; @param <num-dg> Angle degrees from 0 to 360.
;; @return The postscript output.
;; Set the turtle angle to <num-dg> degrees.
;; Upwards is 0, right 45, downwards 180 and left 270i degrees.
;;
;; <br/>
;; <center><h2>Line drawing</h2></center>
;;
;; All commands in this group change the turtle position or 
;; orientation, or both.
;;
;; @syntax (ps:draw <num-s>) 
;; @param <num-s> Distance to draw.
;; @return The postscript output.
;; Draw going forward distance <num-s>. Moves the turtle forward by 
;; the amount of points specified in <num-s> and draws with the current 
;; line color set by 'ps:line-color'.
;;
;; @syntax (ps:drawto <x> <y>) 
;; @param <x> The x coordinate to draw to.
;; @param <y> The y coordinate to draw to.
;; @return The postscript output.
;; Draw a line to point <x>, <y>. Moves the turtle to point 
;; <x>, <y> like '(ps:goto x y)', but also draws a line from 
;; the old to the new position. The turtle position is changed to the
;; new point <x>, <y> and the orientation is changed to the orientaion of 
;; the line drawn.
;;
;; @syntax (ps:line <list-of-lists>) 
;; @param <list-of-lists> A list of turtle movements or Bezier curves.
;; @return The postscript output.
;; Draw a multipart line.  <lists> are turtle movements (<num-dg> <num-s>),
;; or Bezier curves (<x1> <y1> <x2> <y2> <x3> <y3>) starting
;; from the last turtle coordinates <x0>, <y0> and
;; finishing at <x3>, <y3>. All Bezier coordinates are
;; relative to the previous turtle position and
;; orientation.
;;
;; The turtle position and orientation are changed after
;; drawing the line.
;;
;; @syntax (ps:bezier <x1> <y1> <x2> <y2> <x3> <y3>) 
;; @param  <x1,y1> Bezier coordinates of <p1> relative to <p0> = 0,0
;; @param  <x2,y2> Bezier coordinates of <p2> relative to <p0> = 0,0
;; @param  <x3,y3> Bezier coordinates of <p3> relative to <p0> = 0,0
;; @return The postscript output.
;; Draw a Bezier curve.
;; The Bezier curve starts at point <p0> which is the current
;; turtle position and stops at point <p3> which is at offset
;; <x3> and <y3> relative to starting point. The turtle orientation
;; after the drwaing the Bezier curve is perpendicular
;; to the Bezier curve baseline <p0> to <p3>.
;;
;; <br/>
;; <center><h2>Closed shapes, filling and clipping</h2></center>
;;
;; All functions in this group leave the turtle position and
;; orientation unchanged.
;;
;; @syntax (ps:polygon <rad> <n> [<fill>])
;; @param <rad> Radius.
;; @param <n> Number of sides.
;; @param <fill> Optional fill flag.
;; @return The postscript output.
;; Draw a polygon with radius <rad> and <n> sides.
;; <fill> is 'true' or 'nil' (default) for optional color fill
;; The polygon is drawn around the current turtle position.
;; The turtle position or orientation is not changed.
;;
;; @syntax (ps:circle <rad> [<fill>])
;; @param <rad> Radius of the circle.
;; @param <fill> Optional fill flag.
;; @return The postscript output.
;; Draw a circle with radius <rad>. The optional <fill> flag 
;; with either 'true' or 'nil' (default) indicates if the circle
;; is filled with the fill color specified by 'ps:fill-color'.
;; The circle is drawn around the current turtle position.
;; The turtle position or orientation is not changed.
;;
;; @syntax (ps:ellipse <x-rad> <y-rad> <start> <end> [<fill>]) 
;; @param <x-rad> The x axis radius.
;; @param <y-rad> The y axis radius.
;; @param <start> The start angle.
;; @param <end> The end angle.
;; @return The postscript output.
;; Draw an ellipse with optional <fill> either 'true' or 'nil'(default).
;; The ellipse is drawn around the current turtle position
;; with the Y axis oriented like the turtle.
;; For <x-rad>, <y-rad> set to 0, 360 an ellipse is drawn
;; For a partiual radius the opening is closed by a line
;; resulting in segment shape, i.e. -90, 90  would result
;; in a half circle from the left to the right of the turtle.
;; The turtle position or orientation is not changed.
;;
;; @syntax (ps:pie <rad> <width> [<fill>])
;; @param <rad> The radius of the pie.
;; @param <width> The width of the pie slice as an angle.
;; @param <fill> An optional fill flag for color fill
;; @return The postscript output.
;; Draw a pie slice with optional <fill> either 'true' or 'nil' (default).
;; The left edge of the pie is in turtle orientation.
;; The width angle spreads clockwise. The pie is drawn around the current
;; turtle position. The turtle position or orientation is not changed.
;;
;; @syntax (ps:petal <x> <y> [<fill>]) 
;; @param <x> The <x1> coordinate of the  underlying Bezier curve <p0> to <p1> <p2> <p3>.
;; @param <y> The <y1> coordinate of the  underlying Bezier curve <p0> to <p1> <p2> <p3>.
;; @param <fill> An optional fill flag for color fill.
;; @return The postscript output.
;; Draws a petal using a Bezier curve with optional <fill> either 'true' or 'nil' (default).
;; The <x> and <y> parameters are relative to to the current position.
;; The petal is drawn with the tip at the current turtle
;; position and oriented in the direction of the turtle.
;; The turtle position or orientation is not changed.
;;
;; @syntax (ps:shape <list-of-lists> [<fill>])
;; @param <list-of-lists> A list of turtle movements and/or Bezier curves.
;; @param <fill> An optional fill flag for color fill.
;; @return The postscript output.
;; Draws a shape with optional <fill> or eiher 'true' or 'nil' (default).
;; <lists> is either a turtle movement  (<dg> <s>) or a Bezier curve 
;; (<x1> <y1> <x2> <y2> <x3> <y3>) starting from the last turtle coordinates 
;; <x0>, <y0> and finishing at <x3>, <y3>. All Bezier coordinates
;; are relative to the previous turtle position and orientation
;; The turtle position or orientation is not changed.
;;
;; @syntax (ps:clip <list-of-lists>) 
;; @param <list-of-lists> A list of turtle movements and/or Bezier curves.
;; @return The postscript output.
;; Define a clipping path using turtle movements (<dg> <s>) and
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
;;
;; <br/>
;; <center><h2>Text output and clipping</h2></center>
;; 
;; All functions in this goup move the turtle by the textlength
;; drawn and change the orientation when the text is arcing.
;;
;; @syntax (ps:text <str-text>)
;; @param <str-text> The text to draw.
;; @return The postscript output.
;; Draws text. '(...)' parenthesis in text should be escaped with
;; double '\\' characters as in in '\\(' or '\\)', when limiting the string
;; with double quotes '"'. When limiting the string with '{,}' braces
;; a single '\' character is enough as in '\(' and '\)'.
;; Before drawing, a font can be specified, the default font after loading 
;; the 'postscript.lsp' modules is Helvetica 12 points and using
;; the current 'ps:line-color' for drawing.
;;
;; The turtle position is changed to the baseline after the last character.
;; The turtle orientation stays the same.
;;
;; @syntax (ps:textoutline <str-text> [<fill>])
;; @param <str-text> The text to draw. 
;; @param <fill> An optional fill flag for color fill.
;; Draw a text outline with optional color <fill> specified by
;; either 'true' or 'nil' (default).
;; Before drawing a font can be specified
;; the default font after loading 'postscript.lsp' is
;; Helvetica 12 points, the text is drawn using the current
;; line color.
;;
;; The turtle position is changed to the baseline after the last character.
;; The turtle orientation stays the same.
;;
;; @syntax (ps:textarc <rad> <str-text>)
;; @param <rad> The radius of imaginary circle path for text.
;; @param <str-text> The text to draw. 
;; @return The postscript output.
;; Draw text around a circle.
;; The text is drawn out side of an imaginary circle starting at 
;; turtle position and orientation and drawing at the current tangent.
;; To bend text around a circle, draw a sequence of single characters
;; with the same radius. For a positive radius text goes outside
;; the circle and clockwise. For a negative radius text goes inside the
;; circle and counter lock wise. The turtle position and orientation
;; move along the radius.
;;
;;
;; @syntax (ps:textarcoutline <rad> <str-text> [<fill>]) 
;; @param <rad> The radius of imaginary circle path for text.
;; @param <str-text> The text to draw.
;; @param <fill> An optional fill flag for color fill.
;; @return The postscript output.
;; Draw text around a circle.
;; Same as '(ps:textarc ...)' but the text is drawn as ane outline
;; and can be filled with ps:fill-color when specifying the optional
;; fill flag. The turtle position and orientation move along the radius.
;;
;; @syntax (ps:textclip <str-text>)
;; @param <str-text> The text used as a clipping shape.
;; @return The postscript output.
;; A text outline is used as a clipping path.
;; Before redefining the clipping area '(ps:gsave)' should
;; be used to save the old graphics state parameters, after
;; clipping and drawing in the clipped area the graphics
;; state should be restored using '(ps:grestore)'.
;; The turtle moves with the text shape clipped.
;;
;; <br/>
;; <center><h2>Global settings</h2></center>
;;
;; Several global variables control fill and line color, line width
;; and other parameters influencing the scaling and orientation of
;; the coordinate system when drawing. Most have default settings
;; when not explicitely specified.
;;
;; @syntax (ps:font <str-name> <num-size>) 
;; @param <str-name> The font name.
;; @param <num-size> The size of the font in points.
;; @return The postscript output.
;; The current font is set for all subsequent text operations.
;; Depending on the version of the Postsrcipt viewer or device
;; installed different fonts are available.
;;
;; @syntax (ps:translate <x> <y>) 
;; @syntax (ps:translate) 
;; @param <x> The new x position of coordinate origin.
;; @param <y> The new y position of coordinate origin.
;; @return The postscript output.
;; Move the coordinate origin.
;; By default the origin 0,0 is in the bottom left corner
;; of the page. <x> and <y> values extend to the right and top. 
;; When now <x>, <y> values are specified the coordinate origin
;; is moved to the current position of the turtle.
;;
;; @syntax (ps:scale <x> <y>) 
;; @param <x> The new x scale factor.
;; @param <y> The new y scale factor.
;; @return The postscript output.
;; Scale the coordinate space.
;; Scaling factors are 1.0 by default and compress for
;; factors less 1.0 or expand for factors bigger than 1.0.
;; With a scaling factor for x = 2.0 each point position
;; specified would cover the double of horizontal distance
;; on the page. Scaling factors can be saved on the graphics
;; state stack using the function '(ps:gsave)'.
;;
;; @syntax (ps:rotate <deg>) 
;; @param <deg> The degrees of rotation: -360 to 0 to 360.
;; @return The postscript output.
;; Rotate the coordinate space. 
;; The coorinate space is rotated to the right for
;; positive angles and to the left for negative angles.
;; The current rotation angle is 0 by default.
;; The rotation angle is part of the graphics state saved by
;; the '(ps:gsave function)'.
;;
;; @syntax (ps:gsave)
;; @return The postscript output.
;; Saves the current graphics state. The function pushes the 
;; current graphics state on a special stack, from where it
;; can be resored using '(ps:grestore)'.
;;
;; @syntax (ps:grestore)
;; @return The postscript output.
;; Restores the graphics state from the stack.
;;
;; @syntax (ps:line-width <points>)
;; @param <points> The line width in points.
;; @return The postscript output.
;; Sets the line width in points for line drawing and the
;; outlines drawn by shapes and text outlines.
;;
;; @syntax (ps:line-cap <mode>) 
;; @param <mode> The line termination shape mode.
;; @return The postscript output.
;; Sets the line termination shape:
;; <pre>
;;    0 square line at the end
;;    1 semicircular line
;;    2 square line end projecting beyond the end of the line
;;      by half line width
;; </pre>
;;
;; @syntax (ps:line-join <mode>)
;; @param <mode> The line join mode.
;; @return The postscript output.
;; Sets the line join mode:
;; <pre>
;;    0 outer edges of lines mitered together
;;    1 outer edges of lines rounded together
;;    2 for lin-cap with 0 the resulting noth
;;      is filled to produce a chamfered corner
;; </pre>
;;
;; @syntax (ps:line-color <R> <G> <B>) 
;; @param <R> The red color value.
;; @param <G> The green color value.
;; @param <B> The blue color value.

;; @syntax (ps:line-color <str-hex>)
;; @param <str-hex> A hex string specifying the line color.
;; @return The postscript output.
;; Set color for line drawing.
;; Color values assume the following value:
;; <pre>
;;    R - value for red 0.0 to 1.0
;;    G - value for green 0.0 to 1.0
;;    B - value for blue 0.0 to 1.0
;; </pre>
;; In an alternative syntax color values can be specified in a
;; hex string:
;;
;; <str-hex> is a hex string constant '"000000"' to '"FFFFFF"'
;; Colors are specified as usual in HTML coding.
;; Each to two hex digits define a color: 'rrggbb'.  
;;
;; @syntax (ps:fill-color <R> <G> <B>) 
;; @param <R> The red color value.
;; @param <G> The green color value.
;; @param <B> The blue color value.

;; @syntax (ps:fill-color <str-hex>)
;; @param <str-hex> A hex string specifying the line color.
;; @return The postscript output.
;; Set color for shape filling.
;; Color values assume the following value:
;; <pre>
;;    R - value for red 0.0 to 1.0
;;    B - value for green 0.0 to 1.0
;;    G - value for blue 0.0 to 1.0
;; </pre>
;; In an alternative syntax color values can be specified in a
;; hex string:
;;
;; <str-hex> is a hex string constant '"000000"' to '"FFFFFF"'
;; Colors are specified as usual in HTML coding.
;; Each two hex digits define a color: 'rrggbb'.  
;;
;;
;; @syntax (ps:render) 
;; @return The postscript output.
;; Show on monitor (Mac OS X only)
;; Uses the Mac OS X Preview.app to convert
;; and view postscript files ending in '.ps'.
;;
;; On Unix and Win32 systems use (ps:save <filename>)
;; to save the postscript file and convert and view
;; it using ghostscript from @link http://www.ghostscript.com/ www.ghostscript.com/
;; and @link http://www.cs.wisc.edu/~ghost/ www.cs.wisc.edu/~ghost .
;;
;; @syntax (ps:save <str-filename>) 
;; @param <str-filename> The filename. 
;; @return The postscript output.
;; Save to <str-filename>.
;; The filename should end in '.ps' to be recognized as a Postscript file 
;; on Mac OS X, where it can be viewed with the standrad Preview.app, by
;; double clicking the filename. On Linux/UNIX Ghostsript can be used to 
;; convert the file to any image format or to view the file.
;; The Quality of display depends on the underlying OS and hardware.

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

% shapes are close and do not change the turtle position

% radius sides fillflag -
/polygon
  {
  /fillflag exch def
  360 exch div /orientinc exch def
  /radius exch def
  gsave
  xpos ypos translate
  orient neg rotate
  0 sin radius mul
  0 cos radius mul moveto
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

(define (ps:ps str)
	(write-line str buffer))
	
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
		
(define (textarc radius str)
	(ps (format "%g (%s) textarc" radius str)))
	
(define (textarcoutline radius str flag)
	(set 'flag (if flag "true" "false"))
	(ps (format "%g (%s) %s textarcoutline" radius str flag)))
	
; rendering and saving

(define (render)
	(write-file "noname.ps" (append prolog buffer "showpage" line-feed))
	(exec "open noname.ps"))
	
(define (clear)
	(set 'buffer ""))
	
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
	(ps (format "%g setlinecap" mode)))
	
(define (line-join mode)
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




	



