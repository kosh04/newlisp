//
//  CanvasWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 6/8/07.
//
//
//    Copyright (C) 2016 Lutz Mueller
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
//


import java.awt.*;
import java.awt.color.*;
import java.awt.event.*;
import java.awt.geom.*;
import java.awt.print.*;
import java.awt.image.BufferStrategy;
import java.awt.image.BufferedImage;
import javax.swing.*;
import javax.imageio.ImageIO;
import java.util.*;
import java.io.File;
import java.io.IOException;

@SuppressWarnings("unchecked")
public class CanvasWidget extends gsObject {

static CanvasWidget currentCanvas;

Vector drawobjects = new Vector();

MyCanvas canvas;
//Window ownerWindow;
double translationX = 0;
double translationY = 0;
double theta = 0;
double scaleX = 1;
double scaleY = 1;
Color background = null;
Stroke currentStroke;
Color currentPaint;
//AffineTransform transform = AffineTransform.getScaleInstance(1.0, 1.0);
AffineTransform transform = null;
AlphaComposite currentComposite;
Font currentFont = null;
boolean antiAliasing = true;
boolean visible = true;

String mouseClickedEvent;
String mouseEnteredEvent;
String mouseExitedEvent;
String mousePressedEvent;
String mouseReleasedEvent;
String mouseDraggedEvent; 	
String mouseMovedEvent; 	
String mouseWheelMovedEvent;

boolean mousePressedTags = false;
boolean mouseReleasedTags = false;
boolean mouseClickedTags = false;
boolean mouseMovedTags = false;

public CanvasWidget(StringTokenizer params)
	{
	id = params.nextToken();
	canvas = new MyCanvas();

	component = canvas;
	
	// if canvas is not Canvas but JPanel
	container = canvas;
	jcomponent = canvas;

	CanvasWidget.currentCanvas = this;
	
	currentStroke = new BasicStroke();
	currentPaint = new Color(0,0,0);
	
	scaleX = 1.0;
	scaleY = 1.0;
	
	gsObject.widgets.put(id, this);
	}

public void g2Canvas(StringTokenizer tokens)
	{
	CanvasWidget.currentCanvas = this;
	}
	
public void g2Translation(StringTokenizer tokens)
	{
	translationX = Float.parseFloat(tokens.nextToken());
	translationY = Float.parseFloat(tokens.nextToken());
	}	

public void g2Scale(StringTokenizer tokens)
	{
	scaleX = Float.parseFloat(tokens.nextToken());
	scaleY = Float.parseFloat(tokens.nextToken());
	}	
	
public void g2Rotation(StringTokenizer tokens)
	{
	double rotation = Float.parseFloat(tokens.nextToken());
	theta = 2.0 * Math.PI * rotation / 360.0;
	}	
	
public void g2Transform(StringTokenizer tokens)
	{
	double m00 = Float.parseFloat(tokens.nextToken());
	double m01 = Float.parseFloat(tokens.nextToken());
	double m02 = Float.parseFloat(tokens.nextToken());
	double m10 = Float.parseFloat(tokens.nextToken());
	double m11 = Float.parseFloat(tokens.nextToken());
	double m12 = Float.parseFloat(tokens.nextToken());
	
	//transform.concatenate(new AffineTransform(m00, m01, m02, m10, m11, m12));
	}	

public void g2Stroke(StringTokenizer tokens)
	{
	float strokeWidth = Float.parseFloat(tokens.nextToken());
	int strokeCap = BasicStroke.CAP_BUTT;
	int strokeJoin = BasicStroke.JOIN_MITER;
	float strokeMiterLimit = (float)10.0;
	
	if(tokens.hasMoreTokens())
		{
		String cap = tokens.nextToken();
		if(cap.equals("round")) strokeCap = BasicStroke.CAP_ROUND;
		else if(cap.equals("square")) strokeCap = BasicStroke.CAP_SQUARE;
		else strokeCap = BasicStroke.CAP_BUTT;
		}
	
	if(tokens.hasMoreTokens())
		{
		String join = tokens.nextToken();
		if(join.equals("round")) strokeJoin = BasicStroke.JOIN_BEVEL;
		else if(join.equals("square")) strokeJoin = BasicStroke.JOIN_ROUND;
		else strokeJoin = BasicStroke.JOIN_MITER;
		}
		
	if(tokens.hasMoreTokens())
		strokeMiterLimit = Float.parseFloat(tokens.nextToken());

	currentStroke = new BasicStroke(strokeWidth, strokeCap, strokeJoin, strokeMiterLimit);
	}
	
public void g2Paint(StringTokenizer tokens)
	{
	float R, G, B;
	float alpha;
	
	R = Float.parseFloat(tokens.nextToken());
	G = Float.parseFloat(tokens.nextToken());
	B = Float.parseFloat(tokens.nextToken());

	if(tokens.hasMoreTokens())
		alpha = Float.parseFloat(tokens.nextToken());
	else 
		alpha = 1;

	currentPaint = new Color(R, G, B, alpha);
	}
	
public void g2Update(StringTokenizer tokens)
	{
	canvas.repaint();
	}
	
public void g2Export(StringTokenizer tokens)
	{
	String path = Base64Coder.decodeString(tokens.nextToken());
	int width = canvas.getWidth();
	int height = canvas.getHeight();
	
	if(tokens.hasMoreTokens())
		{
		width = Integer.parseInt(tokens.nextToken());
		height = Integer.parseInt(tokens.nextToken());
		}
	
	canvas.export(path, "PNG", width, height);
	}
	
public void g2Print(StringTokenizer tokens)
	{
	}
	
public void setBackground(StringTokenizer tokens)
	{
	float R, G, B;
	float alpha;
	
	R = Float.parseFloat(tokens.nextToken());
	G = Float.parseFloat(tokens.nextToken());
	B = Float.parseFloat(tokens.nextToken());

	if(tokens.hasMoreTokens())
		alpha = Float.parseFloat(tokens.nextToken());
	else 
		alpha = 1;
	
	background = new Color(R, G, B, alpha);
	
	component.setBackground(background);
	}
	
public void setFont(StringTokenizer tokens)
	{
	String name = Base64Coder.decodeString(tokens.nextToken());
	int style = 0;
	int size = 12;
	
	if(tokens.hasMoreTokens())
		size = Integer.parseInt(tokens.nextToken());
	
	while(tokens.hasMoreTokens())
		{
		String sstyle = tokens.nextToken();
		if(sstyle.equals("plain")) style = style | Font.PLAIN;
		if(sstyle.equals("bold")) style = style | Font.BOLD;
		if(sstyle.equals("italic")) style = style | Font.ITALIC;
		}
		
	currentFont = new Font(name, style, size);
	component.setFont(currentFont);
	}
	

public void setAntiAliasing(StringTokenizer tokens) 
	{
	antiAliasing = tokens.nextToken().equals("true");
	}

// create shapes

public void drawLine(StringTokenizer tokens)
	{
	new LineShape(tokens);
	}
	
public void drawRectangle(StringTokenizer tokens)
	{
	new RectangleShape(tokens);
	}

public void fillRectangle(StringTokenizer tokens)
	{
	new FilledRectangleShape(tokens);
	}

public void drawRoundRect(StringTokenizer tokens)
	{
	new RoundRectangleShape(tokens);
	}

public void fillRoundRect(StringTokenizer tokens)
	{
	new FilledRoundRectangleShape(tokens);
	}

public void drawEllipse(StringTokenizer tokens)
	{
	new EllipseShape(tokens);
	}

public void fillEllipse(StringTokenizer tokens)
	{
	new FilledEllipseShape(tokens);
	}

public void drawCircle(StringTokenizer tokens)
	{
	new CircleShape(tokens);
	}

public void fillCircle(StringTokenizer tokens)
	{
	new FilledCircleShape(tokens);
	}

public void drawArc(StringTokenizer tokens)
	{
	new ArcShape(tokens);
	}

public void fillArc(StringTokenizer tokens)
	{
	new FilledArcShape(tokens);
	}

public void drawPolygon(StringTokenizer tokens)
	{
	new PolygonShape(tokens);
	}
	
public void fillPolygon(StringTokenizer tokens)
	{
	new FilledPolygonShape(tokens);
	}

public void drawPath(StringTokenizer tokens)
	{
	new PathShape(tokens);
	}
	
public void drawText(StringTokenizer tokens)
	{
	new TextShape(tokens);
	}

public void drawImage(StringTokenizer tokens)
	{
	new ImageShape(tokens);
	}
	
// register mouse events
	
public void registerMouseClicked(StringTokenizer tokens)
	{
	mouseClickedEvent = tokens.nextToken();
	if(tokens.hasMoreTokens())
		mouseClickedTags = tokens.nextToken().equals("true");
	}
	
public void registerMousePressed(StringTokenizer tokens)
	{
	mousePressedEvent = tokens.nextToken();
	if(tokens.hasMoreTokens())
		mousePressedTags = tokens.nextToken().equals("true");
	}

public void registerMouseRelease(StringTokenizer tokens)
	{
	mouseReleasedEvent = tokens.nextToken();
	if(tokens.hasMoreTokens())
		mouseReleasedTags = tokens.nextToken().equals("true");
	}
	
public void registerMouseExited(StringTokenizer tokens)
	{
	mouseExitedEvent = tokens.nextToken();
	}
	
public void registerMouseEntered(StringTokenizer tokens)
	{
	mouseEnteredEvent = tokens.nextToken();
	}
	
public void registerMouseDragged(StringTokenizer tokens)
	{
	mouseDraggedEvent = tokens.nextToken();
	}
	
public void registerMouseMoved(StringTokenizer tokens)
	{
	mouseMovedEvent = tokens.nextToken();
	if(tokens.hasMoreTokens())
		mouseMovedTags = tokens.nextToken().equals("true");
	}
	
public void registerMouseWheel(StringTokenizer tokens)
	{
	mouseWheelMovedEvent = tokens.nextToken();
	}

// manipulate shapes
	
public void colorTaggedShape(StringTokenizer tokens)
	{
	String label = tokens.nextToken();
	Color paintColor = Shape.getColorParameter(tokens);
	boolean updateFlag = tokens.nextToken().equals("true");
	Shape aShape;
		
	for(int i = 0; i < drawobjects.size(); i++)
		{
		aShape = (Shape)drawobjects.elementAt(i);
		if(aShape.tag.equals(label))
			aShape.paintColor = paintColor;
		}
		
	if(updateFlag) canvas.repaint();
	}
		

public void deleteTaggedShape(StringTokenizer tokens)
	{
	String label = tokens.nextToken();
	boolean updateFlag = tokens.nextToken().equals("true");
	Vector filteredObjects = new Vector();
	Shape aShape;
	
	for(int i = 0; i < drawobjects.size(); i++)
		{
		aShape = (Shape)drawobjects.elementAt(i);
		if(!aShape.tag.equals(label))
			filteredObjects.add(aShape);
		}
		
	drawobjects = filteredObjects;
	if(updateFlag) canvas.repaint();
	}
	
public void moveTaggedShape(StringTokenizer tokens)
	{
	String label = tokens.nextToken();
	int dx = Integer.parseInt(tokens.nextToken());
	int dy = Integer.parseInt(tokens.nextToken());
	boolean updateFlag = tokens.nextToken().equals("true");

	Shape aShape;
	
	for(int i = 0; i < drawobjects.size(); i++)
		{
		aShape = (Shape)drawobjects.elementAt(i);
		if(aShape.tag.equals(label))
			{
			aShape.X += dx;
			aShape.Y += dy;
			}
		}
		
	if(updateFlag) canvas.repaint();
	}

public void translateTaggedShape(StringTokenizer tokens)
	{
	String label = tokens.nextToken();
	double X = Float.parseFloat(tokens.nextToken());
	double Y = Float.parseFloat(tokens.nextToken());
	boolean updateFlag = tokens.nextToken().equals("true");
	Shape aShape;
	
	for(int i = 0; i < drawobjects.size(); i++)
		{
		aShape = (Shape)drawobjects.elementAt(i);
		if(aShape.tag.equals(label))
			aShape.transform.translate(X, Y);
		}
		
	if(updateFlag) canvas.repaint();
	}

	
public void rotateTaggedShape(StringTokenizer tokens)
	{
	String label = tokens.nextToken();
	double theta = Float.parseFloat(tokens.nextToken());
	int X = Integer.parseInt(tokens.nextToken());
	int Y = Integer.parseInt(tokens.nextToken());
	boolean updateFlag = tokens.nextToken().equals("true");
	Shape aShape;
	
	theta = 2.0 * Math.PI * theta / 360.0;
	
	for(int i = 0; i < drawobjects.size(); i++)
		{
		aShape = (Shape)drawobjects.elementAt(i);
		if(aShape.tag.equals(label))
			aShape.transform.rotate(theta, X, Y);
		}
		
	if(updateFlag) canvas.repaint();
	}
	
public void scaleTaggedShape(StringTokenizer tokens)
	{
	String label = tokens.nextToken();
	double sx = Float.parseFloat(tokens.nextToken());
	double sy = Float.parseFloat(tokens.nextToken());
	boolean updateFlag = tokens.nextToken().equals("true");
	Shape aShape;
	
	for(int i = 0; i < drawobjects.size(); i++)
		{
		aShape = (Shape)drawobjects.elementAt(i);
		if(aShape.tag.equals(label))
			aShape.transform.scale(sx, sy);
		}
		
	if(updateFlag) canvas.repaint();
	}

public void shearTaggedShape(StringTokenizer tokens)
	{
	String label = tokens.nextToken();
	double shx = Float.parseFloat(tokens.nextToken());
	double shy = Float.parseFloat(tokens.nextToken());
	boolean updateFlag = tokens.nextToken().equals("true");
	Shape aShape;
	
	for(int i = 0; i < drawobjects.size(); i++)
		{
		aShape = (Shape)drawobjects.elementAt(i);
		if(aShape.tag.equals(label))
			aShape.transform.shear(shx, shy);
		}
		
	if(updateFlag) canvas.repaint();
	}
			
public void hideTaggedShape(StringTokenizer tokens)
	{
	String label = tokens.nextToken();
	boolean updateFlag = tokens.nextToken().equals("true");
	Shape aShape;
	
	for(int i = 0; i < drawobjects.size(); i++)
		{
		aShape = (Shape)drawobjects.elementAt(i);
		if(aShape.tag.equals(label))
			aShape.visible = false;
		}
		
	if(updateFlag) canvas.repaint();
	}
	
	
public void showTaggedShape(StringTokenizer tokens)
	{
	String label = tokens.nextToken();
	boolean updateFlag = tokens.nextToken().equals("true");
	Shape aShape;
	
	for(int i = 0; i < drawobjects.size(); i++)
		{
		aShape = (Shape)drawobjects.elementAt(i);
		if(aShape.tag.equals(label))
			aShape.visible = true;
		}
		
	if(updateFlag) canvas.repaint();
	}


public void orderTaggedShapes(StringTokenizer tokens)
	{
	String label;
	Shape aShape;
	
	Vector orderedTags = new Vector();
	
	while(tokens.hasMoreTokens())
		{
		label = tokens.nextToken();
		for(int i = 0; i < drawobjects.size(); i++)
			{
			aShape = (Shape)drawobjects.elementAt(i);
//			System.out.println("comparing:" + label + "<->" + aShape.tag);
			if(aShape.tag.equals(label))
				{
				orderedTags.add(aShape);
//				System.out.println("adding->" + aShape.tag);
				}
			}
		}
		
	drawobjects = orderedTags;
	}

class MyCanvas extends JPanel implements MouseListener, MouseMotionListener, MouseWheelListener, Printable  {
public static final long serialVersionUID = 1L;
	public MyCanvas() {
		super();
		
		// if canvas extends JPanel
		if(guiserver.DOUBLE_BUFFERING)
			this.setDoubleBuffered(true);
		
		addMouseListener(this);
		addMouseMotionListener(this);
		addMouseWheelListener(this);
		}

	public void update(Graphics g) 
		{ 
		if(guiserver.MAC_OS_X) super.update(g);
		else paint(g); 
		} 
 
	public void paint(Graphics g)
		{
		Graphics2D g2;
//		BufferStrategy myStrategy = null; 
		
/* only if canvas extends Canvas
		if(guiserver.DOUBLE_BUFFERING)
			{
			try {
				this.createBufferStrategy(2);
				}	
			catch (Exception evt) 
				{
				System.out.println("event: =>" + evt);
				}
	
			
			myStrategy = this.getBufferStrategy();
			Graphics myg = myStrategy.getDrawGraphics();
			g2 = (Graphics2D)myg;
			}
		else
*/
			g2 = (Graphics2D)g;
		
		render(g2);
			
		/* only used if canvas is Canvas
		if(guiserver.DOUBLE_BUFFERING)
			{
			myStrategy.show();
			g2.dispose();
			}
		*/
		}
		
		
	public void render(Graphics2D g2)
		{
		if(background != null) g2.setColor(background);
		
		// fill background
		if(background != null)
			{
			g2.setPaint(background);
			g2.fillRect(0, 0, canvas.getWidth(), canvas.getHeight());
			}
				
		g2.translate(translationX, translationY);
		g2.scale(scaleX, scaleY);
		g2.rotate(theta);
		
		AffineTransform normalTransform = g2.getTransform();
		
		if(antiAliasing)
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                            RenderingHints.VALUE_ANTIALIAS_ON);
		
		for(int i = 0; i < drawobjects.size(); i++)
			{
			Shape shape = (Shape)drawobjects.elementAt(i);

			if(shape.visible) 
				{
				g2.transform(shape.transform);
				shape.drawShape(g2);
				g2.setTransform(normalTransform);
				}
			}
		
		paintComponents(g2);
		}
		
	public Iterator getTagsFromPoint(int X, int Y)
		{
		HashMap tags = new HashMap();
		for(int i = 0; i < drawobjects.size(); i++)
			{
			Shape shape = (Shape)drawobjects.elementAt(i);
			if(shape.hasPoint(X, Y))
				{
				if(!tags.containsKey(shape.tag))
					tags.put(shape.tag, 0);
				}
			}
			
		return tags.keySet().iterator();
		}
		
	public void mouseClicked(MouseEvent e) 
		{
		if(mouseClickedEvent == null) return;
		
		int X = e.getX();
		int Y = e.getY();
		
		guiserver.out.print("(" + mouseClickedEvent + " " + X + " " + Y + " " + 
			e.getButton() + " " + e.getClickCount() + " " + e.getModifiers());
		
		if(mouseClickedTags)
			{
			guiserver.out.print(" '(");
			Iterator tags = getTagsFromPoint(X, Y);
			while(tags.hasNext())
				guiserver.out.print("\"" + tags.next() + "\" ");
			guiserver.out.print(")");
			}
			
		guiserver.out.println(")");	
		guiserver.out.flush();
		}
	
	public void mouseEntered(MouseEvent e)
		{
		if(mouseEnteredEvent == null) return;
		}
	
	public void mouseExited(MouseEvent e) 
		{
		if(mouseExitedEvent == null) return;
		}
	
	public void mousePressed(MouseEvent e)
		{
		if(mousePressedEvent == null) return;
			
		int X = e.getX();
		int Y = e.getY();
		
		guiserver.out.print("(" + mousePressedEvent + " " + X + " " + Y + " " + 
			e.getButton() + " " + e.getModifiers());
		
		if(mousePressedTags)
			{
			guiserver.out.print(" '(");
			Iterator tags = getTagsFromPoint(X, Y);
			while(tags.hasNext())
				guiserver.out.print("\"" + tags.next() + "\" ");
			guiserver.out.print(")");
			}
			
		guiserver.out.println(")");	
		guiserver.out.flush();
		}
	
	public void mouseReleased(MouseEvent e)
		{
		if(mouseReleasedEvent == null) return;
				
		int X = e.getX();
		int Y = e.getY();
		
		guiserver.out.print("(" + mouseReleasedEvent + " " + X + " " + Y + " " + 
			e.getButton() + " " + e.getModifiers());
		
		if(mouseReleasedTags)
			{
			guiserver.out.print(" '(");
			Iterator tags = getTagsFromPoint(X, Y);
			while(tags.hasNext())
				guiserver.out.print("\"" + tags.next() + "\" ");
			guiserver.out.print(")");
			}
			
		guiserver.out.println(")");	
		guiserver.out.flush();
		}
	
	public void mouseDragged(MouseEvent e) 
		{
		if(mouseDraggedEvent == null) return;
			
		int X = e.getX();
		int Y = e.getY();
		
		guiserver.out.println("(" + mouseDraggedEvent + " " + X + " " + Y + " " + 
				e.getButton() + " " + e.getModifiers() + ")" );
		guiserver.out.flush();
		}

	public void mouseMoved(MouseEvent e) 
		{
		if(mouseMovedEvent == null) return;
			
		int X = e.getX();
		int Y = e.getY();
		
		guiserver.out.print("(" + mouseMovedEvent + " " + X + " " + Y);
		
		if(mouseMovedTags)
			{
			guiserver.out.print(" '(");
			Iterator tags = getTagsFromPoint(X, Y);
			while(tags.hasNext())
				guiserver.out.print("\"" + tags.next() + "\" ");
			guiserver.out.print(")");
			}
		
		guiserver.out.println(")");	
		guiserver.out.flush();
		}
	
	public void mouseWheelMoved(MouseWheelEvent e) 
		{
		if(mouseWheelMovedEvent == null) return;
		
		int rotation = e.getWheelRotation();
		int X = e.getX();
		int Y = e.getY();
		
		guiserver.out.println("(" + mouseWheelMovedEvent + " " + X + " " + Y + " " + rotation + ")" );
		guiserver.out.flush();
		}
		
	public void export(String path, String format, int width, int height)
		{
		//BufferedImage bufferimage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
		BufferedImage bufferimage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
		
		Graphics2D g2 = bufferimage.createGraphics();
		
		render(g2);
		
		try { ImageIO.write(bufferimage, format, new File(path)); } 
		catch (IOException ie) {
				ErrorDialog.show("Exporting", "Could not write file: " + path);
				}
		}
	
	public int print(Graphics graphics, PageFormat pageFormat, int pageIndex) 
		{
		return(0);
		}
	}
}
 
 
// eof //
