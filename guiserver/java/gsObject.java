//
//  gsObject.java
//  guiserver
//
//  Created by Lutz Mueller on 5/19/07.
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


import java.lang.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.font.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.border.*;
import java.util.*;
import java.io.UnsupportedEncodingException;

public class gsObject {

static HashMap widgets = new HashMap();

String id;
String action;
JComponent jcomponent = null;
Container container = null;
Component component = null;
Dispatcher dispatch = null;
String mouseEvent = null;
String keyEvent = null;

public void addTo(StringTokenizer tokens)
	{
	boolean isBorderLayout = (container.getLayout() instanceof LayoutBorder);
	while(tokens.hasMoreTokens())
		{
		String comp = tokens.nextToken();
		//System.out.println("adding: " + comp + " to: " + id);
		gsObject gsobject = (gsObject)gsObject.widgets.get(comp);
		Component compnt = gsobject.component;
		if(isBorderLayout)
			{
			String direction = tokens.nextToken();
			if(direction.equals("north")) container.add(compnt, BorderLayout.NORTH);
			else if(direction.equals("west")) container.add(compnt, BorderLayout.WEST);
			else if(direction.equals("center")) container.add(compnt, BorderLayout.CENTER);
			else if(direction.equals("east")) container.add(compnt, BorderLayout.EAST);
			else if(direction.equals("south")) container.add(compnt, BorderLayout.SOUTH);
			else container.add(compnt, BorderLayout.CENTER);
			}
		else container.add(compnt);
		}
	}
	
public void removeFrom(StringTokenizer tokens)
	{
	String comp = tokens.nextToken();
	gsObject gsobject = (gsObject)gsObject.widgets.get(comp);
	Component compnt = gsobject.component;
	container.remove(compnt);
	container.update(container.getGraphics());
	}
	
public void layout(StringTokenizer tokens)
	{
	container.validate();
	}
	
public void getBounds(StringTokenizer tokens)
	{
	int x, y, width, height;
	
	x = component.getX();
	y = component.getY();
		
	width = component.getWidth();
	height = component.getHeight();
	
	guiserver.out.println("( set 'gs:bounds '(" + x + " " + y + " " + width + " " + height + "))\n");
	guiserver.out.flush();
	}
	
	
public void loadText(StringTokenizer tokens)
	{
	}
	
public void saveText(StringTokenizer tokens)
	{
	}
	
public void setText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void appendText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void getText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void getSelection(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void clearText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void setCaret(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void selectText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void copyText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void cutText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void pasteText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void undoText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void undoEnable(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void redoText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void clearList(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void setVisible(StringTokenizer tokens)
	{
	if(tokens.nextToken().equals("true"))
		component.setVisible(true);
	else
		component.setVisible(false);
	}


public void setResizable(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void setSize(StringTokenizer tokens)
	{
	int width = Integer.parseInt(tokens.nextToken());
	int height = Integer.parseInt(tokens.nextToken());
	component.setSize(new Dimension(width, height));
	}
	
public void setPreferredSize(StringTokenizer tokens)
	{
	int width = Integer.parseInt(tokens.nextToken());
	int height = Integer.parseInt(tokens.nextToken());
	component.setPreferredSize(new Dimension(width, height));
	}
	
public void Enable(StringTokenizer tokens)
	{
	gsObject gsobject;
	
	component.setEnabled(true);
	
	while(tokens.hasMoreTokens())
		{
		gsobject = (gsObject)gsObject.widgets.get(tokens.nextToken());
		gsobject.component.setEnabled(true);
		}
	}
	
public void Disable(StringTokenizer tokens)
	{
	gsObject gsobject;
	
	component.setEnabled(false);

	while(tokens.hasMoreTokens())
		{
		gsobject = (gsObject)gsObject.widgets.get(tokens.nextToken());
		gsobject.component.setEnabled(false);
		}
	}
	
public void setToolTip(StringTokenizer tokens)
	{
	String text = tokens.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);

	jcomponent.setToolTipText(text);
	}
	
public void setEchoChar(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void setEditable(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void setValue(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void setSelected(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void setTitledBorder(StringTokenizer tokens)
	{
	String text = tokens.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);
		
	jcomponent.setBorder(BorderFactory.createTitledBorder(text));
	}
	
public void setBevelBorder(StringTokenizer tokens)
	{
	int type = BevelBorder.RAISED;
	
	if(tokens.hasMoreTokens())
		{
		String stype = tokens.nextToken();		
		if(stype.equals("lowered")) type = BevelBorder.LOWERED;
		}
	
	jcomponent.setBorder(BorderFactory.createBevelBorder(type));
	}
	
public void setIcon(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void setPressedIcon(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void messageDialog(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void confirmationDialog(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void ColorChooser(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void OpenFileChooser(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void SaveFileChooser(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void requestFocus(StringTokenizer tokens)
	{
	component.requestFocus();
	}
	
public void setFont(StringTokenizer tokens)
	{
	String name = tokens.nextToken();
	
	if(guiserver.UTF8)
		name = Base64Coder.decodeStringUTF8(name);
	else
		name = Base64Coder.decodeString(name);

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
		
	Font font = new Font(name, style, size);
	jcomponent.setFont(font);
	}
	
public void getFontMetrics(StringTokenizer tokens)
	{
	String text = tokens.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);
	
	FontMetrics fm = component.getFontMetrics(component.getFont());
	
	int width = fm.stringWidth(text);
	int height = fm.getHeight();
	
	//System.out.println("width:" + width + " height:" + height);
	
	guiserver.out.println("(set 'gs:font-metrics '(" + width + " " + height + "))");
	guiserver.out.flush();
	}
	
public void setForeground(StringTokenizer tokens)
	{
	Float red = Float.parseFloat(tokens.nextToken());
	Float green = Float.parseFloat(tokens.nextToken());
	Float blue = Float.parseFloat(tokens.nextToken());
	if(tokens.hasMoreTokens())
		{
		Float alpha = Float.parseFloat(tokens.nextToken());
		jcomponent.setForeground(new Color(red, green, blue, alpha));
		}
	else
		jcomponent.setForeground(new Color(red, green, blue));
	}
	
public void setBackground(StringTokenizer tokens)
	{
	Float red = Float.parseFloat(tokens.nextToken());
	Float green = Float.parseFloat(tokens.nextToken());
	Float blue = Float.parseFloat(tokens.nextToken());
	if(tokens.hasMoreTokens())
		{
		Float alpha = Float.parseFloat(tokens.nextToken());
		jcomponent.setBackground(new Color(red, green, blue, alpha));
		}
	else
		jcomponent.setBackground(new Color(red, green, blue));
	}

public void setCaretColor(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}


public void setSyntax(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void setCursor(StringTokenizer tokens)
	{
	String shape = tokens.nextToken();
	Cursor cursor = null;
	
	if(shape.equals("crosshair")) cursor = new Cursor(Cursor.CROSSHAIR_CURSOR);
	else if(shape.equals("text")) cursor = new Cursor(Cursor.TEXT_CURSOR);
	else if(shape.equals("wait")) cursor = new Cursor(Cursor.WAIT_CURSOR);
	else if(shape.equals("sw-resize")) cursor = new Cursor(Cursor.SW_RESIZE_CURSOR);
	else if(shape.equals("se-resize")) cursor = new Cursor(Cursor.SE_RESIZE_CURSOR);
	else if(shape.equals("nw-resize")) cursor = new Cursor(Cursor.NW_RESIZE_CURSOR);
	else if(shape.equals("ne-resize")) cursor = new Cursor(Cursor.NE_RESIZE_CURSOR);
	else if(shape.equals("n-resize")) cursor = new Cursor(Cursor.N_RESIZE_CURSOR);
	else if(shape.equals("s-resize")) cursor = new Cursor(Cursor.S_RESIZE_CURSOR);
	else if(shape.equals("w-resize")) cursor = new Cursor(Cursor.W_RESIZE_CURSOR);
	else if(shape.equals("e-resize")) cursor = new Cursor(Cursor.E_RESIZE_CURSOR);
	else if(shape.equals("hand")) cursor = new Cursor(Cursor.HAND_CURSOR);
	else if(shape.equals("move")) cursor = new Cursor(Cursor.MOVE_CURSOR);
	else cursor = new Cursor(Cursor.DEFAULT_CURSOR);
	
	if(this instanceof WindowFrame || this instanceof WindowWidget)
		component.setCursor(cursor);		
	else
		jcomponent.setCursor(cursor);
	}
	
	
public void dispose(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}


public void insertTab(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

	
public void removeTab(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void setAccelerator(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void addListItem(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void removeListItem(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void selectListItem(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void addSeparator(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void insertListItem(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void setTabSize(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void setTrace(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void setUTF8(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void setLookAndFeel(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void disposeSplash(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void getProperties(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void getScreen(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void getFonts(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void getVersion(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void getTextPosition(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void showPopup(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void findText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void gotoText(StringTokenizer params)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void frameCloseEvent(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void frameResizeEvent(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void frameMoveEvent(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
// 2D Graphics

public void gsCanvas(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void g2Background(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void g2Font(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void g2Stroke(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void g2Paint(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void g2Export(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void g2Print(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void g2Translation(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void g2Scale(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}		
	
public void g2Transform(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void g2Rotation(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	

public void g2Composite(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void g2Clip(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}	
	
public void createLine(StringTokenizer tokens)
	{	
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void drawLine(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void drawRectangle(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void fillRectangle(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void drawRoundRect(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void fillRoundRect(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void drawEllipse(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void fillEllipse(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void drawCircle(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void fillCircle(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void drawArc(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void fillArc(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void drawPolygon(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void fillPolygon(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void drawPath(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void drawText(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void registerMouseEvent(StringTokenizer tokens)
	{
	mouseEvent = tokens.nextToken();
	component.addMouseListener(new MouseAdapter () {
		public void mouseClicked(MouseEvent e) { mouseEventOut(e, "clicked"); }	
		public void mousePressed(MouseEvent e) { mouseEventOut(e, "pressed"); }
		public void mouseReleased(MouseEvent e) { mouseEventOut(e, "released"); }
		});
	}
	
public void registerKeyEvent(StringTokenizer tokens)
	{
	keyEvent = tokens.nextToken();
	component.addKeyListener(new KeyAdapter () {
		public void keyTyped(KeyEvent e) { keyEventOut(e, "typed"); }
		public void keyPressed(KeyEvent e) { keyEventOut(e, "pressed"); }
		public void keyReleased(KeyEvent e) { keyEventOut(e, "released"); }
		});
	}
	
public void registerMouseEntered(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void registerMouseExited(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void registerMousePressed(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void registerMouseReleased(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void registerMouseClicked(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void registerMouseDragged(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void registerMouseMoved(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void registerMouseWheelMoved(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void setAntiAliasing(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void deleteTaggedShape(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void moveTaggedShape(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void	translateTaggedShape(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void rotateTaggedShape(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void scaleTaggedShape(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void shearTaggedShape(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void hideTaggedShape(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void orderTaggedShapes(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void showTaggedShape(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void colorTaggedShape(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void update(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void drawImage(StringTokenizer tokens) 
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

private void mouseEventOut(MouseEvent e, String type)
	{
	guiserver.out.println("(" + mouseEvent + " \"" + id + "\" \"" + type + "\" " + e.getX() + " " + e.getY() + " " + 
		e.getButton() + " " + e.getClickCount() + " " + e.getModifiers() + ")");
	guiserver.out.flush();
	}
	
private void keyEventOut(KeyEvent e, String type)
	{
	guiserver.out.println("(" + keyEvent + " \"" + id + "\" \"" + type + "\" " + 
			e.getKeyCode() + " " + e.getModifiers() + ")");
	guiserver.out.flush();
	}
	
public void runShell(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void evalShell(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void destroyShell(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

}

// eof
 
 
// eof //
