//
//  DialogWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 5/23/07.
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
import javax.swing.*;
import java.util.*;
import java.io.UnsupportedEncodingException;

// DO NOT USE, when set modal inhibits Dispather from processing incoming messages
// this makes it imposible to serve newLISP requests from the Dialog, if modal

@SuppressWarnings("unchecked")

public class DialogWidget extends gsObject {

JDialog jdialog;
JFrame jfowner;
String dialogCloseAction = null;
String dialogMoveAction = null;
String dialogResizeAction = null;

public DialogWidget(StringTokenizer params)
	{
	id = params.nextToken();
	String owner = params.nextToken();
 
	String message = params.nextToken();
	
	if(guiserver.UTF8)
		message = Base64Coder.decodeStringUTF8(message);
	else
		message = Base64Coder.decodeString(message);

	boolean ismodal = false;
	
	jfowner = ((WindowFrame)gsObject.widgets.get(owner)).jframe;
		
	//String closeButtonId = params.nextToken();
	//ButtonWidget closeButton = (ButtonWidget)gsObject.widgets.get(closeButtonId);
	
	int x = jfowner.getX() + 100;
	int y = jfowner.getY() + 100;
	int width = Integer.parseInt(params.nextToken());
	int height = Integer.parseInt(params.nextToken());
	
	jdialog	= new JDialog(jfowner, message, false);
	//closeButton.jdialog = jdialog;
		
	jdialog.setBounds(x, y, width, height);
	
	container = jdialog.getContentPane();
	
	if(params.hasMoreTokens())
		{
		if(params.nextToken().equals("true"))
			jdialog.setVisible(true);	
		}
	else jdialog.setVisible(false);
	
	if(params.hasMoreTokens())
		{
		if(params.nextToken().equals("true"))
			jfowner.setEnabled(false);
		}
	
	WindowAdapter listener = new WindowAdapter() {
		public void windowClosing(WindowEvent w)
			{
			//System.out.println("Closing dialog");
			jfowner.setEnabled(true);
			jdialog.dispose();
			if(dialogCloseAction != null)
				{
				guiserver.out.println("(" + dialogCloseAction + "\"" + id + "\")");
				guiserver.out.flush();
				}
			}
		};

 	jdialog.addWindowListener(listener);
	
	ComponentAdapter componentListener = new ComponentAdapter() {
		public void componentResized(ComponentEvent re) 
			{
			if(dialogResizeAction != null)
				{
				guiserver.out.println("(" + dialogResizeAction + "\"" + id + "\"" + 
						jdialog.getWidth() + " " + jdialog.getHeight() +")");
				guiserver.out.flush();
				}
			}
			
		public void componentMoved(ComponentEvent me)
			{
			if(dialogMoveAction != null)
				{
				guiserver.out.println("(" + dialogMoveAction + "\"" + id + "\"" + 
						jdialog.getLocationOnScreen().x + " " + jdialog.getLocationOnScreen().y +")");
				guiserver.out.flush();
				}
			}
		};
		
	jdialog.addComponentListener(componentListener);

	gsObject.widgets.put(id, this);
	
	jdialog.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	}
	
	
public void setText(StringTokenizer tokens)
	{	
	String text = tokens.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);
	
	jdialog.setTitle(text);
	}


public void getText(StringTokenizer tokens)
	{
	String action = tokens.nextToken();
	String text = jdialog.getTitle();
	
	if(guiserver.UTF8 && text.length() != 0)
		text = Base64Coder.encodeStringUTF8(text);
	else
		text = Base64Coder.encodeString(text);

	if(text.length() == 0)
		guiserver.out.println("(" + action + " \"" + id + "\")");
	else
		guiserver.out.println("(" + action + " \"" + id + "\" [text]" + text + "[/text])");
	guiserver.out.flush();
	}
	

public void clearText(StringTokenizer tokens)
	{
	jdialog.setTitle("");
	}


public void setVisible(StringTokenizer tokens)
	{
	if(tokens.nextToken().equals("true"))
		jdialog.setVisible(true);
	else
		jdialog.setVisible(false);
	}

public void setResizable(StringTokenizer tokens)
	{
	if(tokens.nextToken().equals("true"))
		jdialog.setResizable(true);
	else
		jdialog.setResizable(false);
	}

public void dispose(StringTokenizer tokens)
	{
	jfowner.setEnabled(true);
	jdialog.dispose();
	}
	
public void setBackground(StringTokenizer tokens)
	{
	Float red = Float.parseFloat(tokens.nextToken());
	Float green = Float.parseFloat(tokens.nextToken());
	Float blue = Float.parseFloat(tokens.nextToken());
	if(tokens.hasMoreTokens())
		{
		Float alpha = Float.parseFloat(tokens.nextToken());
		jdialog.setBackground(new Color(red, green, blue, alpha));
		}
	else
		jdialog.setBackground(new Color(red, green, blue));
	}
	
public void setFont(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void setToolTip(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void getBounds(StringTokenizer tokens)
	{
	int x, y, width, height;
	
	x = jdialog.getLocationOnScreen().x;
	y = jdialog.getLocationOnScreen().y;
		
	width = jdialog.getWidth();
	height = jdialog.getHeight();
	
	guiserver.out.println("( set 'gs:bounds '(" + x + " " + y + " " + width + " " + height + "))\n");
	guiserver.out.flush();
	}

	
public void frameCloseEvent(StringTokenizer tokens)
	{
	dialogCloseAction = tokens.nextToken();
	}
	
public void frameResizeEvent(StringTokenizer tokens)
	{
	dialogResizeAction = tokens.nextToken();
	}
	
public void frameMoveEvent(StringTokenizer tokens)
	{
	dialogMoveAction = tokens.nextToken();
	}
	
}
 
 
// eof //
