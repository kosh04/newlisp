//
//  WindowFrame.java
//  guiserver
//
//  Created by Lutz Mueller on 5/11/07.
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
import java.io.*;

@SuppressWarnings("unchecked")
public class WindowFrame extends gsObject  {

JFrame jframe;
String frameCloseAction = null;
String frameMoveAction = null;
String frameResizeAction = null;


public WindowFrame(StringTokenizer params)
	{
	jframe = new JFrame();
	
	id = params.nextToken();
	container = jframe.getContentPane();
	component = container;

	int x = Integer.parseInt(params.nextToken());
	int y = Integer.parseInt(params.nextToken());
	int width = Integer.parseInt(params.nextToken());
	int height = Integer.parseInt(params.nextToken());
	
	gsObject.widgets.put(id, this); 		
	
	jframe.setBounds(x, y, width, height);

	// not visible on OSX, but on Win32 and Linux
	jframe.setIconImage(Toolkit.getDefaultToolkit().getImage(getClass().getResource("/images/newLISP32.png")));
	
	if(params.hasMoreTokens())
		setText(params);
		
	if(params.hasMoreTokens())
		{
		if(params.nextToken().equals("true"))
			jframe.setVisible(true);	
		}
		
		
	WindowAdapter listener = new WindowAdapter() {
		public void windowClosing(WindowEvent w)
			{
			if(frameCloseAction != null)
				{
				jframe.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
				guiserver.out.println("(" + frameCloseAction + "\"" + id + "\")");
				guiserver.out.flush();
				}
			}
		};
		
	jframe.addWindowListener(listener);
	
	
	ComponentAdapter componentListener = new ComponentAdapter() {
		public void componentResized(ComponentEvent re) 
			{
			if(frameResizeAction != null)
				{
				guiserver.out.println("(" + frameResizeAction + "\"" + id + "\"" + 
						jframe.getWidth() + " " + jframe.getHeight() +")");
				guiserver.out.flush();
				}
			}
			
		public void componentMoved(ComponentEvent me)
			{
			if(frameMoveAction != null)
				{
				guiserver.out.println("(" + frameMoveAction + "\"" + id + "\"" + 
						jframe.getLocationOnScreen().x + " " + jframe.getLocationOnScreen().y +")");
				guiserver.out.flush();
				}
			}
		};
		
	jframe.addComponentListener(componentListener);
		
	jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}
	
	
public void setVisible(StringTokenizer tokens)
	{
	//jframe.pack();
	if(tokens.nextToken().equals("true"))
		jframe.setVisible(true);
	else
		jframe.setVisible(false);
	}


public void setText(StringTokenizer tokens)
	{
	String text = tokens.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);

	jframe.setTitle(text);
	}


public void getText(StringTokenizer tokens)
	{
	String action = tokens.nextToken();
	String text = jframe.getTitle();
	
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
	jframe.setTitle("");
	}


public void setResizable(StringTokenizer tokens)
	{
	if(tokens.nextToken().equals("true"))
		jframe.setResizable(true);
	else
		jframe.setResizable(false);
	}
	
	
public void confirmDialog(StringTokenizer tokens)
	{
	String action = tokens.nextToken();
	String title = tokens.nextToken();
	String message = tokens.nextToken(); 

	if(guiserver.UTF8)
		{
		title = Base64Coder.decodeStringUTF8(title);
		message = Base64Coder.decodeStringUTF8(message);
		}
	else
		{
		title = Base64Coder.decodeString(title);
		message = Base64Coder.decodeString(message);
		}
	
	int option = JOptionPane.YES_NO_OPTION;
	
	if(tokens.hasMoreTokens())
		{
		if(!tokens.nextToken().equals("yes-no"))
			option = JOptionPane.YES_NO_CANCEL_OPTION;
		}
			
	int result = JOptionPane.showConfirmDialog(jframe, message, title, option);
	
	guiserver.out.println("(" + action + " \"" + id + "\" " + result + ")");
	guiserver.out.flush();
	}


public void messageDialog(StringTokenizer tokens)
	{
	String title = tokens.nextToken();
	String message = tokens.nextToken(); 

	if(guiserver.UTF8)
		{
		title = Base64Coder.decodeStringUTF8(title);
		message = Base64Coder.decodeStringUTF8(message);
		}
	else
		{
		title = Base64Coder.decodeString(title);
		message = Base64Coder.decodeString(message);
		}
	
	int option = JOptionPane.PLAIN_MESSAGE;
	String path;
	
	if(tokens.hasMoreTokens())
		{
		String soption = tokens.nextToken();
		if(soption.equals("error")) option = JOptionPane.ERROR_MESSAGE;
		if(soption.equals("information")) option = JOptionPane.INFORMATION_MESSAGE;
		if(soption.equals("warning")) option = JOptionPane.WARNING_MESSAGE;
		if(soption.equals("question")) option = JOptionPane.QUESTION_MESSAGE;
		}
		
	if(tokens.hasMoreTokens())
		{
		path = Base64Coder.decodeString(tokens.nextToken());
		JOptionPane.showMessageDialog(jframe, message, title, option, 
				guiserver.getIconFromPath(path, this.getClass()));
		}
	else
		JOptionPane.showMessageDialog(jframe, message, title, option);
	}


public void ColorChooser(StringTokenizer tokens)
	{
	Color color;
	String action = tokens.nextToken();
	String title = tokens.nextToken();
	
	if(guiserver.UTF8)
		title = Base64Coder.decodeStringUTF8(title);
	else
		title = Base64Coder.decodeString(title);
	
	Float red;
	Float green;
	Float blue;
	
	if(tokens.hasMoreTokens()) // get initial color
		{
		red = Float.parseFloat(tokens.nextToken());
		green = Float.parseFloat(tokens.nextToken());
		blue = Float.parseFloat(tokens.nextToken());
		color = new Color(red, green, blue);
		}
	else color = Color.white;
		
	color = JColorChooser.showDialog(jframe, title, color);
	
	if(color == null)
		{
		guiserver.out.println("(" + action + " \"" + id + "\" \"color\")");
		guiserver.out.flush();
		return;
		}
	
	red = (new Integer(color.getRed())).floatValue();
	green = (new Integer(color.getGreen())).floatValue();
	blue = (new Integer(color.getBlue())).floatValue();
	
	red = red/255;
	green = green/255;
	blue = blue/255;
	
	guiserver.out.println("(" + action + " \"" + id + "\" \"color\" " + red +  " " + green + " " + blue + ")");
	guiserver.out.flush();
	}
	
public void OpenFileChooser(StringTokenizer tokens)
	{
	JFileChooser filechooser;
	String file = null;
	
	String action = tokens.nextToken();

	if(tokens.hasMoreTokens())
		filechooser = new JFileChooser(Base64Coder.decodeString(tokens.nextToken()));
	else
		filechooser = new JFileChooser();
		
	filechooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
	
	if(tokens.hasMoreTokens())
		{
		MyFileFilter myfilter = new MyFileFilter();
		myfilter.extensions = Base64Coder.decodeString(tokens.nextToken());
		myfilter.description = Base64Coder.decodeString(tokens.nextToken());
		
		filechooser.setFileFilter(myfilter);
		}

	int selected = filechooser.showOpenDialog(container);
	
	if(selected == JFileChooser.APPROVE_OPTION)
		{
		file = Base64Coder.encodeString(filechooser.getSelectedFile().getAbsolutePath());
		guiserver.out.println("(" + action + " \"" + id + "\" \"open\" " + "\"" + file + "\")");
		}
	else
		{
		guiserver.out.println("(" + action + " \"" + id + "\" \"open\")");
		}
		
	guiserver.out.flush();
	}


public void SaveFileChooser(StringTokenizer tokens)
	{
	JFileChooser filechooser;
	String file = null;
	String initialFile = null;
	
	String action = tokens.nextToken();

	if(tokens.hasMoreTokens()) // initial directory
		filechooser = new JFileChooser(Base64Coder.decodeString(tokens.nextToken()));
	else
		filechooser = new JFileChooser();
		
	if(tokens.hasMoreTokens()) // initial file name
		{
		initialFile = Base64Coder.decodeString(tokens.nextToken());
		filechooser.setSelectedFile(new File(initialFile));
		}
		
		
	if(tokens.hasMoreTokens())
		{
		MyFileFilter myfilter = new MyFileFilter();
		myfilter.extensions = Base64Coder.decodeString(tokens.nextToken());
		myfilter.description = Base64Coder.decodeString(tokens.nextToken());
		filechooser.setFileFilter(myfilter);
		}

	int selected = filechooser.showSaveDialog(container);
	
	if(selected == JFileChooser.APPROVE_OPTION)
		{
		file = Base64Coder.encodeString(filechooser.getSelectedFile().getAbsolutePath());
		guiserver.out.println("(" + action + " \"" + id + "\" \"save\" " + "\"" + file + "\")");
		}
	else
		guiserver.out.println("(" + action + " \"" + id + "\" \"save\")");
		
	guiserver.out.flush();
	}
	
public void setFont(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
		
public void setToolTip(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void dispose(StringTokenizer tokens)
	{
	jframe.dispose();
	}
		
public void setPreferredSize(StringTokenizer tokens)
	{
	int width = Integer.parseInt(tokens.nextToken());
	int height = Integer.parseInt(tokens.nextToken());
	jframe.setSize(new Dimension(width, height));
	}
	
public void setBackground(StringTokenizer tokens)
	{
	Float red = Float.parseFloat(tokens.nextToken());
	Float green = Float.parseFloat(tokens.nextToken());
	Float blue = Float.parseFloat(tokens.nextToken());
	if(tokens.hasMoreTokens())
		{
		Float alpha = Float.parseFloat(tokens.nextToken());
		jframe.setBackground(new Color(red, green, blue, alpha));
		}
	else
		jframe.setBackground(new Color(red, green, blue));
	}
	
public void requestFocus(StringTokenizer tokens)
	{
	jframe.requestFocus();
	}
	
public void frameCloseEvent(StringTokenizer tokens)
	{
	frameCloseAction = tokens.nextToken();
	}
	
public void frameResizeEvent(StringTokenizer tokens)
	{
	frameResizeAction = tokens.nextToken();
	}
	
public void frameMoveEvent(StringTokenizer tokens)
	{
	frameMoveAction = tokens.nextToken();
	}

public void getBounds(StringTokenizer tokens)
	{
	int x, y, width, height;
	
	x = jframe.getLocationOnScreen().x;
	y = jframe.getLocationOnScreen().y;
		
	width = jframe.getWidth();
	height = jframe.getHeight();
	
	guiserver.out.println("( set 'gs:bounds '(" + x + " " + y + " " + width + " " + height + "))\n");
	guiserver.out.flush();
	}

}
 
 
// eof //
