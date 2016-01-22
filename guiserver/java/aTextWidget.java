//
//  aTextWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 6/14/07.
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
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.text.*;
import java.io.UnsupportedEncodingException;

public class aTextWidget extends gsObject {

JTextComponent textcomp;
JScrollPane areaScrollPane;
boolean isScrollable;
String findTextAction;
	
public void setText(StringTokenizer tokens)
	{
	String text = tokens.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);

	textcomp.setText(text);
	}

	
public void appendText(StringTokenizer tokens)
	{
	String text = tokens.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);

	String oldtext = textcomp.getText();
	int len = text.length() + oldtext.length();
	if(len > 100000)
			textcomp.setText((oldtext + text).substring(len - 100000));
	else
			textcomp.setText(oldtext + text);

	if(isScrollable) scrollUp();
	textcomp.repaint();
	}
	
public void scrollUp()
    {
	// work around for Java bug #4201999
	try {Thread.sleep(100); } catch (InterruptedException ie) {}
	//textArea.setCaretPosition(textArea.getText().length() - 1);
	JScrollBar vbar = areaScrollPane.getVerticalScrollBar();
    vbar.setValue(vbar.getMaximum());
    }
	
public void gotoText(StringTokenizer params)
	{
	int line = Integer.parseInt(params.nextToken());
	int column = Integer.parseInt(params.nextToken());
	
	if(line < 1) line = 1;
	line--;
	
	String text = textcomp.getText();
	int len = text.length();
	int pos = 0;
	int col;

	
	for(int idx = 0; idx < line; idx++)
		{
		pos = text.indexOf("\n", pos) + 1;
		if(pos >= len) break;
		}
		
	for(col = 0; col < column; col++)
		{
		if(pos + col >= len) break;
		if(text.charAt(pos + col) == '\n') break;
		}
		
	textcomp.setCaretPosition(pos + col);
	}

public void clearText(StringTokenizer params)
	{
	textcomp.setText("");
	}
	
public void getText(StringTokenizer params)
	{
	String text = textcomp.getText();
	if(text == null) text = "";
	
	if(guiserver.UTF8 && text.length() != 0)
		text = Base64Coder.encodeStringUTF8(text);
	else
		text = Base64Coder.encodeString(text);
	
	if(params.hasMoreTokens())
		{
		String action = params.nextToken();
		if(text.length() == 0)
			guiserver.out.println("(" + action + " \"" + id + "\")");
		else
			guiserver.out.println("(" + action + " \"" + id + "\" [text]" + text + "[/text])");
		}
	else
		{
		if(text.length() == 0)
			guiserver.out.println("(set 'gs:text {})");
		else
			guiserver.out.println("(set 'gs:text (base64-dec [text]" + text + "[/text]))");
		}
		
	guiserver.out.flush();
	}
		
public void getSelection(StringTokenizer params)
	{
	String action = params.nextToken();
	String text = textcomp.getSelectedText();
	if(text == null) text = "";

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
	
public void getTextPosition(StringTokenizer tokens)
	{
	String text = textcomp.getText();
	int caret = textcomp.getCaretPosition();
	int line = 1;
	int col = 1;
	
	for(int i= 0; i < caret; i++)
		{
		if(text.charAt(i) == '\n') 
			{
			col = 0;
			++line;
			}
		++col;
		}
		
	guiserver.out.println("(set 'gs:text-position '(" + line + " " + col + "))");
	guiserver.out.flush();
	}	
	
	
public void setEditable(StringTokenizer tokens)
	{
	if(tokens.nextToken().equals("true"))
		textcomp.setEditable(true);
	else
		textcomp.setEditable(false);
	}
	
public void setCaret(StringTokenizer tokens)
	{
	int pos = Integer.parseInt(tokens.nextToken());
	
	if(pos > textcomp.getText().length())
		pos = textcomp.getText().length();
		
	textcomp.setCaretPosition(pos);
	textcomp.moveCaretPosition(pos);
	}
	
public void selectText(StringTokenizer tokens)
	{
	int toPos;
	
	textcomp.setCaretPosition(Integer.parseInt(tokens.nextToken()));
	if(tokens.hasMoreTokens())
		toPos = Integer.parseInt(tokens.nextToken());
	else
		toPos = textcomp.getText().length();
		
	textcomp.moveCaretPosition(toPos);
	}
	
public void copyText(StringTokenizer tokens)
	{
	textcomp.copy();
	}

public void cutText(StringTokenizer tokens)
	{
	textcomp.cut();
	}
	
public void pasteText(StringTokenizer tokens)
	{
	if(tokens.hasMoreTokens())
		{
		String text = tokens.nextToken();
	
		if(guiserver.UTF8)
			text = Base64Coder.decodeStringUTF8(text);
		else
			text = Base64Coder.decodeString(text);

		textcomp.replaceSelection(text);
		}
	else
		textcomp.paste();
	}

	
public void insertText(StringTokenizer tokens)
	{
	String text = tokens.nextToken();

	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);

	String oldtext = textcomp.getText();
	int newpos = Integer.parseInt(tokens.nextToken());
	if(newpos > textcomp.getText().length()) newpos = textcomp.getText().length();
	textcomp.setText(oldtext.substring(0, newpos) + text + oldtext.substring(newpos));
	textcomp.update(textcomp.getGraphics());
	}

public void requestFocus(StringTokenizer tokens)
	{
	textcomp.requestFocus();
	}

public void setFont(StringTokenizer tokens)
	{
	int style = 0;
	int size = 12;
	String name = tokens.nextToken();

	if(guiserver.UTF8)
		name = Base64Coder.decodeStringUTF8(name);
	else
		name = Base64Coder.decodeString(name);

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
	textcomp.setFont(font);
	}


public void registerMouseEvent(StringTokenizer tokens)
	{
	mouseEvent = tokens.nextToken();
	jcomponent.addMouseListener(new MouseAdapter () {
		public void mouseClicked(MouseEvent e) { mouseEventOut(e, "clicked"); }	
		public void mousePressed(MouseEvent e) { mouseEventOut(e, "pressed"); }
		public void mouseReleased(MouseEvent e) { mouseEventOut(e, "released"); }
		});
	}
	
private void mouseEventOut(MouseEvent e, String type)
	{
	guiserver.out.println("(" + mouseEvent + " \"" + id + "\" \"" + type + "\" " + e.getX() + " " + e.getY() + " " + 
		e.getButton() + " " + e.getClickCount() + " " + e.getModifiers() + ")");
	guiserver.out.flush();
	}
		
public static String removeChar(String str, char ch) 
		{
		String result = "";
		for (int i = 0; i < str.length(); i ++) 
			{
			if (str.charAt(i) != ch) 
				result += str.charAt(i);
			}
		
		return result;
		}
		
public void setCaretColor(StringTokenizer tokens)
	{
	Float red = Float.parseFloat(tokens.nextToken());
	Float green = Float.parseFloat(tokens.nextToken());
	Float blue = Float.parseFloat(tokens.nextToken());
		
	textcomp.setCaretColor(new Color(red, green, blue));
	}


public void setSelectionColor(StringTokenizer tokens)
	{
	Float red = Float.parseFloat(tokens.nextToken());
	Float green = Float.parseFloat(tokens.nextToken());
	Float blue = Float.parseFloat(tokens.nextToken());
	Float alpha = (float)1.0;
	
	if(tokens.hasMoreTokens())
		alpha = Float.parseFloat(tokens.nextToken());
		
	textcomp.setSelectionColor(new Color(red, green, blue, alpha));
	}

}
 
 
// eof //
