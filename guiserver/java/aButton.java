//
//  aButton.java  
//  guiserver
//
//  Created by Lutz Mueller on 5/25/07.
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
import javax.swing.*;
import java.util.*;
import java.io.UnsupportedEncodingException;

public class aButton extends gsObject {

AbstractButton abutton;

public void setText(StringTokenizer tokens)
	{
	String text = tokens.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);

	abutton.setText(text);
	}
	
public void appendText(StringTokenizer tokens)
	{
	String text = tokens.nextToken();

	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);

	String oldtext = abutton.getText();
	text = oldtext + text;

	abutton.setText(text);
	}

public void getText(StringTokenizer params)
	{
	String action = params.nextToken();
	String text = abutton.getText();

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
	abutton.setText("");
	}
	
public void setIcon(StringTokenizer tokens)
	{
	String path = Base64Coder.decodeString(tokens.nextToken());
	abutton.setIcon(guiserver.getIconFromPath(path, this.getClass()));
	}
	
public void setPressedIcon(StringTokenizer tokens)
	{	
	String path = Base64Coder.decodeString(tokens.nextToken());
	abutton.setPressedIcon(guiserver.getIconFromPath(path, this.getClass()));
	}
	
public void setSelected(StringTokenizer tokens)
	{
	String target;
	aButton thebutton;
	
	abutton.setSelected(tokens.nextToken().equals("true"));
	
	while(tokens.hasMoreTokens())
		{
		target = tokens.nextToken();
		thebutton = (aButton)gsObject.widgets.get(target);
		thebutton.abutton.setSelected(tokens.nextToken().equals("true"));
		}
	}

}
 
 
// eof //
