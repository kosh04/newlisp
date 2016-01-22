//
//  MenuItemCheckWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 5/24/07.
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
import javax.swing.*;
import java.awt.event.*;
import java.util.*;
import java.io.UnsupportedEncodingException;

public class MenuItemCheckWidget extends gsObject{

JCheckBoxMenuItem cbmenuitem;

@SuppressWarnings("unchecked") 
public MenuItemCheckWidget(StringTokenizer params)
	{
	cbmenuitem = new JCheckBoxMenuItem();
	jcomponent = cbmenuitem;
	component = cbmenuitem;
	container = cbmenuitem;
	
	id = params.nextToken();
	action = params.nextToken();
	
	if(params.hasMoreTokens())
		{
		String mstring = params.nextToken();
	
		if(guiserver.UTF8)
			mstring = Base64Coder.decodeStringUTF8(mstring);
		else
			mstring = Base64Coder.decodeString(mstring);

		cbmenuitem.setText(mstring);
		}
	
	if(params.hasMoreTokens())
		{
		if(params.nextToken().equals("true"))
			cbmenuitem.setSelected(true);
		}
	
	gsObject.widgets.put(id, this);

	
	ActionListener listener = new ActionListener() {
		public void actionPerformed(ActionEvent e)
			{	
			if(action != null)
				{
				if(cbmenuitem.isSelected())
					guiserver.out.println("("+ action + " \"" + id + "\" true)");
				else
					guiserver.out.println("("+ action + " \"" + id + "\" nil)");
					
				guiserver.out.flush();
				}
			}
		};

	cbmenuitem.addActionListener(listener);
	}

public void setText(StringTokenizer tokens)
	{
	String text = tokens.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);

	cbmenuitem.setText(text);
	}

public void clearText(StringTokenizer tokens)
	{
	cbmenuitem.setText("");
	}

public void setSelected(StringTokenizer tokens)
	{
	if(tokens.nextToken().equals("true"))
		cbmenuitem.setSelected(true);
	else
		cbmenuitem.setSelected(false);
	}
	
public void setIcon(StringTokenizer tokens)
	{
	String path = tokens.nextToken();
	cbmenuitem.setIcon(guiserver.getIconFromPath(path, this.getClass()));
	}

public void setAccelerator(StringTokenizer tokens)
	{
	String keystroke = Base64Coder.decodeString(tokens.nextToken());
	
	//System.out.println("setting accelerator: " + keystroke);
	
	cbmenuitem.setAccelerator(KeyStroke.getKeyStroke(keystroke));
	
	// Syntax:
	// <modifiers>* (<typedID> | <pressedReleasedID>)
	// modifiers := shift | control | ctrl | meta | alt | button1 | button2 | button3
	// typedID := typed <typedKey>
    // typedKey := string of length 1 giving Unicode character.
    // pressedReleasedID := (pressed | released) key
    // key := KeyEvent key code name, i.e. the name following "VK_".
	//
	// Examples:
	// "INSERT"
    // "control DELETE"
    // "alt shift X"
    // "alt shift released X"
    // "typed a"
	//
	// note that alt on MacOS X is the option key
	// for letters use uppercase, keys care added in menu item display automatically
	}

	
}
 
 
// eof //
