//
//  MenuItemWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 5/21/07.
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

public class MenuItemWidget extends aButton {

JMenuItem menuitem;

@SuppressWarnings("unchecked") 
public MenuItemWidget(StringTokenizer params)
	{
	menuitem = new JMenuItem();
	jcomponent = menuitem;
	component = menuitem;
	container = menuitem;
	abutton = menuitem;
	
	id = params.nextToken();
	action = params.nextToken();
	
	String mtext = params.nextToken();
	
	if(guiserver.UTF8)
		mtext = Base64Coder.decodeStringUTF8(mtext);
	else
		mtext = Base64Coder.decodeString(mtext);

	menuitem.setText(mtext);
	
	gsObject.widgets.put(id, this);
	
	ActionListener listener = new ActionListener() {
		public void actionPerformed(ActionEvent e)
			{	
			if(action != null)
				{
				guiserver.out.println("("+ action + " \"" + id + "\")");		
				guiserver.out.flush();
				}
			}
		};

	menuitem.addActionListener(listener);
	}
	
public void setAccelerator(StringTokenizer tokens)
	{
	String keystroke = Base64Coder.decodeString(tokens.nextToken());
	
	//System.out.println("setting accelerator: " + keystroke);
	
	menuitem.setAccelerator(KeyStroke.getKeyStroke(keystroke));
	
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
