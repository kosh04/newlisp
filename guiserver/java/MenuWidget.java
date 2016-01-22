//
//  MenuWidget.java
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
import javax.swing.event.*;
import java.util.*;
import java.io.UnsupportedEncodingException;

public class MenuWidget extends gsObject {

JMenu jmenu;

@SuppressWarnings("unchecked") 
public MenuWidget(StringTokenizer params)
	{
	id = params.nextToken();
	String text = params.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);

	jmenu = new JMenu(text);
	container = jmenu;
	jcomponent = jmenu;
	component = jmenu;
	
	gsObject.widgets.put(id, this);
	}

public void addSeparator(StringTokenizer tokens)
	{
	jmenu.addSeparator();
	}
}
 
 
// eof //
