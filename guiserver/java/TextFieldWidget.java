//
//  TextFieldWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 5/16/07.
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
import java.io.*;
import javax.swing.*;
import javax.swing.text.*;

@SuppressWarnings("unchecked")
public class TextFieldWidget extends aTextWidget {

JPasswordField textfield;

public TextFieldWidget(StringTokenizer params)
	{
	textfield = new JPasswordField();
	jcomponent = textfield;
	component = textfield;
	container = textfield;
	textcomp = textfield;
	isScrollable = false;
	
	id = params.nextToken();
	action = params.nextToken();
	textfield.setColumns(Integer.parseInt(params.nextToken()));

	if (params.hasMoreTokens())
	    {
		String covered = Base64Coder.decodeString(params.nextToken()) ;
		textfield.setEchoChar(covered.charAt(0)) ;
	    }
	else
	    {
		textfield.setEchoChar((char)0) ;
	    }

	gsObject.widgets.put(id, this);

	ActionListener listener = new ActionListener() {
		public void actionPerformed(ActionEvent e)
			{	
			if(action != null)
				{	
				String param = "\"" + Base64Coder.encodeString(String.valueOf(textfield.getPassword())) + "\"";
				guiserver.out.println("("+ action + "\"" + id +  "\"" + param + ")");
				guiserver.out.flush();
				}
			}
		};
		

	KeyListener keyListener = new KeyAdapter() {
		public void keyPressed(KeyEvent e)
			{
			Character chr = new Character(e.getKeyChar());
			if(chr.hashCode() == 27)
				{
				guiserver.out.println("("+ action + "\"" + id + "\")");
				guiserver.out.flush();
				}
			} 		
		};
		
	textfield.addKeyListener(keyListener);
	textfield.addActionListener(listener);
	}

public void setEchoChar(StringTokenizer params)
    {
	if (params.hasMoreTokens())
	    {
		String p = params.nextToken() ;
		String str = Base64Coder.decodeString(p) ;

		textfield.setEchoChar(str.charAt(0)) ;
	    }
	else
	    {
		textfield.setEchoChar((char)0) ;
	    }

    }

}
 
 
// eof //
