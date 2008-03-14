//
//  CheckBoxWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 5/13/07.
//
//
//    Copyright (C) 2007 Lutz Mueller
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

public class CheckBoxWidget extends aButton {

@SuppressWarnings("unchecked") 

public CheckBoxWidget(StringTokenizer params)
	{
	abutton = new JCheckBox();
	jcomponent = abutton;
	component = abutton;
	container = abutton;
	
	id = params.nextToken();
	action = params.nextToken();
	
	if(params.hasMoreTokens())
		{
		String base64string = params.nextToken();
		abutton.setText(Base64Coder.decodeString(base64string));
		}
	
	if(params.hasMoreTokens())
		{
		if(params.nextToken().equals("true"))
			abutton.setSelected(true);
		}
	
	gsObject.widgets.put(id, this);

	
	ActionListener listener = new ActionListener() {
		public void actionPerformed(ActionEvent e)
			{	
			if(action != null)
				{
				if(abutton.isSelected())
					guiserver.out.println("("+ action + " \"" + id + "\" true)");
				else
					guiserver.out.println("("+ action + " \"" + id + "\" nil)");
					
				guiserver.out.flush();
				}
			}
		};

	abutton.addActionListener(listener);
	}

}
 
 
// eof //