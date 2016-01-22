//
//  ImageButtonWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 5/22/07.
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

public class ImageButtonWidget  extends aButton {

@SuppressWarnings("unchecked") 
public ImageButtonWidget(StringTokenizer params)
	{
	int width = 32;
	int height = 32;
	
	abutton = new JButton();
	jcomponent = abutton;
	component = abutton;
	container = abutton;
	
	id = params.nextToken();
	action = params.nextToken();
	
	setIconFromPath(Base64Coder.decodeString(params.nextToken()));
	
	if(params.hasMoreTokens())
		setPressedIconFromPath(Base64Coder.decodeString(params.nextToken()));
	else
		abutton.setPressedIcon(new ImageIcon(Toolkit.getDefaultToolkit().getImage(getClass().getResource("/images/pressedbutton32.png"))));
	
	
	if(params.hasMoreTokens())
		{
		width = Integer.parseInt(params.nextToken());
		height = Integer.parseInt(params.nextToken());
		}
		
	abutton.setPreferredSize(new Dimension(width, height));
	
	abutton.setBorder(BorderFactory.createEmptyBorder());
	
	gsObject.widgets.put(id, this);
	
	ActionListener listener = new ActionListener() {
		public void actionPerformed(ActionEvent e)
			{
			if(action != null)
				{
				guiserver.out.println("("+ action + " \"" + id +  "\")");
				guiserver.out.flush();
				}
			}
		};
		
	abutton.addActionListener(listener);
	}
	
		
public void setIconFromPath(String path)
	{
	abutton.setIcon(guiserver.getIconFromPath(path, this.getClass()));
	}
	
public void setPressedIconFromPath(String path)
	{
	abutton.setPressedIcon(guiserver.getIconFromPath(path, this.getClass()));
	}

}
 
 
// eof //
