//
//  ToolbarWidget.java
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


@SuppressWarnings("unchecked") 
public class ToolbarWidget extends gsObject {

JToolBar jtoolbar;

public ToolbarWidget(StringTokenizer params)
	{
	jtoolbar = new JToolBar();
	container = jtoolbar;
	component = jtoolbar;
	jcomponent = jtoolbar;
	
	id = params.nextToken();

	if(params.hasMoreTokens())
		jtoolbar.setFloatable(params.nextToken().equals("true"));
	else	
		jtoolbar.setFloatable(false);
	
	if(params.hasMoreTokens())
		{
		int width = Integer.parseInt(params.nextToken());
		int height = Integer.parseInt(params.nextToken());
		jtoolbar.setPreferredSize(new Dimension(width, height));
		}
		
	gsObject.widgets.put(id, this);
	}

public void addSeparator(StringTokenizer tokens)
	{
	jtoolbar.add(new JToolBar.Separator());
	}
}
 
 
// eof //
