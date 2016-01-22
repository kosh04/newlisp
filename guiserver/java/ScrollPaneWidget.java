//
//  ScrollPaneWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 5/23/07.
//
//
//    Copyright (C) 2013 Lutz Mueller
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
import java.util.*;

@SuppressWarnings("unchecked")
public class ScrollPaneWidget extends gsObject {

JScrollPane jscrollpane;

public ScrollPaneWidget(StringTokenizer params)
	{	
	id = params.nextToken();
	JComponent client  = ((gsObject)gsObject.widgets.get(params.nextToken())).jcomponent;
	
	jscrollpane = new JScrollPane();
	jscrollpane.setViewportView(client);
	jcomponent = jscrollpane;
	component = jscrollpane;
	container = jscrollpane;

    jscrollpane.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE); // added v10.6.5 
	
	// AS_NEEDED is default	
	//jscrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
	//jscrollpane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		
	if(params.hasMoreTokens())
		{
		int width = Integer.parseInt(params.nextToken());
		int height = Integer.parseInt(params.nextToken());
		jscrollpane.setPreferredSize(new Dimension(width, height));
		
		// FdB added
		if(params.hasMoreTokens())
			{
				JComponent customColumnHeader = ((gsObject)gsObject.widgets.get(params.nextToken())).jcomponent;
				JComponent customRowHeader = ((gsObject)gsObject.widgets.get(params.nextToken())).jcomponent;
				JComponent customCorner = ((gsObject)gsObject.widgets.get(params.nextToken())).jcomponent;
				jscrollpane.setColumnHeaderView(customColumnHeader);
				jscrollpane.setRowHeaderView(customRowHeader);
				jscrollpane.setCorner(JScrollPane.UPPER_LEFT_CORNER,customCorner);
			}
	}

	gsObject.widgets.put(id, this);
	}


}
 
 
// eof //
