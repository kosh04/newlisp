//
//  WindowWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 6/16/07.
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
import java.awt.color.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;

@SuppressWarnings("unchecked")
public class WindowWidget extends gsObject {

Window window;
Frame frame;

public WindowWidget(StringTokenizer params)
	{
	id = params.nextToken();
	
	frame = new Frame();
	window = new Window(frame);
	component = window;
	container = window;
	
	int x = Integer.parseInt(params.nextToken());
	int y = Integer.parseInt(params.nextToken());
	int width = Integer.parseInt(params.nextToken());
	int height = Integer.parseInt(params.nextToken());

	window.setBounds(x, y, width, height);
		
	gsObject.widgets.put(id, this);
	}
	

public void setBackground(StringTokenizer tokens)
	{
	Float red = Float.parseFloat(tokens.nextToken());
	Float green = Float.parseFloat(tokens.nextToken());
	Float blue = Float.parseFloat(tokens.nextToken());
	if(tokens.hasMoreTokens())
		{
		Float alpha = Float.parseFloat(tokens.nextToken());
		component.setBackground(new Color(red, green, blue, alpha));
		}
	else
		component.setBackground(new Color(red, green, blue));
	}

public void dispose(StringTokenizer tokens)
	{
	window.dispose();
	frame.dispose();
	} 
	
public void setFont(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
		
public void setToolTip(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}

public void setTitledBorder(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void setBevelBorder(StringTokenizer tokens)
	{
	ErrorDialog.wrongApplication(Dispatcher.cmd, id);
	}
	
public void getBounds(StringTokenizer tokens)
	{
	int x, y, width, height;
	
	x = frame.getLocationOnScreen().x;
	y = frame.getLocationOnScreen().y;
		
	width = frame.getWidth();
	height = frame.getHeight();
	
	guiserver.out.println("( set 'gs:bounds '(" + x + " " + y + " " + width + " " + height + "))\n");
	guiserver.out.flush();
	}
		
}
 
 
// eof //
