//
//  MenuBarWidget.java
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


import java.lang.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;

public class MenuBarWidget extends gsObject {

JMenuBar menubar;
JFrame jframe;

public MenuBarWidget(StringTokenizer params)
	{
	String target = params.nextToken();
	menubar = new JMenuBar();
	
	jframe = ((WindowFrame)gsObject.widgets.get(target)).jframe;
	jframe.setJMenuBar(menubar);
	
	while(params.hasMoreTokens())
		{
		String menu = params.nextToken();
		JMenu jmenu = ((MenuWidget)gsObject.widgets.get(menu)).jmenu;
		menubar.add(jmenu);
		}
	
	}
	
public void addTo(StringTokenizer tokens)
	{}

public void setSize(StringTokenizer tokens)
	{}

}
 
 
// eof //
