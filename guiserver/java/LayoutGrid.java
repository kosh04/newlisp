//
//  LayoutGrid.java
//  guiserver
//
//  Created by Lutz Mueller on 5/12/07.
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
import java.util.*;

@SuppressWarnings("serial")
public class LayoutGrid extends GridLayout{

public LayoutGrid(StringTokenizer params)
	{
	super();
	
	String id = params.nextToken();
	
	gsObject gsobject = (gsObject)gsObject.widgets.get(id);
	
	Container target = gsobject.container;
	
	setRows(Integer.parseInt(params.nextToken()));
	setColumns(Integer.parseInt(params.nextToken()));
	
	if(params.hasMoreTokens())
		{
		setHgap(Integer.parseInt(params.nextToken()));
		setVgap(Integer.parseInt(params.nextToken()));
		}
	
	target.setLayout(this);
	}
	
}
 
 
// eof //
