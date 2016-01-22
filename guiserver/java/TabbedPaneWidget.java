//
//  TabbedPaneWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 5/17/07.
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
import javax.swing.event.*;
import java.util.*;
import java.io.UnsupportedEncodingException;

@SuppressWarnings("unchecked")
public class TabbedPaneWidget extends gsObject {

JTabbedPane tabbedpane;
HashMap tabsHash;

public TabbedPaneWidget(StringTokenizer params)
	{
	id = params.nextToken();
	action = params.nextToken();
	
	String orientation = params.nextToken();
	if(orientation.equals("bottom")) tabbedpane = new JTabbedPane(JTabbedPane.BOTTOM);
	else if(orientation.equals("left")) tabbedpane = new JTabbedPane(JTabbedPane.LEFT);
	else if (orientation.equals("right")) tabbedpane = new JTabbedPane(JTabbedPane.RIGHT);
	else tabbedpane = new JTabbedPane(JTabbedPane.TOP);
	
	component = tabbedpane;
	jcomponent = tabbedpane;
	container = tabbedpane;
	tabsHash = new HashMap();
	
	while(params.hasMoreTokens())
		{
		String cid = params.nextToken();
		gsObject gsobject = (gsObject)gsObject.widgets.get(cid);

		String title = params.nextToken();
	
		if(guiserver.UTF8)
			title = Base64Coder.decodeStringUTF8(title);
		else
			title = Base64Coder.decodeString(title);

		tabbedpane.addTab(title, gsobject.component);
		tabsHash.put(gsobject.component, cid);
		}
		
	gsObject.widgets.put(id, this);
	//guiserver.widgets.put(id, tabbedpane);
	
	ChangeListener changelistener = new ChangeListener() {
		public void stateChanged(ChangeEvent e)
			{
			int index = tabbedpane.getSelectedIndex();
			String title = tabbedpane.getTitleAt(index);

			if(guiserver.UTF8 && title.length() != 0)
				title = Base64Coder.encodeStringUTF8(title);
			else
				title = Base64Coder.encodeString(title);

			//Component cmpnt = tabbedpane.getComponentAt(index);
			//Object obj = cmpnt;
			//gsObject gso = (gsObject)obj;
	
			String cid = (String)tabsHash.get(tabbedpane.getComponentAt(index));
			
			guiserver.out.println("("+ action + " \"" + id +  "\" \"" + cid + "\" \"" + title + "\" " + index + ")");
			guiserver.out.flush();
			}
		};
		
	tabbedpane.addChangeListener(changelistener);
	}
	

public void insertTab(StringTokenizer tokens)
	{
	String scomp = tokens.nextToken();
	
	String text = tokens.nextToken();
	
	if(guiserver.UTF8)
		text = Base64Coder.decodeStringUTF8(text);
	else
		text = Base64Coder.decodeString(text);
	
	int index = 0;
	ImageIcon icon = null;
	
	if(tokens.hasMoreTokens())
		index = Integer.parseInt(tokens.nextToken());
		
	if(tokens.hasMoreTokens())
		icon = guiserver.getIconFromPath(Base64Coder.decodeString(tokens.nextToken()), this.getClass());
		
	if(index > tabbedpane.getTabCount()) 
		index = tabbedpane.getTabCount();
	
	gsObject gsobject = (gsObject)gsObject.widgets.get(scomp);
	Component comp = gsobject.component;
	
	tabbedpane.insertTab(text, icon, comp, null, index);
	tabsHash.put(comp, gsobject.id);
	}

public void removeTab(StringTokenizer tokens)
	{
	int index = 0;
	int tcount = tabbedpane.getTabCount();
	
	if(tcount == 1) return;
	
	if(tokens.hasMoreTokens())
		index = Integer.parseInt(tokens.nextToken());

	
	if(index >= tcount) 
		index = tcount - 1;
	if(index < 0) index = 0;
		
	Component comp = tabbedpane.getComponentAt(index);
	tabsHash.remove(comp);

	try { tabbedpane.removeTabAt(index); } catch (IndexOutOfBoundsException ex) {}
	}
		

public void setIcon(StringTokenizer tokens)
	{
	ImageIcon icon = guiserver.getIconFromPath(Base64Coder.decodeString(tokens.nextToken()), this.getClass());
	
	int index = Integer.parseInt(tokens.nextToken());

	if(index >= tabbedpane.getTabCount())
		index = tabbedpane.getTabCount() - 1;
	if(index < 0) index = 0;
	
	tabbedpane.setIconAt(index, icon);
	}
	
public void setText(StringTokenizer tokens)
	{
	String title = tokens.nextToken();
	
	if(guiserver.UTF8)
		title = Base64Coder.decodeStringUTF8(title);
	else
		title = Base64Coder.decodeString(title);

	int index = Integer.parseInt(tokens.nextToken());

	if(index >= tabbedpane.getTabCount())
		index = tabbedpane.getTabCount() - 1;
	if(index < 0) index = 0;
	
	tabbedpane.setTitleAt(index, title);
	}
	
	
public void requestFocus(StringTokenizer tokens)
	{
	int index = Integer.parseInt(tokens.nextToken());
	
	if(index >= tabbedpane.getTabCount()) 
		index = tabbedpane.getTabCount() - 1;
	if(index < 0) index = 0;
	
	tabbedpane.setSelectedIndex(index);
	}
		
}
 
 
// eof //
