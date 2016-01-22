//
//  ListBoxWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 5/15/07.
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

@SuppressWarnings("unchecked") 
public class ListBoxWidget extends gsObject {

JList list = null;
JScrollPane scrollpane;
DefaultListModel listModel;

public ListBoxWidget(StringTokenizer params)
	{
	scrollpane = new JScrollPane();
	component = scrollpane;
	container = scrollpane;
	
	id = params.nextToken();
	action = params.nextToken();
	
	listModel = new DefaultListModel();
	while(params.hasMoreTokens())
		{
		addListItem(params);
		}
	
//	listModel.addElement(Base64Coder.decodeString(params.nextToken()));
		
	list = new JList(listModel);
	jcomponent = list;
	scrollpane.getViewport().setView(list);
	
	list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    list.setSelectedIndex(0);

	gsObject.widgets.put(id, this);
	
	
	KeyListener keyListener = new KeyAdapter() {
		public void keyTyped(KeyEvent e)
			{
			int index = list.getSelectedIndex();
			String item = (String)list.getSelectedValue();
	
			if(guiserver.UTF8 && item.length() != 0)
				item = Base64Coder.encodeStringUTF8(item);
			else
				item = Base64Coder.encodeString(item);
	
			guiserver.out.println("(" + action + " \"" + id + "\" " + index + " \"" + item + "\")");
			guiserver.out.flush();
			}
		};

	MouseListener mouseListener = new MouseAdapter() {
     public void mouseClicked(MouseEvent e) 
		{
		int clickCount = e.getClickCount();
		int index = list.locationToIndex(e.getPoint());

		if(action != null)
			{
			try {
				String item = (String)listModel.getElementAt(index);

				if(guiserver.UTF8 && item.length() != 0)
					item = Base64Coder.encodeStringUTF8(item);
				else
					item = Base64Coder.encodeString(item);

				guiserver.out.println("(" + action + " \"" + id + "\" " + index + " \"" + item + "\" " + clickCount + ")");
				guiserver.out.flush();
				} catch (Exception exc) { }
			}

		if(mouseEvent != null) 
			{
			mouseEventOut(e, "clicked");
			}
		}
		
	public void mousePressed(MouseEvent e) 
		{ 
		if(mouseEvent != null) mouseEventOut(e, "pressed"); 
		}
		
	public void mouseReleased(MouseEvent e) 
		{ 
		if(mouseEvent != null) mouseEventOut(e, "released"); 
		}
	
	};
	
	list.addMouseListener(mouseListener);	
	list.addKeyListener(keyListener);
	}
	
public void addListItem(StringTokenizer tokens)
	{
	while(tokens.hasMoreTokens()) 
		{
		String text = tokens.nextToken();
	
		if(guiserver.UTF8)
			text = Base64Coder.decodeStringUTF8(text);
		else
			text = Base64Coder.decodeString(text);

		listModel.addElement(text);
		}
	}
	
public void removeListItem(StringTokenizer tokens)
	{
	int index = 0; 
	
	while(tokens.hasMoreTokens()) 
		{
		index = Integer.parseInt(tokens.nextToken());
		if(index > (listModel.size() - 1)) index = listModel.size() - 1;
		if(index < 0) index = 0;
		listModel.remove(index);
		}
	}
	
public void clearList(StringTokenizer tokens)
	{
	while(listModel.size() > 0)
		listModel.remove(0);
	}
	
public void insertListItem(StringTokenizer tokens)
	{
	int index = 0; 
	String text;
	
	while(tokens.hasMoreTokens())
		{
		text = tokens.nextToken();
	
		if(guiserver.UTF8)
			text = Base64Coder.decodeStringUTF8(text);
		else
			text = Base64Coder.decodeString(text);

		index = Integer.parseInt(tokens.nextToken());
		if(index > (listModel.size() - 1)) index = listModel.size() - 1;
		if(index < 0) index = 0;
		
		listModel.insertElementAt(text, index);
		}
	}
	
	
public void selectListItem(StringTokenizer tokens)
	{
	Boolean flag = false;
	String item = tokens.nextToken();
	
	if(guiserver.UTF8)
		item = Base64Coder.decodeStringUTF8(item);
	else
		item = Base64Coder.decodeString(item);
	
	if(tokens.hasMoreTokens())
		flag = tokens.nextToken().equals("true");

	list.setSelectedValue(item, flag);
	}
	
					
public void scrollDown()
    {
	JScrollBar vbar = scrollpane.getVerticalScrollBar();
    vbar.setValue(vbar.getMinimum());
    }
	
public void registerMouseEvent(StringTokenizer tokens)
	{
	mouseEvent = tokens.nextToken();
	}
	
private void mouseEventOut(MouseEvent e, String type)
	{
	guiserver.out.println("(" + mouseEvent + " \"" + id + "\" \"" + type + "\" " + e.getX() + " " + e.getY() + " " + 
		e.getButton() + " " + e.getClickCount() + " " + e.getModifiers() + ")");
	guiserver.out.flush();
	}

}
 
 
// eof //
