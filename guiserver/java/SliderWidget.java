//
//  SliderWidget.java
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


import java.lang.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;


@SuppressWarnings("unchecked") 
public class SliderWidget extends gsObject {

final JSlider slider;

public SliderWidget(StringTokenizer params)
	{
	slider = new JSlider();
	jcomponent = slider;
	component = slider;
	container = slider;
	
	id = params.nextToken();
	action = params.nextToken();
	
	
	if(params.nextToken().equals("vertical"))
		slider.setOrientation(JSlider.VERTICAL);
	else 
		slider.setOrientation(JSlider.HORIZONTAL);
	
	slider.setMinimum(Integer.parseInt(params.nextToken()));
	slider.setMaximum(Integer.parseInt(params.nextToken()));
	slider.setValue(Integer.parseInt(params.nextToken()));
	
	class SliderListener implements ChangeListener {
		public void stateChanged(ChangeEvent e) {
			guiserver.out.println("(" + action + " \"" + id + "\" " + slider.getValue() + ")");
			guiserver.out.flush();
			}
		}
		
	slider.addChangeListener(new SliderListener());
	gsObject.widgets.put(id, this);
	}
	
public void setValue(StringTokenizer tokens)
	{
	slider.setValue(Integer.parseInt(tokens.nextToken()));
	}

}
 
 
// eof //
