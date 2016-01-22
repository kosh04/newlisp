//
//  ProgressBarWidget.java
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
import java.util.*;

@SuppressWarnings("unchecked") 
public class ProgressBarWidget extends gsObject {

JProgressBar progressbar;

public ProgressBarWidget(StringTokenizer params)
	{
	progressbar = new JProgressBar();
	jcomponent = progressbar;
	component = progressbar;
	container = progressbar;
	
	id = params.nextToken();
	
	progressbar.setMinimum(Integer.parseInt(params.nextToken()));
	progressbar.setMaximum(Integer.parseInt(params.nextToken()));
	progressbar.setValue(Integer.parseInt(params.nextToken()));
	
	gsObject.widgets.put(id, this);
	}

public void setValue(StringTokenizer tokens)
	{
	progressbar.setValue(Integer.parseInt(tokens.nextToken()));
	}

}
 
 
// eof //
