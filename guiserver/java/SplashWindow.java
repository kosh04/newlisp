//
//  SplashWindow.java
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
import java.awt.event.*;
import javax.swing.*;
import java.util.*;

@SuppressWarnings("unchecked") 
class SplashWindow extends Window
{
public static final long serialVersionUID = 1L;
int width;
int height;
Image splashImage;
Window splash;

public SplashWindow(Window parent, Image splashImage)
  {
  super(parent);
  splash = this;
  this.splashImage =  splashImage;
  width = splashImage.getWidth(this) + 2;
  height = splashImage.getHeight(this) + 2;
  setSize(width, height);
  Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
  setLocation((screen.width - width)/2, (screen.height - height)/2);
  if(guiserver.MAC_OS_X)
	this.setBackground(new Color(0, 0, 0, 0));
  setVisible(true);
  addMouseListener(new MouseAdapter () {
    public void mousePressed(MouseEvent e) { splash.dispose(); }
    });
  }

public void paint(Graphics g)
  {
  if(guiserver.MAC_OS_X)
	{
	g.setColor(new Color(0, 0, 0, 0));
	g.fillRect(0, 0, width, height);
	System.out.println("mac os x");
	}
  else
	{
	g.setColor(Color.lightGray);
	g.fill3DRect(0, 0, width, height, true);
	}
  if(splashImage != null)
    g.drawImage(splashImage,1,1,this);
  }
}
 
 
// eof //
