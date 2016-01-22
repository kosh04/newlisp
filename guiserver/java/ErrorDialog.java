//
//  ErrorDialog.java
//  guiserver
//
//  Created by Lutz Mueller on 5/18/07.
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
import java.util.*;
import java.awt.*;
import javax.swing.*;

public class ErrorDialog {

static public void show(String command, String message)
	{
	int len = command.length();
	String cmd;
	
	if(len > 128) 
		cmd = command.substring(0, 64) + "..." + command.substring(len - 64);
	else 
		cmd = command;
	
	JOptionPane.showMessageDialog(null,
					cmd + " :    \n" + message + "    ",
					"newLISP GUI server",
					JOptionPane.ERROR_MESSAGE);
	//System.exit(0);
	return;
	}

static public void showExit(String command, String message)
    {
    show(command, message);
    System.exit(0);
    }

static public void wrongApplication(String command, String id)
	{
	int len = command.length();
	String cmd;
		
	if(len > 128) 
		cmd = command.substring(0, 64) + "..." + command.substring(len - 64);
	else 
		cmd = command;
	

	JOptionPane.showMessageDialog(null,
				cmd + ":    \ncannot be applied to " + id + "    ",
				"newLISP GUI server",
				JOptionPane.ERROR_MESSAGE);
	//System.exit(0);
	return;
	}

}
 
 
// eof //
