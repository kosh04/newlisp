//
//  ListenerThread.java
//  guiserver
//
//  Created by Lutz Mueller on 5/24/07.
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

public class ListenerThread implements Runnable {

public ListenerThread() 
	{
	}
	
public void run() {
	String cmd = null;
	while(true)
		{
		try { cmd = guiserver.in.readLine(); } catch (Exception ex) {}
		//cmd =  guiserver.in.readLine();
		if(cmd == null) System.exit(0);   
		//if(guiserver.debug) Dispatcher.dispatch(cmd);
		//else
			{
			try { Dispatcher.dispatch(cmd);	}
			catch (Exception e)	{ ErrorDialog.show(cmd, "Missing argument or item not found"); }
			}
		}
	}

}
 
 
// eof //
