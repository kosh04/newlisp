// OSXAdapter.java
// guiserver
//
//
//    Copyright (C) 2010 Lutz Mueller
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


// adapted from OSXAdapter.java in package apple.dts.samplecode.osxadapter;

import com.apple.eawt.*;

public class OSXAdapter extends ApplicationAdapter {

	private static OSXAdapter						theAdapter;
	private static com.apple.eawt.Application		theApplication;

	private guiserver									mainApp;
	
	private OSXAdapter (guiserver inApp) {
		mainApp = inApp;
	}
	
	public void handleAbout(ApplicationEvent ae) {
		if (mainApp != null) {
			ae.setHandled(true);
			mainApp.about();
		} else {
			throw new IllegalStateException("handleAbout: MyApp instance detached from listener");
		}
	}
	
	public void handlePreferences(ApplicationEvent ae) {
		if (mainApp != null) {
			mainApp.preferences();
			ae.setHandled(true);
		} else {
			throw new IllegalStateException("handlePreferences: MyApp instance detached from listener");
		}
	}
	
	public void handleQuit(ApplicationEvent ae) {
		if (mainApp != null) {
			ae.setHandled(false);
			mainApp.quit();
		} else {
			throw new IllegalStateException("handleQuit: MyApp instance detached from listener");
		}
	}
	
	
	public static void registerMacOSXApplication(guiserver inApp) {
		if (theApplication == null) {
			theApplication = new com.apple.eawt.Application();
		}			
		
		if (theAdapter == null) {
			theAdapter = new OSXAdapter(inApp);
		}
		theApplication.addApplicationListener(theAdapter);
	}
	
	public static void enablePrefs(boolean enabled) {
		if (theApplication == null) {
			theApplication = new com.apple.eawt.Application();
		}
		theApplication.setEnabledPreferencesMenu(false);
	}
}

// eof //
