//
//  guiserver.java
//  guiserver
//
//  Created by Lutz Mueller on 5/10/07.
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
import java.lang.reflect.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.font.*;
import java.util.*;
import java.io.*;
import java.net.*;
import javax.swing.*;

@SuppressWarnings("unchecked") 
public class guiserver extends gsObject
	{
	static BufferedReader in;
	static PrintWriter out;
	static boolean listening = true;
	static boolean debug = false;
	static boolean MAC_OS_X;
	static boolean UTF8 = true;
	static boolean DOUBLE_BUFFERING = true;
	static boolean connected = false;
	static SplashWindow splash = null;
	static Frame frame = null;
	static double version = 1.66;

    public static void main (String args[]) throws IOException, InterruptedException 
		{
		int portIn = 64001;
		int portOut = 64002;
		String host = null;
		String tkn = null;
		String osName;
        String newlispPath;
		Socket socket = null;
		String splashImagePath = null;
		
		osName = System.getProperty("os.name");
		MAC_OS_X = osName.toLowerCase().startsWith("mac os x");

		System.out.println("newLISP-GS v." + version + " on " + osName);
	
		if(args.length == 3) splashImagePath = args[2];
		
		// put splash screen first
		if(splashImagePath != null)
			{
			frame = new Frame();
			MediaTracker mt = new MediaTracker(frame);
			Image splashImage = guiserver.getImageFromPath(splashImagePath, frame.getClass());
			mt.addImage(splashImage, 0);
			try { mt.waitForID(0); } catch(InterruptedException ie) {}
			splash = new SplashWindow(frame, splashImage);
			}
			
		gsObject gsobject = new guiserver(new StringTokenizer("System"));
		
		if(args.length >= 1) 
			portIn = Integer.parseInt(args[0]);
		
		portOut = portIn + 1;

        if(MAC_OS_X) newlispPath = "/usr/local/bin/newlisp ";
        else newlispPath = "newlisp ";
			
		if(args.length >= 2)
			execCommand(newlispPath + args[1] + " " + portIn + " javastart &");
			
		// open listener and connection to remote
		System.out.println(" listening on " + portIn);
		ServerSocket ssocket = new ServerSocket(portIn);
		Socket client = ssocket.accept();
		host = ssocket.getInetAddress().getHostAddress();
		System.out.println(" accepted connection from " + host);
		in = new BufferedReader(new InputStreamReader(client.getInputStream()));
		ssocket.close();
		
		//System.out.println("->" + System.getProperty("line.separator").length());
		System.setProperty("line.separator", "\n");
		
		System.out.println(" connecting to " + host + ":" + portOut);

		int count = 0;
		while(connected == false)
		{
			try { socket = new Socket(host, portOut); connected = true; } catch (IOException ioe)
				{
				Thread.sleep(100);
				if(count == 30)
					{
					System.out.println(" server could not connect to " + host + ":" + portOut);
					System.exit(1);
					}
				count = count + 1;
				System.out.println(" retrying to connect");
				continue;
				}
		}
		
		out = new PrintWriter(new OutputStreamWriter(socket.getOutputStream()));
		System.out.println("server connected");

		Dispatcher.init();
		
		String cmd = null;
		try {
			while(listening)
				{
				cmd = in.readLine();
				if(cmd == null) { System.out.println("server shutdown"); System.exit(0); }   
				if(debug) Dispatcher.dispatch(cmd);
				else
					{
					try { Dispatcher.dispatch(cmd);	}
					catch (Exception e)	{ ErrorDialog.show(cmd, "Missing argument or item not found"); }
					}
				}
			} catch (IOException IOexc) { System.out.println("server shutdown"); System.exit(0); }
		}

/*
	public static void disposeSplash()
		{
		if(splash != null) splash.dispose();
		if(frame != null) frame.dispose();
		}
*/
		
	public static void execCommand(String command) throws IOException
		{
		String s;
		
		
		System.out.println("guiserver starting newLISP \"" + command + "\"");
		Process p = Runtime.getRuntime().exec(command);
/*
		BufferedReader stdInput = new BufferedReader(new 
                 InputStreamReader(p.getInputStream()));

		BufferedReader stdError = new BufferedReader(new 
                 InputStreamReader(p.getErrorStream()));

            // read the output from the command
            while ((s = stdInput.readLine()) != null) {
                System.out.println(s);
            }
            
            // read any errors from the attempted command
            while ((s = stdError.readLine()) != null) {
                System.out.println(s);
            }
*/
		System.out.println("guiserver finished exec");
		}
		
		
	public static Image getImageFromPath(String path,  Class cls)
		{
		if(path.startsWith("/local/"))
			return(Toolkit.getDefaultToolkit().getImage(cls.getClass().getResource("/images" + path.substring(6))));
		else
			return(Toolkit.getDefaultToolkit().getImage(path));
		}

	public static ImageIcon getIconFromPath(String path, Class cls)
		{
		if(path.startsWith("/local/"))
			return(new ImageIcon(Toolkit.getDefaultToolkit().getImage(cls.getClass().getResource("/images" + path.substring(6)))));
		else
			return(new ImageIcon(path));
		}
	
		
	public guiserver(StringTokenizer tokens)
		{
		id = tokens.nextToken();
		
		gsObject.widgets.put(id, this);
		
		if(MAC_OS_X) 
			{
			registerForMacOSXEvents();
			guiserver.DOUBLE_BUFFERING = false; // not necessary on OX X
			}
		else
			{
			GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
			GraphicsDevice gd = ge.getDefaultScreenDevice();
			GraphicsConfiguration gc = gd.getDefaultConfiguration();
			BufferCapabilities bufCap = gc.getBufferCapabilities();
			if(bufCap.isPageFlipping())
				System.out.println(" double buffering supported.");
			else
				System.out.println(" double buffering not supported.");

			try { UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName()); } 
			catch( Exception ex ) { }
			}
		}


// System object methods
public void setTrace(StringTokenizer tokens)
	{
	if(tokens.nextToken().equals("true"))
			guiserver.debug = true;
		else
			guiserver.debug = false;
	}	

public void setLookAndFeel(StringTokenizer tokens)
	{
	try { 
		UIManager.setLookAndFeel(tokens.nextToken()); } 
	catch(Exception ex) {
		ErrorDialog.show("set-look-and-feel", "Could not set look and feel");}
	}	
	
public void disposeSplash(StringTokenizer tokens)
	{
	if(splash != null) splash.dispose();
	if(frame != null) frame.dispose();
	}	
	
public void getScreen(StringTokenizer tokens)
	{
	Toolkit tk = Toolkit.getDefaultToolkit();
	
	double screenW = tk.getScreenSize().getWidth();
	double screenH = tk.getScreenSize().getHeight();
	int screenRes = tk.getScreenResolution();
	
	guiserver.out.println("(set 'gs:screen '(" + screenW + " " + screenH + " " + screenRes + "))\n");
	guiserver.out.flush();
	}

public void getFonts(StringTokenizer tokens)
{
    GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
    String[] fontNames = ge.getAvailableFontFamilyNames();
    String item;

    guiserver.out.print("(set 'gs:fonts '( ");
    for(int i = 0; i < fontNames.length; i++)
        {
        item = fontNames[i];
		if(guiserver.UTF8)
        	guiserver.out.print("\"" + Base64Coder.encodeStringUTF8(item) + "\" ");
		else
        	guiserver.out.print("\"" + Base64Coder.encodeString(item) + "\" ");
        }
    guiserver.out.println(")) ");
    guiserver.out.flush();
	}

public void getVersion(StringTokenizer tokens)
	{
	guiserver.out.println("(set 'gs:version " + guiserver.version + ")\n");
	guiserver.out.flush();
	}
	
public void setSyntaxColors(StringTokenizer tokens)
	{
	SyntaxHighlighter.setSyntaxColors(tokens);
	}
	
public void playSound(StringTokenizer tokens)
	{
	SoundSystem.playSound(Base64Coder.decodeString(tokens.nextToken()));
	}

public void midiInit(StringTokenizer tokens)
	{
	MidiSynth.midiInit(tokens);
	}

public void midiClose(StringTokenizer tokens)
	{
	MidiSynth.midiClose();
	}
	
public void playNote(StringTokenizer tokens)
	{
	MidiSynth.playNote(tokens);
	}
	
public void playSequence(StringTokenizer tokens)
	{
	MidiSynth.playSequence(tokens);
	}

public void stopSequence(StringTokenizer tokens)
	{
	MidiSynth.stopSequence();
	}
	
public void saveSequence(StringTokenizer tokens)
	{
	MidiSynth.saveSequence(tokens);
	}

public void addTrack(StringTokenizer tokens)
	{
	MidiSynth.addTrack(tokens);
	}

public void getInstruments(StringTokenizer tokens)
	{
	MidiSynth.getInstruments();
	}
	
public void midiPatch(StringTokenizer tokens)
	{
	MidiSynth.midiPatch(tokens);
	}
	
public void channelBend(StringTokenizer tokens)
	{
	MidiSynth.channelBend(tokens);
	}

public void channelReverb(StringTokenizer tokens)
	{
	MidiSynth.channelReverb(tokens);
	}

public void midiBPM(StringTokenizer tokens)
	{
	MidiSynth.midiBPM(tokens);
	}
	
public void muteTrack(StringTokenizer tokens)
	{
	MidiSynth.muteTrack(tokens);
	}
	
public void soloTrack(StringTokenizer tokens)
	{
	MidiSynth.soloTrack(tokens);
	}
	
public void setUTF8(StringTokenizer tokens)
	{
	if(tokens.nextToken().equals("true"))
		guiserver.UTF8 = true;
	else
		guiserver.UTF8 = false;
	}
	
// Mac OS X specific methods

    public void registerForMacOSXEvents() {
        if (MAC_OS_X) {
            try {
                // Generate and register the OSXAdapter, passing it a hash of all the methods we wish to
                // use as delegates for various com.apple.eawt.ApplicationListener methods
                OSXAdapter.setQuitHandler(this, getClass().getDeclaredMethod("quit", (Class[])null));
                OSXAdapter.setAboutHandler(this, getClass().getDeclaredMethod("about", (Class[])null));
                //OSXAdapter.setPreferencesHandler(this, getClass().getDeclaredMethod("preferences", (Class[])null));
            } catch (Exception e) {
                System.err.println("Error while loading the OSXAdapter:");
                e.printStackTrace();
            }
        }
    }

	public void about() 
		{
		gsObject gsobject = (gsObject)gsObject.widgets.get("AboutDialog");
		if(gsobject != null && (gsobject instanceof WindowFrame))
			((DialogWidget)gsobject).jdialog.setVisible(true);			
		else
			JOptionPane.showMessageDialog(null, 
				"Software: copyright (c) 2016 Lutz Mueller http://newlisp.org\n" +
				"Icons: copyright (c) 2016 Michael Michaels http://neglook.com\nAll rights reserved.", 
				"About newLISP-GS v." + version, JOptionPane.PLAIN_MESSAGE,
				getIconFromPath("/local/newLISP64.png", this.getClass()));
   		}
	
	public void preferences() 
		{ 
		gsObject gsobject = (gsObject)gsObject.widgets.get("PreferencesDialog");
		if(gsobject != null && (gsobject instanceof WindowFrame))
			((DialogWidget)gsobject).jdialog.setVisible(true);
		}
	
	public int quit() 
		{
		int option = JOptionPane.showConfirmDialog(null, 
			"Are you sure you want to quit?\nPossibility of losing unsaved content.", "Quit?", JOptionPane.YES_NO_OPTION);
		if (option == JOptionPane.YES_OPTION) 
			System.exit(0);
        return option;
		}
	}
	
	
// the end
 
 
// eof //
