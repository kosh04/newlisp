//
//  TextAreaWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 5/16/07.
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
import java.awt.event.ActionEvent;
import java.util.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;


@SuppressWarnings("unchecked")
public class TextAreaWidget extends aTextWidget {

JTextArea textArea;
int lastCharCode = 65535;
int lastDot = 0;
Process process = null;
BufferedWriter stdOut;
BufferedReader stdInput;
BufferedReader stdError;
String command = "";
int historyIndex = 0;
Vector history;
String lastShellCommand;
String lastShellArgs;
String commandBatch = "";
boolean isInBatch = false;

public TextAreaWidget(StringTokenizer params)
	{
	id = params.nextToken();
	action = params.nextToken();
	
	textArea = new JTextArea();
	textArea.setFont(new Font("SansSerif", Font.PLAIN, 12));
	textArea.setLineWrap(true);
    textArea.setWrapStyleWord(true);
	
    areaScrollPane = new JScrollPane(textArea);
    areaScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
	
	container = areaScrollPane;
	jcomponent = textArea;
	component = areaScrollPane;
	textcomp = textArea;
	isScrollable = true;

	if(params.hasMoreTokens())
		areaScrollPane.setPreferredSize(
			new Dimension(Integer.parseInt(params.nextToken()),Integer.parseInt(params.nextToken())));
		
	gsObject.widgets.put(id, this);
		
	KeyListener keyListener = new KeyAdapter() {
		public void keyPressed(KeyEvent e)
			{
			Character chr = new Character(e.getKeyChar());
			lastCharCode = chr.hashCode();
			
			if(process != null)
				{
				if(lastCharCode == 12) // formfeed Ctrl-L
					{
					textArea.setText("");
					lastDot = 0;
					try {
						stdOut.write(10);
						stdOut.flush();
						}
					catch (IOException ioex) { System.out.println("clear screen:" + ioex); };	
					}
				}				
			}
		};
		
	CaretListener caretListener = new CaretListener() {
		public void caretUpdate(CaretEvent ce)
			{
			int mark = ce.getMark();
			int dot = ce.getDot();
			guiserver.out.println("(" + action + " \"" + id + "\" " + lastCharCode + " " + dot + " " + mark + ")");
			guiserver.out.flush();
			lastCharCode = 65535;
			//System.out.println(":" + lastDot);
			}
		};
		
	textArea.addCaretListener(caretListener);
	textArea.addKeyListener(keyListener);
	}

public void setTabSize(StringTokenizer tokens)
	{
	textArea.setTabSize(Integer.parseInt(tokens.nextToken()));
	}	

public void clearText(StringTokenizer params)
	{
	textcomp.setText("");
	lastCharCode = 65535;
	lastDot = 0;
	if(process != null)
		try {
			stdOut.write(10);
			stdOut.flush();
			}
		catch (IOException ioex) { System.out.println("clear screen:" + ioex); };
	}


public void runShell(StringTokenizer tokens)
	{
	String command;
    String args;
	
	if(tokens == null)
		{
		command = lastShellCommand;
        args = lastShellArgs;
		}
	else
		{
		command = Base64Coder.decodeString(tokens.nextToken());
		lastShellCommand = command;
        args = Base64Coder.decodeString(tokens.nextToken());
        lastShellArgs = args;
		}
	
	history = new Vector();
	historyIndex = 0;
	commandBatch = "";
	isInBatch = false;

	if(process != null)
		process.destroy();
		
	InputMap im = textArea.getInputMap();
	ActionMap am = textArea.getActionMap();
	im.put(KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0), "history-up");
	im.put(KeyStroke.getKeyStroke(KeyEvent.VK_P, InputEvent.CTRL_MASK), "history-up");
	am.put("history-up", new HistoryUpAction());
	im.put(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0), "history-down");
	im.put(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_MASK), "history-down");
	am.put("history-down", new HistoryDownAction());
	im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "command");
	am.put("command", new CommandAction());
	im.put(KeyStroke.getKeyStroke(KeyEvent.VK_A, InputEvent.CTRL_MASK), "bol");
	im.put(KeyStroke.getKeyStroke(KeyEvent.VK_HOME, 0), "bol");
	am.put("bol", new BeginningOfLineAction());
	im.put(KeyStroke.getKeyStroke(KeyEvent.VK_E, InputEvent.CTRL_MASK), "eol");
	im.put(KeyStroke.getKeyStroke(KeyEvent.VK_END, 0), "eol");
	am.put("eol", new EndOfLineAction());

	textArea.setText("");
	lastCharCode = 65535;
	lastDot = 0;
	try { setupShell(command, args); }
	catch (IOException ioex) {
		ErrorDialog.show("run-shell", "Could not start " + command + " " + args);
		process = null;
		}
	}
	
public void setupShell(String command, String args) throws IOException
	{
    String[] cmdArray = {"","","","","","","","","",""};

    cmdArray[0] = command;
    //System.out.println("--->" + command);
    String[] argsArray = args.split("\\s");
    for (int x = 0; x < argsArray.length; x++)
        {
        cmdArray[x + 1] = argsArray[x];
        //System.out.println(argsArray[x]);
        }
	process = Runtime.getRuntime().exec(cmdArray);

	if(guiserver.UTF8)
		{
		stdInput = new BufferedReader(new InputStreamReader(process.getInputStream(), "UTF8"));
		stdError = new BufferedReader( new InputStreamReader(process.getErrorStream(), "UTF8"));
		}
	else
		{
		stdInput = new BufferedReader(new InputStreamReader(process.getInputStream()));
		stdError = new BufferedReader( new InputStreamReader(process.getErrorStream()));
		}

	stdOut = new BufferedWriter( new OutputStreamWriter(process.getOutputStream()));
		
	new Thread(new stdinListener()).start();
	new Thread(new stderrorListener()).start();
	}
	
public void destroyShell(StringTokenizer params)
	{
	if(process != null) process.destroy();
	process = null;
	stdOut = null;
	}
	
public void evalShell(StringTokenizer params)
	{
	String command = Base64Coder.decodeString(params.nextToken());
	
	while(true)
		{
		if(process == null)
			{
			textArea.append("--- restarting shell ---"); 
			runShell(null);
			if(process == null) break;
			}
		else
			{
			try {
				textArea.append("\n");
				stdOut.write(command, 0, command.length());
				stdOut.flush();
				break;
				}
			catch (IOException ioex) { 
				textArea.append("--- eval shell: must restart shell ---"); 
				process = null;
				};
			}
		}
		
	}
	
class stdinListener implements Runnable
	{
	int len;
	char buff[];
	String str;
	String text;
	
	stdinListener()
		{
		buff = new char[512];
		}
		
	public void run()
		{
		try {
			while((len = stdInput.read(buff, 0, 512)) != -1)
				{
				str = new String(buff, 0, len);
				text = textArea.getText();
				if(text.length() > 100000)
					textArea.setText(text.substring(50000));
					
				textArea.append(str);
				lastDot = textArea.getText().length();
				textArea.setCaretPosition(lastDot);				
				}
			}
		catch (IOException ioex) {};
		}
	}
	
class stderrorListener implements Runnable
	{
	int len;
	char buff[];
	String str;
	String text;
	
	stderrorListener()
		{
		buff = new char[512];
		}
		
	public void run()
		{
		try {
			while((len = stdError.read(buff, 0, 512)) != -1)
				{
				str = new String(buff, 0, len);
				text = textArea.getText();
				if(text.length() > 100000)
					textArea.setText(text.substring(50000));
					
				textArea.append(str);
				lastDot = textArea.getText().length();
				textArea.setCaretPosition(lastDot);				
				}
			}
		catch (IOException ioex) {};
		}
	}


private class BeginningOfLineAction extends AbstractAction {
	public static final long serialVersionUID = 1L;
	public void actionPerformed(ActionEvent ev) {
		textArea.setCaretPosition(lastDot);
		}
	}

private class EndOfLineAction extends AbstractAction {
	public static final long serialVersionUID = 1L;
	public void actionPerformed(ActionEvent ev) {
		textArea.setCaretPosition(textArea.getText().length());
		}
	}

private class HistoryUpAction extends AbstractAction {
	public static final long serialVersionUID = 1L;
	public void actionPerformed(ActionEvent ev) {
		textArea.setCaretPosition(lastDot);
		textArea.moveCaretPosition(textArea.getText().length());
		textArea.cut();
		if(history.size() > 0)
			{
			textArea.append((String)history.elementAt(historyIndex));
			if(historyIndex < (history.size() - 1))
				historyIndex += 1;
			}
		}
	}

private class HistoryDownAction extends AbstractAction {
	public static final long serialVersionUID = 1L;
	public void actionPerformed(ActionEvent ev) {
		textArea.setCaretPosition(lastDot);
		textArea.moveCaretPosition(textArea.getText().length());
		textArea.cut();
		if(history.size() > 0)
			{
			textArea.append((String)history.elementAt(historyIndex));
			if(historyIndex > 0)
				historyIndex -= 1;
			}
		}
	}

private class CommandAction extends AbstractAction {
	public static final long serialVersionUID = 1L;
	public void actionPerformed(ActionEvent ev) {
		String text = textArea.getText();
		if(text.length() < lastDot) lastDot = text.length();
		text = text.substring(lastDot);
			
		if(guiserver.UTF8)
			try {
			text = new String(text.getBytes("UTF-8"));
			} 
		catch (UnsupportedEncodingException e) {}
			
		command = text;
		if(command.equals("[cmd]"))
			isInBatch = true;

		textArea.append("\n");
						
		try { 
			if(isInBatch == false)
				{
				stdOut.write(command, 0, command.length());
				stdOut.write(10);
				stdOut.flush();
				}
			if(command.length() > 0)
				{
				history.insertElementAt(new String(command), 0);
				}
			historyIndex = 0;
			}
		catch (IOException ioex) { 
			textArea.append("--- cannot execute: restart shell ---"); 
			};

		if(command.endsWith("\n[/cmd]"))
			{
			isInBatch = false;
			try {
				stdOut.write(command, 0, command.length());
				stdOut.write(10);
				stdOut.flush();
				}
			catch (IOException ioex) {
				textArea.append("--- cannot execute command batch ---");
				};
			}
		}
	}

}

 
// eof //
