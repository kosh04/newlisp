//
//  TextPaneWidget.java
//  guiserver
//
//  Created by Lutz Mueller on 6/11/07.
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
import java.util.*;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.text.html.*;
import javax.swing.event.*;
import javax.swing.undo.*;
import java.io.UnsupportedEncodingException;

@SuppressWarnings("unchecked")
public class TextPaneWidget extends aTextWidget {

JTextPane textPane;
String contentType = null;

CaretListener caretListener;
CaretEvent lastCaretEvent = null;
int lastCharCode = 65535;
int lastModifiers = 0;
int documentLength = 0;
String undoState = "nil";
String redoState = "nil";

Color foreground = new Color(0,0,0);
StyledDocument styledDoc;
Vector shTopLevels;
TextPaneWidget widget;

static final int SYNTAX_NONE = 0;
static final int SYNTAX_NEWLISP = 1;
static final int SYNTAX_C = 2;
static final int SYNTAX_CPP = 3;
static final int SYNTAX_JAVA = 4;
static final int SYNTAX_PHP = 5;

int syntaxSelected = SYNTAX_NONE;

//undo helpers
protected UndoAction undoAction;
protected RedoAction redoAction;
protected UndoManager undo = new UndoManager();
MyUndoableEditListener undoableEditListener;
boolean undoEnabled = true;

public TextPaneWidget(StringTokenizer params)
	{
	id = params.nextToken();
	action = params.nextToken();
	
	textPane = new JTextPane();
	textPane.setDoubleBuffered(true);
	
	Caret caret = new MyCaret();
	caret.setBlinkRate(500);
	
	textPane.setCaret(caret);
	
	if(params.hasMoreTokens())
		contentType = params.nextToken();

	textPane.setContentType(contentType);
			
	areaScrollPane = new JScrollPane(textPane);
    areaScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
	
	container = areaScrollPane;
	jcomponent = textPane;
	component = areaScrollPane;
	textcomp = textPane;
	isScrollable = true;
	
	widget = this;

	shTopLevels = new Vector();

	if(params.hasMoreTokens())
		areaScrollPane.setPreferredSize(
			new Dimension (Integer.parseInt(params.nextToken()),Integer.parseInt(params.nextToken())));
		
	gsObject.widgets.put(id, this);

	styledDoc = textPane.getStyledDocument();
	AbstractDocument doc = (AbstractDocument)styledDoc;
	//doc.setDocumentFilter(new MyDocumentFilter(1000000));
	
	KeyListener keyListener = new KeyAdapter() {
		public void keyPressed(KeyEvent e)
			{
			Character chr = new Character(e.getKeyChar());
			int code = e.getKeyCode();
			lastCharCode = chr.hashCode();
			
			lastModifiers = e.getModifiersEx();
			//System.out.println("->" + lastCharCode);
			if(syntaxSelected != SYNTAX_NONE) colorSyntax();
					
			switch(lastCharCode)
				{
				case 40:
					highlightClosingPar('(', ')');
					break;
				case 41:
					highlightOpeningPar('(', ')');
					break;
				case 123:
					highlightClosingPar('{', '}');
					break;
				case 125:
					highlightOpeningPar('{', '}');
					break;
				case 91:
					highlightClosingPar('[', ']');
					break;
				case 93:
					highlightOpeningPar('[', ']');
					break;

				}
			}
		};
		
	caretListener = new CaretListener() {
		public void caretUpdate(CaretEvent ce)
			{
			int mark, dot;
			
			if(ce == null) return;
			
			mark = ce.getMark();
			dot = ce.getDot();
			
			lastCaretEvent = ce;
			
			undoAction.updateUndoState();
			redoAction.updateRedoState();
			
			if(undoAction.isEnabled()) undoState = "true";
			else undoState = "nil";
			
			if(redoAction.isEnabled()) redoState = "true";
			else redoState = "nil";
			
			guiserver.out.println("(" + action + " \"" + id + "\" " + lastCharCode + " " + 
				lastModifiers + " " + dot + " " + mark + " " + documentLength + " " + 
				undoState + " " + redoState + ")");
			guiserver.out.flush();
			
			lastCharCode = 65535;
			}
		};
	
	undoAction = new UndoAction();
	redoAction = new RedoAction();
	
	undoableEditListener = new MyUndoableEditListener();
	
	
	// set ctrl-Z and meta-Z undo/redo keys
	InputMap inputMap = textPane.getInputMap();
	KeyStroke key;
	
	if(guiserver.MAC_OS_X)
		key = KeyStroke.getKeyStroke(KeyEvent.VK_Z, Event.META_MASK);
	else
		key = KeyStroke.getKeyStroke(KeyEvent.VK_Z, Event.CTRL_MASK);
	inputMap.put(key, undoAction);

	if(guiserver.MAC_OS_X)
		key = KeyStroke.getKeyStroke(KeyEvent.VK_Z, Event.META_MASK | Event.SHIFT_MASK);
	else
		key = KeyStroke.getKeyStroke(KeyEvent.VK_Z, Event.CTRL_MASK | Event.SHIFT_MASK);
	inputMap.put(key, redoAction);
	
	
	styledDoc.addUndoableEditListener(undoableEditListener);
	styledDoc.addDocumentListener(new MyDocumentListener());

	textPane.addHyperlinkListener(new Hyperactive());
	textPane.addCaretListener(caretListener);
	textPane.addKeyListener(keyListener);
	}
	
public void highlightOpeningPar(char opng, char clsng)
	{
	int balance = -1;
	String text = textPane.getText();
	int currentPos = textPane.getCaretPosition();
	int caretPos = 0;
	int len = text.length();

	caretPos = currentPos - 1; // new
	// go back looking for matching parenthesis
	while(balance != 0 && caretPos >= 0)
		{
		if(text.charAt(caretPos) == opng) ++balance;
		else if(text.charAt(caretPos) == clsng) --balance;
		--caretPos;
		}
		
	++caretPos;

	if(balance == 0) highlightPosition(caretPos, currentPos);
	}
	
public void highlightClosingPar(char opng, char clsng)
	{
	int balance = 1;
	String text = textPane.getText();
	int currentPos = textPane.getCaretPosition();
	int caretPos = 0;
	int len = text.length();	
	
	if(len == 0) return;
		
	caretPos = currentPos;
	
	// go forward to matching parentheseis
	while(balance != 0 && caretPos < len)
		{
		if(text.charAt(caretPos) == opng) ++balance;
		else if(text.charAt(caretPos) == clsng) --balance;
		++caretPos;
		}
		
	if(balance == 0) highlightPosition(caretPos, currentPos);
	}
	
public void highlightPosition(int pos, int oldPos)
	{
	textPane.setCaretPosition(pos);
	textPane.getCaret().paint(textPane.getGraphics());
	textPane.repaint();
	try {Thread.sleep(400); } catch (InterruptedException ie) {}
	textPane.setCaretPosition(oldPos);
	textPane.getCaret().paint(textPane.getGraphics());
	}


public void loadText(StringTokenizer tokens)
	{
	String path = Base64Coder.decodeString(tokens.nextToken());
	
	EditorKit kit = textPane.getEditorKit();
	
	System.setProperty("line.separator", "\n");
	
	try {
		if(guiserver.UTF8)
			kit.read(new FilterFileReader(path, "UTF8"), styledDoc, 0); 
		else
			kit.read(new FilterFileReader(path), styledDoc, 0); 			
		}
	catch(Exception ex) { ErrorDialog.show("gs:load-text", "Cannot load or decode file: " + path); }
	}
	
	
public void saveText(StringTokenizer tokens)
	{
	String path = Base64Coder.decodeString(tokens.nextToken());
	
	EditorKit kit = textPane.getEditorKit();
	
	try { kit.write(new FileWriter(path), styledDoc, 0, styledDoc.getLength()); }
	catch(Exception ex) { ErrorDialog.show("gs:save-text", "Cannot save file: " + path); }
	}


public void findText(StringTokenizer tokens)
	{
	String findtext = tokens.nextToken();
	
	if(guiserver.UTF8)
		findtext = Base64Coder.decodeStringUTF8(findtext);
	else
		findtext = Base64Coder.decodeString(findtext);

	String direction = "next";
	boolean isRegex = false;
	int dot;
	
	String findTextAction = tokens.nextToken();
	
	if(tokens.hasMoreTokens())
		direction = tokens.nextToken();
				
	if(tokens.hasMoreTokens())
		isRegex = tokens.nextToken().equals("true");
	
	String text = textPane.getText();
	
	int currentCaret = textPane.getCaretPosition();
	
	if(direction.equals("previous"))
		{
		if(currentCaret > findtext.length() + 1) 
			currentCaret -= findtext.length() + 1;
		dot = text.lastIndexOf(findtext, currentCaret);
		}
	else
		dot = text.indexOf(findtext, currentCaret);

	guiserver.out.println("(" + findTextAction + " \"" + id + "\" " + dot + ")");
	guiserver.out.flush();
	
	if(dot < 0)
		{
		textPane.setCaretPosition(currentCaret);
		return;
		}
		
	textPane.setCaretPosition(dot);
	textPane.moveCaretPosition(dot + findtext.length());
	}

public void setTabSize(StringTokenizer tokens)
	{
	int tabSize = Integer.parseInt(tokens.nextToken());
			
	TabSet tabSet = new TabSet(new TabStop[] { 
		new TabStop(tabSize), new TabStop(2 * tabSize), new TabStop(3 * tabSize), 
		new TabStop(4 * tabSize), new TabStop(5 * tabSize), new TabStop(6 * tabSize),
		new TabStop(7 * tabSize), new TabStop(8 * tabSize), new TabStop(9 * tabSize) });
		
	SimpleAttributeSet attributes = new SimpleAttributeSet();
    StyleConstants.setTabSet(attributes, tabSet);
    styledDoc.setParagraphAttributes(0, documentLength, attributes, false);
	}
	
public void setSyntax(StringTokenizer tokens)
	{
	String tkn  = tokens.nextToken();
	
	if(tkn.equals("true") || tkn.equals("lsp"))
		syntaxSelected = SYNTAX_NEWLISP;
	else if(tkn.equals("c"))
		syntaxSelected = SYNTAX_C;
	else if(tkn.equals("cpp"))
		syntaxSelected = SYNTAX_CPP;
	else if(tkn.equals("java"))
		syntaxSelected = SYNTAX_JAVA;
	else if(tkn.equals("php"))
		syntaxSelected = SYNTAX_PHP;
	else syntaxSelected = SYNTAX_NONE;

	if(syntaxSelected == SYNTAX_NONE)
		{
		SimpleAttributeSet normal = new SimpleAttributeSet();
		StyleConstants.setForeground(normal, widget.foreground);
		styledDoc.setCharacterAttributes(0, documentLength, normal, true);
		}
	else
		{
		shTopLevels.removeAllElements();
		if(syntaxSelected == SYNTAX_NEWLISP)
			SyntaxHighlighter.color(widget, 0, documentLength);
		else
			{
			SyntaxHighlighterC.setFlavor(syntaxSelected);
			SyntaxHighlighterC.color(widget, 0, documentLength);
			}
		}
			
	undo.discardAllEdits();
	}
	
public void setForeground(StringTokenizer tokens)
	{
	Float red = Float.parseFloat(tokens.nextToken());
	Float green = Float.parseFloat(tokens.nextToken());
	Float blue = Float.parseFloat(tokens.nextToken());
	if(tokens.hasMoreTokens())
		{
		Float alpha = Float.parseFloat(tokens.nextToken());
		textPane.setForeground(new Color(red, green, blue, alpha));
		}
	else
		textPane.setForeground(new Color(red, green, blue));
		
	widget.foreground = textPane.getForeground();
	}
	
class Hyperactive implements HyperlinkListener {
         public void hyperlinkUpdate(HyperlinkEvent e) {
 	          if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
 		      JEditorPane pane = (JEditorPane) e.getSource();
 		      if (e instanceof HTMLFrameHyperlinkEvent) {
 		          HTMLFrameHyperlinkEvent  evt = (HTMLFrameHyperlinkEvent)e;
 		          HTMLDocument doc = (HTMLDocument)pane.getDocument();
 		          doc.processHTMLFrameHyperlinkEvent(evt);
 		      } else {
 		          try {
 			      pane.setPage(e.getURL());
 		          } catch (Throwable t) {
 			      textPane.setText("The page could not be displayed.");
 		          }
 		      }
 	          }
 	      }
     }
	 
// undo and redo 

public void undoEnable(StringTokenizer tokens)
	{
	undoEnabled = tokens.nextToken().equals("true");
	}
	
public void undoText(StringTokenizer tokens)
	{
	undoAction.actionPerformed(new ActionEvent(textPane, Event.ACTION_EVENT, "Undo"));
	}
	
public void redoText(StringTokenizer tokens)
	{
	redoAction.actionPerformed(new ActionEvent(textPane, Event.ACTION_EVENT, "Redo"));
	}

public void colorSyntax() 
	{
	int caretPos = textPane.getCaretPosition();
	int pos = 0;
	int size = shTopLevels.size();
	int p, len;

	//System.out.println(".");
	for(int idx = size - 1; idx >= 0; idx--)
		{
		pos = (Integer)shTopLevels.elementAt(idx);
		if(caretPos > pos) 
			{
			for(int i = idx; i < size; i++)
				{
				p = (Integer)shTopLevels.elementAt(idx);
				shTopLevels.removeElementAt(idx);
				}
			break;
			}
		}
				
	if(documentLength > caretPos + 1024)
		len = caretPos + 1024;
	else
		len = documentLength;
		
	if(syntaxSelected == SYNTAX_NEWLISP)
		SyntaxHighlighter.color(widget, pos, len);
	else
		SyntaxHighlighterC.color(widget, pos, len);
	}
		
//This one listens for edits that can be undone.
protected class MyUndoableEditListener implements UndoableEditListener 
	{
	public void undoableEditHappened(UndoableEditEvent e) 
		{
		//Remember the edit and update the menus.
		if(SyntaxHighlighter.active || undoEnabled != true) return;
		undo.addEdit(e.getEdit());
		undoAction.updateUndoState();
		redoAction.updateRedoState();
        }
    }

// Listens for any changes to the document.
protected class MyDocumentListener implements DocumentListener 
	{
	public void insertUpdate(DocumentEvent e) { updateParams(e);}
	public void removeUpdate(DocumentEvent e) { updateParams(e);}
	public void changedUpdate(DocumentEvent e) { updateParams(e);}

	private void updateParams(DocumentEvent e)
		{
		Document document = e.getDocument();
		documentLength = document.getLength();
		}
	}

class UndoAction extends AbstractAction 
	{
	public static final long serialVersionUID = 1L;
	public UndoAction() {
		super("Undo");
		setEnabled(false);
		}

	public void actionPerformed(ActionEvent e) 
		{
		String pname = undo.getUndoPresentationName();
		try { undo.undo(); 	} catch (Exception ex) 
			{
			Toolkit.getDefaultToolkit().beep();
			}
						
		String qname = undo.getUndoPresentationName();
		if(pname.equals("Undo addition") && qname.equals("Undo deletion"))
			{
			try { undo.undo(); 	} catch (Exception ex) 
				{
				Toolkit.getDefaultToolkit().beep();
				}
			}
		
		updateUndoState();
		redoAction.updateRedoState();
		caretListener.caretUpdate(lastCaretEvent);
		}

	protected void updateUndoState() {
		if (undo.canUndo()) {
			setEnabled(true);
			putValue(Action.NAME, undo.getUndoPresentationName());
			} 
		else {
			setEnabled(false);
			putValue(Action.NAME, "Undo");
			}
		}
	}

class RedoAction extends AbstractAction 
	{
	public static final long serialVersionUID = 1L;
	public RedoAction() {
		super("Redo");
		setEnabled(false);
		}

	public void actionPerformed(ActionEvent e) 
		{
		String pname = undo.getRedoPresentationName();
		try { undo.redo();} catch (Exception ex) 
			{
			Toolkit.getDefaultToolkit().beep();
			}
						
		String qname = undo.getRedoPresentationName();
		if(pname.equals("Redo deletion") && qname.equals("Redo addition"))
			{
			try { undo.redo(); 	} catch (Exception ex) 
				{
				Toolkit.getDefaultToolkit().beep();
				}
			}
			
		updateRedoState();
		undoAction.updateUndoState();
		caretListener.caretUpdate(lastCaretEvent);
		}

	protected void updateRedoState() 
		{
		if (undo.canRedo()) {
			setEnabled(true);
			putValue(Action.NAME, undo.getRedoPresentationName());
			} 
		else {
			setEnabled(false);
			putValue(Action.NAME, "Redo");
			}
		}	
	}
	
class MyCaret extends DefaultCaret 
	{
	public static final long serialVersionUID = 1L;
	public void paint(Graphics g) {
    if (!isVisible()) return;
    try 
		{
		JTextComponent c = getComponent();
		int dot = getDot();
		Rectangle r = c.modelToView(dot);
		g.setColor(c.getCaretColor());
		g.fillRect(r.x, r.y, 2, r.height);
		}
	catch (Exception e) { System.err.println("."); }
	}

	protected synchronized void damage(Rectangle r) 
		{
		if (r == null) return;
		x = r.x;
		y = r.y;
		width = 2;
		height = r.height;
		repaint();
		}
	}
	
}
 
// eof //
