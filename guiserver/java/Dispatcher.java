//
//  Dispatcher.java
//  guiserver
//
//  Created by Lutz Mueller on 5/14/07.
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
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.*;
import java.io.*;
import java.net.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

@SuppressWarnings("unchecked")
public class Dispatcher {

static String cmd;
static HashMap classes = new HashMap();
static HashMap methods = new HashMap();
	
public static void dispatch(String command)
	{
	StringTokenizer tokens = null;
	String id = null;
	gsObject gsobject = null;
	Class cfoo;
	Constructor constor;
	Class[] defArgs = {StringTokenizer.class};
	Method method;
	String methodName;
	
	if(guiserver.debug) System.out.println("-> " + command);
	
	tokens = new StringTokenizer(command);
	
	Object[] initArgs = {tokens};
	cmd = tokens.nextToken();
			
	// look for constructor methods
	if((cfoo = (Class)classes.get(cmd)) != null)
		{
		try { 
			constor = cfoo.getConstructor(defArgs); 
			constor.newInstance(initArgs);
			} catch (Exception ce) { 
			if(guiserver.debug)
				System.out.println("Error: " + cmd + "=>" + ce);
			else
				{
				ErrorDialog.show(command, "Could not create " + cmd);
				//System.exit(1);
				return;
				}
			}
				
		return;
		}

	else // get target object
		{
		id = tokens.nextToken();
		gsobject = (gsObject)gsObject.widgets.get(id);
		//System.out.println("get target: " + id);
		if(gsobject == null)
			{
			ErrorDialog.wrongApplication(cmd, id);
			//System.exit(1);
			return;
			}
		}

	// get method for target and invoke it with arguments
	try { 
		//System.out.println("invoke " + cmd + " on " + id);
		methodName = (String)methods.get(cmd);
		//System.out.println("methodName:" + methodName);
		method = gsobject.getClass().getMethod(methodName, defArgs);
		//System.out.println("method:" + method);
		if(method != null)
			{
			method.invoke(gsobject, initArgs);
			return;
			}
		} catch (Exception ce) { 
		if(guiserver.debug)
			ce.printStackTrace();
		else
			{
			ErrorDialog.show(command, "Could not invoke method " + cmd + " with " + id);
			//System.exit(1);
			return;
			}
		}
	}
	
public static void init()
	{
	// GUI widget classes
	classes.put("window", WindowWidget.class);
	classes.put("frame", WindowFrame.class);
	classes.put("dialog", DialogWidget.class);
	classes.put("panel", PanelWidget.class);
	classes.put("tabbed-pane", TabbedPaneWidget.class);
	classes.put("split-pane", SplitPaneWidget.class);
	classes.put("scroll-pane", ScrollPaneWidget.class);
	classes.put("label", LabelWidget.class);
	classes.put("image-label", ImageLabelWidget.class);
	classes.put("set-grid-layout", LayoutGrid.class);
	classes.put("set-flow-layout", LayoutFlow.class);
	classes.put("set-border-layout", LayoutBorder.class);
	classes.put("button", ButtonWidget.class);
	classes.put("image-button", ImageButtonWidget.class);
	classes.put("toggle-button", ToggleButtonWidget.class);
	classes.put("check-box", CheckBoxWidget.class);
	classes.put("radio-button", RadioButtonWidget.class);
	classes.put("combo-box", ComboBoxWidget.class);
	classes.put("slider", SliderWidget.class);
	classes.put("progress-bar", ProgressBarWidget.class);
	classes.put("list-box", ListBoxWidget.class);
	classes.put("text-field", TextFieldWidget.class);
	classes.put("text-area", TextAreaWidget.class);
	classes.put("menu-item", MenuItemWidget.class);
	classes.put("menu-item-check", MenuItemCheckWidget.class);
	classes.put("menu", MenuWidget.class);
	classes.put("menu-popup", PopupMenuWidget.class);
	classes.put("menu-bar", MenuBarWidget.class);
	classes.put("table", TableWidget.class);
	classes.put("tool-bar", ToolbarWidget.class);
	classes.put("text-pane", TextPaneWidget.class);

	// 2D Graphics classes
	classes.put("canvas", CanvasWidget.class);
	
	// GUI methods
	methods.put("add-list-item","addListItem");
	methods.put("add-separator","addSeparator");
	methods.put("add-to","addTo");
	methods.put("append-text","appendText");
	methods.put("clear-list","clearList");
	methods.put("clear-text","clearText");
	methods.put("confirm-dialog","confirmDialog");
	methods.put("color-dialog","ColorChooser");
	methods.put("copy-text","copyText");
	methods.put("cut-text","cutText");
	methods.put("disable","Disable");
	methods.put("dispose","dispose");
	methods.put("enable","Enable");
	methods.put("find-text", "findText");
	methods.put("frame-closed", "frameCloseEvent");
	methods.put("frame-moved", "frameMoveEvent");
	methods.put("frame-resized", "frameResizeEvent");
	methods.put("get-bounds", "getBounds");
	methods.put("get-font-metrics", "getFontMetrics");
	methods.put("get-selected-text", "getSelection");
	methods.put("get-text","getText");
	methods.put("get-text-position","getTextPosition");
	methods.put("goto-text", "gotoText");
	methods.put("insert-list-item","insertListItem");
	methods.put("insert-tab","insertTab");
	methods.put("insert-text", "insertText");
	methods.put("layout", "layout");
	methods.put("load-text", "loadText");
	methods.put("open-file-dialog","OpenFileChooser");
	methods.put("message-dialog","messageDialog");
	methods.put("paste-text","pasteText");
	methods.put("redo-text","redoText");
	methods.put("remove-from", "removeFrom");
	methods.put("remove-list-item","removeListItem");
	methods.put("remove-tab","removeTab");
	methods.put("request-focus","requestFocus");
	methods.put("save-file-dialog","SaveFileChooser");
	methods.put("save-text", "saveText");
	methods.put("set-caret", "setCaret");
	methods.put("select-text","selectText");
	methods.put("set-accelerator","setAccelerator");
	methods.put("set-background","setBackground");
	methods.put("set-bevel-border","setBevelBorder");
	methods.put("set-caret-color","setCaretColor");
	methods.put("set-color","setBackground");	
	methods.put("set-cursor","setCursor");
	methods.put("set-echo-char", "setEchoChar");
	methods.put("set-editable","setEditable");
	methods.put("set-font","setFont");
	methods.put("set-foreground","setForeground");
	methods.put("set-icon","setIcon");
	methods.put("select-list-item","selectListItem");
	methods.put("set-pressed-icon","setPressedIcon");
	methods.put("set-resizable","setResizable");
	methods.put("set-selected","setSelected");
	methods.put("set-selection-color","setSelectionColor");
	methods.put("set-size","setPreferredSize");
	methods.put("set-text","setText");
	methods.put("set-tab-size","setTabSize");
	methods.put("set-titled-border","setTitledBorder");
	methods.put("set-tool-tip","setToolTip");
	methods.put("set-value","setValue");
	methods.put("set-visible","setVisible");
	methods.put("set-syntax", "setSyntax");
	methods.put("show-popup", "showPopup");
	methods.put("undo-text","undoText");
	methods.put("undo-enable", "undoEnable");
	methods.put("run-shell", "runShell");
	methods.put("eval-shell", "evalShell");
	methods.put("destroy-shell", "destroyShell");
	
	// methods for System object (guiserver)
	methods.put("add-track", "addTrack");
	methods.put("channel-bend", "channelBend");
	methods.put("channel-reverb", "channelReverb");
	methods.put("dispose-splash", "disposeSplash");
	methods.put("get-fonts", "getFonts");
	methods.put("get-screen", "getScreen");
	methods.put("get-instruments", "getInstruments");
	methods.put("get-version", "getVersion");
	methods.put("midi-init", "midiInit");
	methods.put("midi-close", "midiClose");
	methods.put("midi-patch", "midiPatch");
	methods.put("midi-bpm", "midiBPM");
	methods.put("mute-track", "muteTrack");
	methods.put("solo-track", "soloTrack");
	methods.put("set-trace", "setTrace");
	methods.put("set-look-and-feel", "setLookAndFeel");
	methods.put("set-syntax-colors", "setSyntaxColors");
	methods.put("play-note", "playNote");
	methods.put("play-sound", "playSound");
	methods.put("play-sequence", "playSequence");
	methods.put("stop-sequence", "stopSequence");
	methods.put("save-sequence", "saveSequence");
	methods.put("set-utf8", "setUTF8");
	
	// text table
	methods.put("table-add-column", "tableAddColumn") ;
	methods.put("table-add-row", "tableAddRow");
	methods.put("table-remove-row", "tableRemoveRow"); // FdB
	methods.put("table-set-column", "tableSetColumn");
	methods.put("table-set-column-name", "tableSetColumnIdentifiers"); // FdB
	methods.put("table-show-row-number", "tableShowRowNumber");
	methods.put("table-set-row-number", "tableShowRowNumber"); // deprecated
	methods.put("table-set-row-count", "tableSetRowCount"); // FdB
	methods.put("table-set-cell", "tableSetCell");
	methods.put("table-get-cell", "tableGetCell");
	methods.put("table-get", "tableGetValueAll");
	methods.put("table-get-size", "tableGetSize");

	// 2D Grapics
	// All methods hace the current canvas as target
	
	// methods for creating shapes
	// put in Vector in current canvas
	methods.put("draw-arc", "drawArc");
	methods.put("draw-circle", "drawCircle");
	methods.put("draw-ellipse", "drawEllipse");
	methods.put("draw-image", "drawImage"); 
	methods.put("draw-line", "drawLine");
	methods.put("draw-path", "drawPath"); 
	methods.put("draw-polygon", "drawPolygon");
	methods.put("draw-text", "drawText"); 
	methods.put("draw-rect", "drawRectangle");
	methods.put("draw-round-rect", "drawRoundRect");
	methods.put("fill-arc", "fillArc");
	methods.put("fill-circle", "fillCircle");
	methods.put("fill-ellipse", "fillEllipse");
	methods.put("fill-polygon", "fillPolygon");
	methods.put("fill-rect", "fillRectangle");
	methods.put("fill-round-rect", "fillRoundRect");
	methods.put("set-anti-aliasing", "setAntiAliasing");

	// general mouse and key events
	methods.put("mouse-event","registerMouseEvent");
	methods.put("key-event","registerKeyEvent");
	
	// specialize events for canvas
	methods.put("mouse-clicked","registerMouseClicked");
	methods.put("mouse-pressed","registerMousePressed");
	methods.put("mouse-released","registerMouseRelease");
	methods.put("mouse-entered","registerMouseEntered");
	methods.put("mouse-exited","registerMouseExited");
	methods.put("mouse-dragged","registerMouseDragged");
	methods.put("mouse-moved","registerMouseMoved");
	methods.put("mouse-wheel", "registerMouseWheel");
	
	// take canvas for global settings as target
	methods.put("set-canvas", "g2Canvas");
	methods.put("set-translation", "g2Translation");
	methods.put("set-transform", "g2Transform");
	methods.put("set-rotation", "g2Rotation");
	methods.put("set-scale", "g2Scale");
	methods.put("export", "g2Export");
	methods.put("print", "g2Print");
	methods.put("update", "g2Update");
	
	// current canvas settings used in next shape
	methods.put("set-stroke", "g2Stroke");
	methods.put("set-paint", "g2Paint");
	methods.put("set-composite", "g2Composite");
	methods.put("set-clipping", "g2Clip");
	
	// take tags for drawing shapes as target
	methods.put("delete-tag", "deleteTaggedShape");
	methods.put("move-tag", "moveTaggedShape");
	methods.put("translate-tag", "translateTaggedShape");
	methods.put("rotate-tag", "rotateTaggedShape");
	methods.put("scale-tag", "scaleTaggedShape");
	methods.put("shear-tag", "shearTaggedShape");
	methods.put("hide-tag", "hideTaggedShape");
	methods.put("show-tag", "showTaggedShape");
	methods.put("delete-tag", "deleteTaggedShape");	
	methods.put("reorder-tags", "orderTaggedShapes");	
	methods.put("color-tag", "colorTaggedShape");	
	}
}
 
 
// eof //
