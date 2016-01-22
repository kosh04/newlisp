//
//  TextShape.java
//  guiserver
//
//  Created by Lutz Mueller on 6/24/07.
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
import java.awt.color.*;
import java.awt.geom.*;
import java.util.*;
import java.io.UnsupportedEncodingException;

@SuppressWarnings("unchecked")
public class TextShape extends Shape {

String text;
Font textShapeFont;

public TextShape(StringTokenizer tokens)
	{
	tag = tokens.nextToken();
	text = Base64Coder.decodeString(tokens.nextToken());
	if(guiserver.UTF8)
	try {
		text = new String(text.getBytes(), "UTF-8");
		} catch (UnsupportedEncodingException ee) {}

	X = Integer.parseInt(tokens.nextToken());
	Y = Integer.parseInt(tokens.nextToken());
	
	if(tokens.hasMoreTokens())
		paintColor = Shape.getColorParameter(tokens);
	else
		paintColor = CanvasWidget.currentCanvas.currentPaint;
		
	//System.out.println("text color:" + 
			//paintColor.getRed() + " " + paintColor.getGreen() + " " + paintColor.getBlue());
	
	if(tokens.hasMoreTokens())
		{
		double theta = Float.parseFloat(tokens.nextToken());
		theta = 2.0 * Math.PI * theta / 360.0;
		transform = AffineTransform.getRotateInstance(theta, X, Y);
		}
		
	//stroke = CanvasWidget.currentCanvas.currentStroke;
	
	textShapeFont = CanvasWidget.currentCanvas.currentFont;
	
	CanvasWidget.currentCanvas.drawobjects.add(this);
	}
	
public void drawShape(Graphics2D g2)
	{
	//g2.setStroke(stroke);
	g2.setPaint(paintColor);
	//System.out.println("font draw text with:" + textShapeFont);
	g2.setFont(textShapeFont);
	//g2.setComposite(currentComposite);
	
	g2.drawString(text, X, Y);
	}
	
public boolean hasPoint(int x, int y)
	{
	Component component = CanvasWidget.currentCanvas.component;
	FontMetrics fm = component.getFontMetrics(textShapeFont);	

	int height = fm.getHeight();

	return(new Rectangle(X, Y - height, fm.stringWidth(text), height).contains(x, y));
	}
	
}
 
 
// eof //
