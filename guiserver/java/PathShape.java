//
//  PathShape.java
//  guiserver
//
//  Created by Lutz Mueller on 7/3/07.
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

@SuppressWarnings("unchecked")
public class PathShape extends Shape {

int pointXd[];
int pointYd[];

int N;

public PathShape(StringTokenizer tokens)
	{
	tag = tokens.nextToken();
	N = Integer.parseInt(tokens.nextToken());
	
	pointXd = new int [N];
	pointYd = new int [N];
	
	X = Integer.parseInt(tokens.nextToken());
	Y = Integer.parseInt(tokens.nextToken());
	
	pointXd[0] = pointYd[0] = 0;
	
	for(int i = 1; i < N; i++)
		{
		pointXd[i] = Integer.parseInt(tokens.nextToken()) - X;
		pointYd[i] = Integer.parseInt(tokens.nextToken()) - Y;
		}
		
	if(tokens.hasMoreTokens())
		paintColor = Shape.getColorParameter(tokens);
		
	stroke = CanvasWidget.currentCanvas.currentStroke;
		
	CanvasWidget.currentCanvas.drawobjects.add(this);
	}
	
public void drawShape(Graphics2D g2)
	{
	g2.setStroke(stroke);
	g2.setPaint(paintColor);
	
	int pX[] = new int [N];
	int pY[] = new int [N];
	
	GeneralPath path = new GeneralPath();
	
	path.moveTo(X, Y);
	
	for(int i = 1; i < N; i++)
		path.lineTo(pointXd[i] + X, pointYd[i] + Y);
	
	g2.draw(path);
	}

}
 
 
// eof //
