//
//  Shape.java
//  guiserver
//
//  Created by Lutz Mueller on 6/9/07.
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
import java.awt.color.*;
import java.awt.geom.*;
import java.util.*;

public class Shape {

String tag;
Stroke stroke;
Color paintColor;
AlphaComposite composite;
AffineTransform transform = AffineTransform.getScaleInstance(1.0, 1.0);
//AffineTransform transform = null;
boolean visible = true;
int X;
int Y;

public void drawShape(Graphics2D g2) 
	{ }
	
	
public boolean hasPoint(int X, int Y)
	{
	return false;
	}

static Color getColorParameter(StringTokenizer tokens)
	{
	float R, G, B;
	
	R = Float.parseFloat(tokens.nextToken());
	G = Float.parseFloat(tokens.nextToken());
	B = Float.parseFloat(tokens.nextToken());
	
	return new Color(R, G, B);
	}

}
 
 
// eof //
