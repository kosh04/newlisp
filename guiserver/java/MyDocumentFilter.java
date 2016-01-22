//
//  MyDocumentFilter.java
//  guiserver
//
//  Created by Lutz Mueller on 7/7/07.
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


import javax.swing.*;
import javax.swing.text.*;
import java.awt.Toolkit;

public class MyDocumentFilter extends DocumentFilter
	{
	int maxCharacters;
		
	public MyDocumentFilter(int max)
		{
		maxCharacters = max;
		}
	
	public void insertString(FilterBypass fb, int offs, String str, AttributeSet a) throws BadLocationException 
		{
		if ((fb.getDocument().getLength() + str.length()) <= maxCharacters)
			super.insertString(fb, offs, str, a);
        else
			Toolkit.getDefaultToolkit().beep();
		}
	
    public void replace(FilterBypass fb, int offs, int length, String str, AttributeSet a) throws BadLocationException 
		{
        if ((fb.getDocument().getLength() + str.length()
             - length) <= maxCharacters)
            super.replace(fb, offs, length, str, a);
        else
            Toolkit.getDefaultToolkit().beep();
		}
	}
 
 
// eof //
