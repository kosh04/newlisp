//
//  SyntaxHighlighterC.java
//  guiserver
//
//  Created by Lutz Mueller on 8/4/07.
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
import java.util.*;
import java.util.regex.Pattern;
import javax.swing.*;
import javax.swing.text.*;

@SuppressWarnings("unchecked")
public class SyntaxHighlighterC {

static String reservedC[] = {
"auto", "break", "byte", "case", "char", "const", "continue", "default", "do", 
"double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", 
"long", "register", "return", "short", "signed", "sizeof", "static", "struct", "switch", 
"typedef", "union", "unsigned", "void", "volatil", "while"
};

static String reservedCPP[] = {
"bool",	"catch", "class", "delete", "friend", "inline", "new", "namespace", 
"operator",	"private", "protected", "public", "this", "throw", "try", "template"
}; 

static String reservedJava[] = {
"abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", 
"class", "const", "continue", "default", "do", "double", "else", "enum", 
"extends", "final", "finally", "float", "for", "goto", "if", "implements", 
"import", "instanceof", "int", "interface", "long", "native", "new", "package", 
"private", "protected", "public", "return", "short", "static", "strictfp", 
"super", "switch", "synchronized", "this", "throw", "throws", "transient", 
"try", "void", "volatile", "while"
};
 
static String reservedPHP[] = {
"abstract", "and", "array", "as", "break", "case", "catch", "cfunction", 
"class", "clone", "const", "continue", "declare", "default", "die", "do", 
"echo", "else", "elseif", "empty", "enddeclare", "endfor", "endforeach", 
"endif", "endswitch", "endwhile", "eval", "exception", "exit", "extends", 
"extends", "final", "for", "foreach", "function", "global", "if", "implements", 
"include", "include_once", "interface", "isset", "list", "new", "old_function", 
"or", "php_user_filter", "print", "private", "protected", "public", "require", 
"require_once", "return", "static", "switch", "this", "throw", "try", "unset", 
"use", "var", "while", "xor"
};
 
static SimpleAttributeSet comment;
static SimpleAttributeSet keyword;
static SimpleAttributeSet string;
static SimpleAttributeSet number;
static SimpleAttributeSet paren;
static SimpleAttributeSet quoted;
static SimpleAttributeSet normal;

static String numberPattern = "^-?\\d+$|^-?\\d+\\.\\d+$|^0x[0-9a-fA-F]+$";
static Pattern compiledPattern;

static StyledDocument doc;
static String text;
static String token;
static Vector topLevels;

static HashSet keys = null;

static HashSet keysC;
static HashSet keysCPP;
static HashSet keysJava;
static HashSet keysPHP;

@SuppressWarnings("unchecked") 
static public void color(TextPaneWidget widget, int offset, int length)
	{
	int parenCount = 0;
	int curlCount = 0;
	int startToken;
	int idx = offset;
	char chr;

	SyntaxHighlighter.active = true;


	doc = widget.styledDoc;
	topLevels = widget.shTopLevels;
	
	normal = new SimpleAttributeSet();
	StyleConstants.setForeground(normal, widget.foreground);
	
	try { text = doc.getText(0, widget.documentLength); } 
	catch (Exception e) {text = null;} 	
	
	//if(keys == null) init();
	
	while(idx < length)
		{
		chr = text.charAt(idx++);
		while(chr <= ' ' && idx < length) chr = text.charAt(idx++);
		switch(chr)
			{
			case '#':
				startToken = idx - 1;
				while(chr != '\n' && idx < length) chr = text.charAt(idx++);
				doc.setCharacterAttributes(startToken, idx - startToken, quoted, false);
				continue;
			case '{':
				++parenCount;
				doc.setCharacterAttributes(idx - 1, 1, paren, false);
				continue;
			case '}':
				--parenCount;
				doc.setCharacterAttributes(idx - 1, 1, paren, false);
				if(parenCount == 0) // top level
					{
					topLevels.addElement(offset);
					offset = idx;
					}
				continue;
			case '(':
				doc.setCharacterAttributes(idx - 1, 1, paren, false);
				continue;
			case ')':
				doc.setCharacterAttributes(idx - 1, 1, paren, false);
				continue;
			case '"':
				startToken = idx - 1;
				while(idx < length)
					{
					chr = text.charAt(idx++);
					if(chr == '"') 
						{
						doc.setCharacterAttributes(startToken, idx - startToken, string, false);
						break;
						}
					if(chr == '\\')
						{
						idx++;
						continue;
						}
					}
				continue;
			case '\'':
				startToken = idx - 1;
				while(idx < length)
					{
					chr = text.charAt(idx++);
					if(chr == '\'') 
						{
						doc.setCharacterAttributes(startToken, idx - startToken, string, false);
						break;
						}
					if(chr == '\\')
						{
						idx++;
						continue;
						}
					}
				continue;
			case '/':
				startToken = idx - 1;
				if(text.startsWith("/*", startToken))
					{
					idx = text.indexOf("*/", startToken + 2);
					if(idx < 0) idx = length;
					else idx = idx + 2;
					doc.setCharacterAttributes(startToken, idx - startToken, comment, false);
					}
				else if(text.startsWith("//", startToken))
					{
					while(chr != '\n' && idx < length) chr = text.charAt(idx++);
					doc.setCharacterAttributes(startToken, idx - startToken, comment, false);
					}
				continue;
			default:
				startToken = idx - 1;
				while(chr > ' ' && chr != '(' && chr != ')' && 
						chr != '\'' && chr != '"' && 
						chr != ':' && chr != ';' && chr != ',' &&
						chr != '{' && chr != '}' && chr != '[' && chr != ']' &&
						idx < length) chr = text.charAt(idx++);
				token = text.substring(startToken, idx - 1);

				if(keys.contains(token))
					doc.setCharacterAttributes(startToken, idx - startToken - 1, keyword, false);
				else
					{
					//if(token.matches(numberPattern))
					if(compiledPattern.matcher(token).matches())
						doc.setCharacterAttributes(startToken, idx - startToken - 1, number, false);
					else
						doc.setCharacterAttributes(startToken, idx - startToken - 1, normal, false);
					}
					
				switch(chr)
					{
					case '(':
					case ')':
					case '[':
					case ']':
						doc.setCharacterAttributes(idx - 1, 1, paren, false);
						continue;
					case '{':
						++parenCount;
						doc.setCharacterAttributes(idx - 1, 1, paren, false);
						continue;
					case '}':
						--parenCount;
						doc.setCharacterAttributes(idx - 1, 1, paren, false);
						if(parenCount == 0) // top level
							{
							topLevels.addElement(offset);
							offset = idx;
							}
						continue;
					case ':':
					case ';':
					case ',':
						doc.setCharacterAttributes(idx - 1, 1, normal, false);
						continue;
					case '"':
					case '\'':
						--idx;
						continue;
					}
			}
		}
		
	SyntaxHighlighter.active  = false;
	}
	
static void setFlavor(int flavor)
	{
	if(keys == null) init();
	switch(flavor)
		{
		case TextPaneWidget.SYNTAX_C:
			keys = keysC;
			break;
		case TextPaneWidget.SYNTAX_CPP:
			keys = keysCPP;
			break;
		case TextPaneWidget.SYNTAX_JAVA:
			keys = keysJava;
			break;
		case TextPaneWidget.SYNTAX_PHP:
			keys = keysPHP;
			break;
		}
	}
	
static void init()
	{
	if(keys == null)
		{
		keysC = new HashSet();
		for(int idx = 0; idx < reservedC.length; idx++)
			keysC.add(reservedC[idx]);
			
		keysCPP = new HashSet();
		for(int idx = 0; idx < reservedC.length; idx++)
			keysCPP.add(reservedC[idx]);
		for(int idx = 0; idx < reservedCPP.length; idx++)
			keysCPP.add(reservedCPP[idx]);

		keysJava = new HashSet();
		for(int idx = 0; idx < reservedJava.length; idx++)
			keysJava.add(reservedJava[idx]);

		keysPHP = new HashSet();
		for(int idx = 0; idx < reservedPHP.length; idx++)
			keysPHP.add(reservedPHP[idx]);
			
		keys = keysC;
		}
		
	compiledPattern = Pattern.compile(numberPattern);
	
	comment = new SimpleAttributeSet();
	StyleConstants.setForeground(comment, SyntaxHighlighter.commentColor);
	
	keyword = new SimpleAttributeSet();
	StyleConstants.setForeground(keyword, SyntaxHighlighter.keywordColor);
	
	string = new SimpleAttributeSet();
	StyleConstants.setForeground(string, SyntaxHighlighter.stringColor);
	
	number = new SimpleAttributeSet();
	StyleConstants.setForeground(number, SyntaxHighlighter.numberColor);
	
	quoted = new SimpleAttributeSet();
	StyleConstants.setForeground(quoted, SyntaxHighlighter.quotedColor);
	
	paren = new SimpleAttributeSet();
	StyleConstants.setForeground(paren, SyntaxHighlighter.parenColor);
	}	
}

// eof
