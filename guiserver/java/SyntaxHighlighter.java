//
//  SyntaxHighlighter.java
//  guiserver
//
//  Created by Lutz Mueller on 7/19/07.
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
public class SyntaxHighlighter {

static String reserved[] = {
"!","!=","$","$0","$1","$10","$11","$12","$13","$14","$15","$2","$3","$4","$5","$6","$7","$8","$9","$args",
"$error-event","$idx","$it","$main-args","$x","%","&","*","+","-","/","<","<<","<=","=",">",">=",">>","?","@","MAIN","NaN?",
"^","abort","abs","acos","acosh","add","address","amb","and","append","append-file","apply","args","array",
"array-list","array?","asin","asinh","assoc","atan","atan2","atanh","atom?","base64-dec","base64-enc",
"bayes-query","bayes-train","begin","beta","betai","bind","binomial","bits","callback","case","catch",
"ceil","change-dir","char","chop","clean","close","command-event","command-line","cond","cons","constant",
"context","context?","copy","copy-file","corr","cos","cosh","count","cpymem","crc32","crit-chi2","crit-f","crit-t",
"crit-z","current-line", "curry","date","date-parse","date-list","date-value","debug","dec","def-new","default","define",
"define-macro", "delete","delete-file","delete-url","destroy","det","device","difference","directory","directory?","div",
"do-until","do-while","doargs", "dolist","dostring","dotimes","dotree","dump","dup","empty?","encrypt",
"ends-with","env","erf","error-event","estack","eval","eval-string","even?","exec","exists","exit","exp","expand",
"explode","extend","factor","fft","file-info","file?","filter","find","find-all","first","flat","float","float?",
"floor","flt","for","for-all","fork", "format","fv","gammai","gammaln","gcd","get-char","get-float","get-int",
"get-long","get-string","get-url", "global","global?","if","if-not","ifft","import","inc","index","inf?","int",
"integer","integer?","intersect", "invert","irr","join","json-error","json-parse","lambda?","last","last-error",
"legal?","length","let","letex","letn","list","list?", "load","local","log","lookup","lower-case","macro","macro?",
"main-args","make-dir","map","mat","match","max","member", "min","mod","mul","multiply","net-accept","net-close",
"net-connect", "net-error","net-eval", "net-interface", "net-listen","net-local","net-lookup","net-packet","net-ipv", 
"net-peek", "net-peer","net-ping","net-receive", "net-receive-from", "net-receive-udp","net-select","net-send",
"net-send-to", "net-send-udp","net-service", "net-sessions","new", "nil","nil?","normal","not","now","nper","npv",
"nth","null?", "number?","odd?","open","or",
"ostype","pack","parse", "parse-date","path","peek","pipe","pmt","pop","pop-assoc", "post-url","pow","prefix",
"pretty-print", "primitive?","print", "println","prob-chi2","prob-f","prob-t","prob-z","process","prompt-event","protected?",
"push","put-url","pv","quote","quote?","rand", "random","randomize","read","read-buffer","read-char","read-expr",
"read-file","read-key","read-line","reader-event", "read-utf8", "real-path","receive","ref","ref-all","regex",
"regex-comp","remove-dir", "rename-file","replace", "replace-assoc","reset", "rest","reverse", "rotate","round",
"save","search","seed","seek","select","self","semaphore","send", "sequence","series","set", "set-locale",
"set-ref", "set-ref-all","setq","setf","sgn","share","signal","silent", "sin","sinh","sleep","slice", "sort",
"source","spawn", "sqrt","starts-with","stats","string","string?","struct","sub","sync","swap","sym","symbol?","symbols", 
"sys-error","sys-info","t-test","tan", "tanh","term","throw","throw-error","time","time-of-day", "timer","title-case",
"trace", "trace-highlight","transpose", "trim", "true","true?","unicode","unify","union","unique", "unless","unpack", 
"until","upper-case","utf8","utf8len", "uuid", "wait-pid","when","while","write","write-buffer", "write-char",
"write-file", "write-line","xfer-event","xml-error","xml-parse", "xml-type-tags","zero?","|","~" };

static SimpleAttributeSet comment;
static SimpleAttributeSet keyword;
static SimpleAttributeSet string;
static SimpleAttributeSet number;
static SimpleAttributeSet paren;
static SimpleAttributeSet quoted;
static SimpleAttributeSet normal;

static Color normalColor = Color.black;
static Color commentColor = Color.gray;
static Color keywordColor = new Color(0, 0, 192);
static Color stringColor = new Color(0, 128, 0);
static Color numberColor = new Color(192,128, 0);
static Color parenColor = new Color(192, 0, 0);
static Color quotedColor = new Color(96, 96, 192);

static String numberPattern = "^[+-]?\\d+$|^[+-]?\\d+\\.\\d+$|^0x[0-9a-fA-F]+$|^0b[01]+$|^(\\d*(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?$";
/*                                    lead     hex            octal      decimal                                scientific       binary */
/* static String numberPattern = "{^(\\s+|\\(|\\))(0x[0-9a-fA-F]+|[+-]?0\\d+|([+-]?(0|[1-9]\\d*)(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?)|0[bB][01]+$} ";*/
static Pattern compiledPattern;

static StyledDocument doc;
static String text;
static String token;
static Vector topLevels;

static HashSet keys = null;
static boolean active = false;

@SuppressWarnings("unchecked") 
static public void color(TextPaneWidget widget, int offset, int length)
	{
	int parenCount = 0;
	int curlCount = 0;
	int startToken;
	int idx = offset;
	int tokenLen;
	char chr;

	active = true;

	doc = widget.styledDoc;
	topLevels = widget.shTopLevels;
	
	normal = new SimpleAttributeSet();
	StyleConstants.setForeground(normal, widget.foreground);
	
	try { text = doc.getText(0, widget.documentLength); } 
	catch (Exception e) {text = null;} 	
	
	if(keys == null) init();
	while(idx < length)
		{
		chr = text.charAt(idx++);
		while(chr <= ' ' && idx < length) chr = text.charAt(idx++);		
		//System.out.println("idx: " + idx + " length: " + length + " text->" + text + "<->");
		switch(chr)
			{
			case ';':
			case '#':
				startToken = idx - 1;
				while(chr != '\n' && idx < length) chr = text.charAt(idx++);
				doc.setCharacterAttributes(startToken, idx - startToken, comment, false);
				continue;
			case '(':
				++parenCount;
				doc.setCharacterAttributes(idx - 1, 1, paren, false);
				continue;
			case ')':
				--parenCount;
				doc.setCharacterAttributes(idx - 1, 1, paren, false);
				if(parenCount == 0) // top level
					{
					topLevels.addElement(offset);
					offset = idx;
					}
				continue;
			case '\'':
 				startToken = idx - 1;
				while(chr > ' ' && chr != '(' && chr != ')' && idx < length) chr = text.charAt(idx++);
				if(chr == ' ' || chr == '(' || chr == ')')
					{
					doc.setCharacterAttributes(startToken, idx - startToken - 1, quoted, false);
					idx--;
					continue;
					}
				else
					doc.setCharacterAttributes(startToken, idx - startToken, quoted, false);
					
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
			case '{':
				startToken = idx - 1;
				curlCount = 1;
				while(curlCount != 0 && idx < length)
					{
					chr = text.charAt(idx++);
					if(chr == '}') curlCount--;
					else if(chr == '{') curlCount++;
					}
				doc.setCharacterAttributes(startToken, idx - startToken, string, false);
				continue;
			case '[':
				startToken = idx - 1;
				if(text.startsWith("[text]", startToken))
					{
					idx = text.indexOf("[/text]", startToken + 6);
					if(idx < 0) idx = length;
					else idx = idx + 7;
					doc.setCharacterAttributes(startToken, idx - startToken, string, false);
					continue;
					}
				continue;
			default:
				startToken = idx - 1;
				//System.out.println("startToken: " + startToken);
				while(chr > ' ' && chr != '(' && chr != ')' && chr != '\'' && idx < length) chr = text.charAt(idx++);

				//System.out.println("->" + chr + "<-");	
					
				if(chr == '(' || chr == ')' || chr == ' ')
					token = text.substring(startToken, idx - 1);
				else
					token = text.substring(startToken, idx );
					
				tokenLen = token.length();
					
				//System.out.println("start: " + startToken + "token: " + token + " tokenLen: " + tokenLen);

				if(keys.contains(token))
					doc.setCharacterAttributes(startToken, tokenLen, keyword, false);
				else
					{
					if(compiledPattern.matcher(token).matches())
						doc.setCharacterAttributes(startToken, tokenLen, number, false);
					else
						doc.setCharacterAttributes(startToken, tokenLen, normal, false);
					}

				if(chr == '(')
					{
					++parenCount;
					doc.setCharacterAttributes(idx - 1, 1, paren, false);
					continue;
					}
				else if(chr == ')')
					{
					--parenCount;
					doc.setCharacterAttributes(idx - 1, 1, paren, false);
					if(parenCount == 0) // top level
						{
						topLevels.addElement(offset);
						offset = idx;
						}
					continue;
					}
				else if(chr == '\'')
					{
					doc.setCharacterAttributes(idx - 1, 1, paren, false);
					continue;
					}
			}
		}
		
	active = false;
	}
	
static void init()
	{
	if(keys == null)
		{
		keys = new HashSet();
		for(int idx = 0; idx < reserved.length; idx++)
			keys.add(reserved[idx]);
		}
		
	compiledPattern = Pattern.compile(numberPattern);
	
	comment = new SimpleAttributeSet();
	StyleConstants.setForeground(comment, commentColor);
	
	keyword = new SimpleAttributeSet();
	StyleConstants.setForeground(keyword, keywordColor);
	
	string = new SimpleAttributeSet();
	StyleConstants.setForeground(string, stringColor);
	
	number = new SimpleAttributeSet();
	StyleConstants.setForeground(number, numberColor);
	
	quoted = new SimpleAttributeSet();
	StyleConstants.setForeground(quoted, quotedColor);
	
	paren = new SimpleAttributeSet();
	StyleConstants.setForeground(paren, parenColor);
	}
	
public static void setSyntaxColors(StringTokenizer tokens)
	{
	commentColor = new Color(Float.parseFloat(tokens.nextToken()),
		Float.parseFloat(tokens.nextToken()), Float.parseFloat(tokens.nextToken()));

	keywordColor = new Color(Float.parseFloat(tokens.nextToken()),
		Float.parseFloat(tokens.nextToken()), Float.parseFloat(tokens.nextToken()));
		
	stringColor = new Color(Float.parseFloat(tokens.nextToken()),
		Float.parseFloat(tokens.nextToken()), Float.parseFloat(tokens.nextToken()));
		
	numberColor = new Color(Float.parseFloat(tokens.nextToken()),
		Float.parseFloat(tokens.nextToken()), Float.parseFloat(tokens.nextToken()));
		
	quotedColor = new Color(Float.parseFloat(tokens.nextToken()),
		Float.parseFloat(tokens.nextToken()), Float.parseFloat(tokens.nextToken()));
		
	parenColor = new Color(Float.parseFloat(tokens.nextToken()),
		Float.parseFloat(tokens.nextToken()), Float.parseFloat(tokens.nextToken()));
		
	init();
	SyntaxHighlighterC.init();
	}
}

// eof
