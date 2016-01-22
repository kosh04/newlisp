//
//  FilterFileReader.java
//  guiserver
//
//  Created by Lutz Mueller on 7/17/07.
//    Copyright (C) 2016 Lutz Mueller

import java.awt.event.*;
import java.io.*;
import java.io.InputStreamReader;
// import java.io.FileReader;
import java.io.FileInputStream;


public class FilterFileReader extends InputStreamReader {

public FilterFileReader(String path, String charSet) throws java.io.FileNotFoundException, UnsupportedEncodingException
	{
	super(new FileInputStream(path), charSet);
	}

public FilterFileReader(String path) throws java.io.FileNotFoundException, UnsupportedEncodingException
	{
	super(new FileInputStream(path));
	}

/*
public class FilterFileReader extends FileReader  {

public FilterFileReader(String path) throws java.io.FileNotFoundException
	{
	super(path);
	}
*/
	
public int read() throws java.io.IOException
	{
	int chr = super.read();
	
	if(chr  == 13)
		chr = super.read();
	
	return(chr);
	}
	
public int read(char[] cbuf, int offset, int length) throws IOException
	{
	int chr;
	int len = 0;

	//System.out.println("reading:" + offset + " " + length);
	
	for(int i = 0; i < length; i++)
		{
		chr = super.read();

		if(chr == 13)
			chr = super.read();
		
		if(chr == -1) break;

		cbuf[offset + i] = (char)chr;
		++len;
		}

	if(len == 0) return(-1);
	
	return(len);
	}
		

}
