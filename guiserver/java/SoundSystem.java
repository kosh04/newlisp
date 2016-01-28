//
//  Dispatcher.java
//  guiserver
//
//  Created by Lutz Mueller on 8/13/07.
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

import java.io.File;
import java.io.IOException;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.SourceDataLine;

public class SoundSystem
{
private static final int	EXTERNAL_BUFFER_SIZE = 64000;

public static void playSound(String filePath)
	{
	File soundFile = new File(filePath);
	AudioInputStream audioInputStream = null;

	try	{ audioInputStream = AudioSystem.getAudioInputStream(soundFile);	}
	catch (Exception e)	{
		ErrorDialog.show("play-sound","Unsupported audio file");
		return;
		}

	AudioFormat	audioFormat = audioInputStream.getFormat();

	SourceDataLine line = null;

	DataLine.Info info = new DataLine.Info(SourceDataLine.class, audioFormat);
	try	{	
		line = (SourceDataLine) AudioSystem.getLine(info);
		line.open(audioFormat);
		}
	catch (LineUnavailableException e) {
		System.out.println("Sound data line not available");
		return;
		}
	catch (Exception e)	{
		ErrorDialog.show("play-sound","Could nor open sound data line");	
		return;
		}

	line.start();

	int	nBytesRead = 0;
	byte[]	abData = new byte[EXTERNAL_BUFFER_SIZE];

	while (nBytesRead != -1)
		{
		try	{ nBytesRead = audioInputStream.read(abData, 0, abData.length);	}
		catch (IOException e) {
			ErrorDialog.show("play-sound","Could not read audio file");
			}
		if (nBytesRead >= 0)
			{
			int	nBytesWritten = line.write(abData, 0, nBytesRead);
			}
		}

	line.drain();
	line.close();
	line = null;
	}

}

// eof
