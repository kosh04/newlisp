// MidiSynth.java
// guiserver
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
import java.util.*;
import java.io.File;
import javax.sound.midi.*;

public class MidiSynth
{
static int	noteNumber = 0;
static int	velocity = 0;
static int	duration = 0;
static Synthesizer synth = null;
static Sequencer sequencer = null;
static Receiver receiver = null;
static Transmitter transmitter = null;
static MidiChannel[] channels = null;
static Instrument instruments[] = null;
static Sequence sequence = null;
static Soundbank soundbank = null;
static int sysResolution = 32;
static final int sysBpm = 120;
static int userResolution = 16;
static int userBpm = 120; 
static int tickAccum;
static Track track;
static Integer[] channelPrograms = null;
static Integer[] channelBends = null;
static HashMap instrumentNumbers = new HashMap();

@SuppressWarnings("unchecked")
public static void midiInit(StringTokenizer params)
	{
	String filePath;
	try	{
		synth = MidiSystem.getSynthesizer();
		synth.open();
		sequencer = MidiSystem.getSequencer(); 
		//transmitter = sequencer.getTransmitter();
		//receiver = synth.getReceiver();			
		//transmitter.setReceiver(receiver);
		} 
	catch (Exception me)
		{
		ErrorDialog.showExit("gs:midi-init", "MIDI system unavailable - will exit");	
		return;	
		}
		
	if(params.hasMoreTokens())
		{
		filePath = Base64Coder.decodeString(params.nextToken());
		try { soundbank = MidiSystem.getSoundbank(new File(filePath)); }
		catch (Exception me) {
			ErrorDialog.show("gs:midi-init", "Cannot read soundbank:" + filePath);
			}
		}
	else 
		soundbank = synth.getDefaultSoundbank();
		
	instruments = soundbank.getInstruments();
	
	//instruments = synth.getDefaultSoundbank().getInstruments();
	//instruments = synth.getAvailableInstruments();
	for(int i = 0; i < Math.min(128, instruments.length); i++)
		instrumentNumbers.put(instruments[i].getName(), i);
		
    synth.loadInstrument(instruments[0]);
	channelPrograms = new Integer[16];
	channelBends = new Integer[16];
	for(int i = 0; i < 16; i++) 
		{
		channelPrograms[i] = 0;
		channelBends[i] = 8192;
		}
	channels = synth.getChannels();
	}

public static void playNote(StringTokenizer params)
	{
	int bend = 0;
	
	noteNumber = Integer.parseInt(params.nextToken());
	noteNumber = Math.min(127, Math.max(0, noteNumber));
	
	duration = Integer.parseInt(params.nextToken());
	int milliseconds = (duration * 60000)/(userBpm * userResolution);
	
	velocity = Integer.parseInt(params.nextToken());
	velocity = Math.min(127, Math.max(0, velocity));
	
	int chan = Integer.parseInt(params.nextToken());
		
	if(params.hasMoreTokens())
		{
		bend = Integer.parseInt(params.nextToken());
		bend = Math.min(8192, Math.max(-8192, bend));
		}
	
	if(channels == null)
		{
		ErrorDialog.showExit("gs:play-note", "MIDI system no initialized - will exit");
		return;
		}
		
	MidiChannel	channel = channels[chan];
	channel.setPitchBend(channelBends[chan] + bend);

	channel.noteOn(noteNumber, velocity);
	
	try	{ Thread.sleep(milliseconds);}
	catch (InterruptedException e) { }

	channel.noteOff(noteNumber);
	}
	
public static void getInstruments()
	{
	if(instruments == null)
		{
		ErrorDialog.showExit("gs:get-instruments", "MIDI system no initialized - will exit");
		return;
		}
		
	guiserver.out.print("(set 'gs:instruments '( ");
	for(int i = 0; i < Math.min(128, instruments.length); i++)
		{
		Instrument instrument = instruments[i];
		guiserver.out.print("\"" + Base64Coder.encodeString(instrument.getName()) + "\" ");
		}
	guiserver.out.println(")) ");
	guiserver.out.flush();

	}
	
public static void midiPatch(StringTokenizer params)
	{
	String programName = Base64Coder.decodeString(params.nextToken());
	int chan = Integer.parseInt(params.nextToken());
	
	if(synth == null)
		ErrorDialog.showExit("gs:midi-patch", "MIDI system no initialized - will exit");

	int program = 0;
	
	try { program = (Integer)instrumentNumbers.get(programName); }
	catch (Exception ex) {
		ErrorDialog.show("gs:midi-patch", "Instrument not known");
		}
			
	try { synth.loadInstrument(instruments[program]); }
	catch (Exception e)	{ 
		ErrorDialog.show("gs:midi-path", "Instrument not available");
		}
		
	MidiChannel	channel = channels[chan];
	channel.programChange(program);
	channelPrograms[chan] = program;
	}
	
public static void midiBPM(StringTokenizer params)
	{
	userBpm = Integer.parseInt(params.nextToken());
	if(params.hasMoreTokens())
		userResolution = Integer.parseInt(params.nextToken());
	}
	
public static void channelBend(StringTokenizer params)
	{
	int chan = Integer.parseInt(params.nextToken());
	int bend = Integer.parseInt(params.nextToken());
	bend = Math.min(8191, Math.max(-8192, bend));
	channelBends[chan] = bend + 8192;
	
	MidiChannel channel = channels[chan];
	}
	
public static void channelReverb(StringTokenizer params)
	{
	final int REVERB = 91;
	int chan = Integer.parseInt(params.nextToken());
	int reverb = Integer.parseInt(params.nextToken());
	reverb = Math.min(127, Math.max(0, reverb));
	
	MidiChannel channel = channels[chan];
	channel.controlChange(REVERB, reverb);
	}

public static void channelPressure(StringTokenizer params)
	{
	int chan = Integer.parseInt(params.nextToken());
	int pressure = Integer.parseInt(params.nextToken());
	pressure = Math.min(127, Math.max(0, pressure));
	
	MidiChannel channel = channels[chan];
	channel.setChannelPressure(pressure);
	}
	
public static void addTrack(StringTokenizer params)
	{
	ShortMessage message;
	int chan = Integer.parseInt(params.nextToken());
	MidiChannel channel = channels[chan];
	int program = channelPrograms[chan];
	float tickFactor = ((userResolution * 120) / userBpm);
	
	//System.out.println("tickFactor: " + tickFactor);
	
	if(sequence == null)
		try {
			//System.out.println("creating sequence: " + resolution);
			sequence = new Sequence(Sequence.PPQ, sysResolution);
			}
		catch (Exception ex) {
			ErrorDialog.showExit("gs:add-track", "Cannot create sequence - will exit");
		}


	track = sequence.createTrack();
	tickAccum = 0;
	
	synth.loadInstrument(instruments[program]);
	createShortMessage(ShortMessage.PROGRAM_CHANGE, chan, program, 64);
	tickAccum = 1;

	int oldBend = 8192;
	
	while(params.hasMoreTokens())
		{
		int key = Integer.parseInt(params.nextToken());
		int duration = Integer.parseInt(params.nextToken());
		int velocity = Integer.parseInt(params.nextToken()); 
		int bend = Integer.parseInt(params.nextToken());
		bend = Math.min(8191, Math.max(-8192, bend)) + channelBends[chan];
		
		//System.out.println("bend: " + bend + " tick: " + tickAccum);
		
		int tickIncrement = (int)((duration * tickFactor)/8);
		
		if(bend != oldBend)
			{ 
			createShortMessage(ShortMessage.PITCH_BEND, 0, bend & 0x7F, (bend >> 7) & 0xFF); 
			tickAccum += 1;
			tickIncrement -= 1;
			oldBend = bend;
			}			
			
		createShortMessage(ShortMessage.NOTE_ON, chan, key, velocity);
		tickAccum += tickIncrement;
		createShortMessage(ShortMessage.NOTE_OFF, chan, key, velocity);
		}
	}
	
public static void createShortMessage(int type, int chan, int num, int velocity)
	{
	ShortMessage message = new ShortMessage();
	
	try {
		message.setMessage(type, chan, num, velocity);
		MidiEvent event = new MidiEvent(message, tickAccum);
		track.add(event);
		}
	catch (InvalidMidiDataException de) 
		{ 
		ErrorDialog.show("MIDI system", "Cannot create message channel:" + chan + " tick:" + tickAccum ); 
		de.printStackTrace();
		}
	}
	
public static void muteTrack(StringTokenizer params)
	{
	int track = Integer.parseInt(params.nextToken());
	if(params.nextToken().equals("nil"))
		sequencer.setTrackMute(track, false);
	else
		sequencer.setTrackMute(track, true);
	}
	
// seems not to work on Mac OS X, 2007 MacBook
public static void soloTrack(StringTokenizer params)
	{
	int track = Integer.parseInt(params.nextToken());
	if(params.nextToken().equals("nil"))
		sequencer.setTrackSolo(track, false);
	else
		sequencer.setTrackSolo(track, false);
	}
	
public static void playSequence(StringTokenizer params)
	{
	if(synth == null)
		ErrorDialog.showExit("gs:play-sequence", "MIDI system not initialized - will exit");

	if(sequence == null)
		ErrorDialog.showExit("gs:play-sequence", "No tracks created - will exit");

	int start = Integer.parseInt(params.nextToken()); // default = 0	
	int loopCount = Integer.parseInt(params.nextToken()); // default = 0
	long startPoint = Long.parseLong(params.nextToken()); // default = 0
	long endPoint = Long.parseLong(params.nextToken()); // default = -1

	
	try	{
		if(sequencer.isOpen() != true) 		
			sequencer.open();
		
		sequencer.setSequence(sequence);
		
		if(endPoint == -1) endPoint = sequence.getTickLength();

		//System.out.println("start:" + start + " loop count:" + loopCount + " loop start:" + startPoint + " loop end:" + endPoint);
	
		sequencer.setTickPosition(start);
		sequencer.start();
		sequencer.setLoopStartPoint(startPoint);
		sequencer.setLoopEndPoint(endPoint);
		sequencer.setLoopCount(loopCount);
		sequencer.setTempoInBPM(sysBpm);
		} 
	catch (Exception e)
		{ ErrorDialog.showExit("gs:play-sequence", "Cannot start sequencer - will exit"); }

	}
	
public static void stopSequence()
	{
	if(sequencer == null)
		ErrorDialog.show("gs:play-sequence", "No sequencer to stop");
		
	sequencer.stop();
	long tickCount = sequencer.getTickPosition();
	
	guiserver.out.print("(set 'gs:tick-position " + tickCount + ")");
	guiserver.out.flush();
	}
	
	
public static void saveSequence(StringTokenizer params) 
	{
	String filePath = Base64Coder.decodeString(params.nextToken());
	//System.out.println("MIDI file:" + filePath);
	File file = new File(filePath);
	try 	
		{
		int[] fileTypes = MidiSystem.getMidiFileTypes(sequence);
		if (fileTypes.length == 0)
			ErrorDialog.show("gs:save-sequence", "Cannot save sequence");
		else 
			if (MidiSystem.write(sequence, fileTypes[0], file) == -1) 
				ErrorDialog.show("gs:save-sequence", "Cannot write file");
		} catch (Exception ex) {ErrorDialog.show("gs:save-sequence", "Cannot write file:" + filePath);}
	}


public static void midiClose()
	{
	if(synth != null) synth.close();
	if(sequence != null) sequencer.close();
		
	synth = null;
	sequencer = null;
	}

}


/* eof */
