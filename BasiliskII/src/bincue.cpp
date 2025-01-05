/*
 *  Copyright (C) 2002-2010  The DOSBox Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/* Geoffrey Brown 2010
 * Includes ideas from dosbox src/dos/cdrom_image.cpp 
 *
 * Limitations:	1) cue files must reference single bin file
 *              2) only supports raw mode1 data and audio
 *              3) no support for audio flags
 *              4) requires SDL audio or OS X core audio
 *              5) limited cue file keyword support
 *
 * Creating cue/bin files:
 * 	cdrdao read-cd --read-raw --paranoia 3 foo.toc
 *  toc2cue foo.toc
 */

#include "sysdeps.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <libgen.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>

#include <list>

#ifdef OSX_CORE_AUDIO
#include "../MacOSX/MacOSX_sound_if.h"
static int bincue_core_audio_callback(void);
#endif

#ifdef USE_SDL_AUDIO
#include <SDL.h>
#include <SDL_audio.h>
#endif

#ifdef WIN32
#define bzero(b,len) (memset((b), '\0', (len)), (void) 0)  
#define bcopy(b1,b2,len) (memmove((b2), (b1), (len)), (void) 0)
#endif

#include "bincue.h"
#define DEBUG 0
#include "debug.h"

#define MAXTRACK 100
#define MAXLINE 512
#define CD_FRAMES 75
//#define RAW_SECTOR_SIZE		2352
//#define COOKED_SECTOR_SIZE	2048

// Bits of Track Control Field -- These are standard for scsi cd players

#define PREMPHASIS 0x1
#define COPY	   0x2
#define DATA	   0x4
#define AUDIO	   0
#define FOURTRACK  0x8

// Audio status -- These are standard for scsi cd players

#define CDROM_AUDIO_INVALID    0x00
#define CDROM_AUDIO_PLAY       0x11
#define CDROM_AUDIO_PAUSED     0x12
#define CDROM_AUDIO_COMPLETED  0x13
#define CDROM_AUDIO_ERROR      0x14
#define CDROM_AUDIO_NO_STATUS  0x15

typedef unsigned char uint8;

// cuefiles can be challenging as some information is
// implied.  For example, there may a pregap (also postgap)
// of silence that must be generated.  Here we implement
// only the pregap.

typedef struct {
	int number;
	unsigned int start;	// Track start in frames
	unsigned int length;	// Track length in frames
	loff_t fileoffset;		// Track frame start within file
	unsigned int pregap;	// Silence in frames to generate
	unsigned int postgap;	// Silence in frames to generate at end
	unsigned char tcf;		// Track control field
} Track;

typedef struct {
	char *binfile;			// Binary file name
	unsigned int length;	// file length in frames
	int binfh;				// binary file handle
	int tcnt;				// number of tracks
	Track tracks[MAXTRACK]; // Track management
	int raw_sector_size;	// Raw bytes to read per sector
	int cooked_sector_size; // Actual data bytes per sector (depends on Mode)
	int header_size;		// Number of bytes used in header
	int big_endian_audio;   // Expect raw audio samples in big-endian format
} CueSheet;

typedef struct CDPlayer {
	CueSheet *cs;				// cue sheet to play from
	int audiofh;				// file handle for audio data
	unsigned int audioposition; // current position from audiostart (bytes)
	unsigned int audiostart;	// start position if playing (frame)
	unsigned int audioend;		// end position if playing (frames)
	unsigned int silence;		// pregap (silence) bytes
	unsigned char audiostatus;	// See defines above for status
	uint8 volume_left;			// CD player volume (left)
	uint8 volume_right;			// CD player volume (right)
	uint8 volume_mono;			// CD player single-channel volume
	loff_t fileoffset;			// offset from file beginning to audiostart
	bool audio_enabled = false; // audio initialized for this player?
	bool scanning = false;      // is there currently scanning in progress
	int reverse;                // for scanning, 0=forward, 1=reverse
#ifdef OSX_CORE_AUDIO
	OSXsoundOutput soundoutput;
#endif
#ifdef USE_SDL_AUDIO
	SDL_AudioStream *stream;
#endif
} CDPlayer;

// Minute,Second,Frame data type

typedef struct {
	int m, s, f; // note size matters since we scan for %d !
} MSF;

// Parser State

static unsigned int totalPregap;
static unsigned int prestart;

// Current audio output settings

struct OutputSettings {
	int freq;
	int format; // SDL format
	int channels;
	int default_cd_player_volume;
};

static bool have_current_output_settings = false;
static OutputSettings current_output_settings;

// Set up a locking implementation to deal with audio requests concurrent to other functionality

#define SHOW_LOCKING 0

#define SHOWFILE(s) D(bug("%s in %s(), %s:%d\n", s, __FUNCTION__, __FILE__, __LINE__))

static int calls_since_unlock_count = 0;
static int calls_since_lock_count = 0;
static bool locked = false;

#if defined(USE_SDL_AUDIO)
/* Threading notes for the SDL audio case:
 * - all MixAudio_bincue calls come from an SDL-owned thread
 * - all other calls come from the main thread
 */
#if !SDL_VERSION_ATLEAST(3, 0, 0)
#define SDL_Mutex SDL_mutex
#endif
  static SDL_Mutex *bincue_mutex;
  #define LOCK_BINCUE { if (SHOW_LOCKING) SHOWFILE("Doing LockBincue"); SDL_LockMutex(bincue_mutex); calls_since_lock_count = 0; locked = true; }
  #define UNLOCK_BINCUE { if (SHOW_LOCKING) SHOWFILE("Doing UnlockBincue"); calls_since_unlock_count = 0; locked = false; SDL_UnlockMutex(bincue_mutex); }
#else

 #define LOCK_BINCUE
 #define UNLOCK_BINCUE

#endif

// Audio System Variables

static uint8 silence_byte;

// CD Player state; multiple players supported through list

std::list<CDPlayer*> players;

CDPlayer* currently_playing = NULL;

static void debug_identify(const char * pref, CDPlayer* player) {
	if (player) {
		D(bug("%s(player=%p cs=%p binfile=%s)\n", pref, player, player->cs, player->cs->binfile));
	} else {
		D(bug("%s(null player)\n", pref));
	}
}

static void set_currently_playing(CDPlayer* player) {
	if (player) {
		debug_identify(" set currently playing=", player);
	} else {
		D(bug(" cleared currently playing\n"));
	}
	currently_playing = player;
}


CDPlayer* CSToPlayer(CueSheet* cs)
{
	for (std::list<CDPlayer*>::iterator it = players.begin(); it != players.end(); ++it)
		if (cs == (*it)->cs) // look for cuesheet matching existing player
			return *it;
	return NULL; // if no player with the cuesheet found, return null player
}

static void FramesToMSF(unsigned int frames, MSF *msf)
{
	msf->m = frames/(60 * CD_FRAMES);
	frames = frames%(60 * CD_FRAMES);
	msf->s = frames/CD_FRAMES;
	msf->f = frames%CD_FRAMES;
}

static int MSFToFrames(MSF msf)
{
	return (msf.m * 60 * CD_FRAMES) + (msf.s * CD_FRAMES) + msf.f;
}


static int PositionToTrack(CueSheet *cs, unsigned int position)
{
	int i;
	MSF msf;

	FramesToMSF(position, &msf);

	for (i = 0; i < cs->tcnt; i++) {
		if ((position >= cs->tracks[i].start) &&
			(position <= (cs->tracks[i].start + cs->tracks[i].length)))
			break;
	}
	return i;
}

static bool AddTrack(CueSheet *cs)
{
	int skip = prestart;
	Track *prev;
	Track *curr = &(cs->tracks[cs->tcnt]);

	prestart = 0;

	if (skip > 0) {
		if (skip > curr->start) {
			D(bug("AddTrack: prestart > start\n"));
			return false;
		}
	}

	curr->fileoffset = curr->start * cs->raw_sector_size;

	// now we patch up the indicated time

	curr->start += totalPregap;

	// curr->pregap is supposed to be part of this track, but it
	// must be generated as silence

	totalPregap += curr->pregap;

	if (cs->tcnt == 0) {
		if (curr->number != 1) {
			D(bug("AddTrack: number != 1\n"));
			return false;
		}
		cs->tcnt++;
		return true;
	}

	prev = &(cs->tracks[cs->tcnt - 1]);

	if (prev->start < skip)
		prev->length = skip - prev->start - curr->pregap;
	else
		prev->length = curr->start - prev->start - curr->pregap;

	// error checks

	if (curr->number <= 1) {
		D(bug("Bad track number %d\n", curr->number));
		return false;
	}
	if ((prev->number + 1 != curr->number) && (curr->number != 0xAA)) {
		D(bug("Bad track number %d\n", curr->number));
		return false;
	}
	if (curr->start < prev->start + prev->length) {
		D(bug("unexpected start %d\n", curr->start));
		return false;
	}

	cs->tcnt++;
	return true;
}

static bool ParseCueSheet(FILE *fh, CueSheet *cs, const char *cuefile)
{
	bool seen1st = false;
	char line[MAXLINE];
	unsigned int i_line=0;
	char *keyword;
	
	totalPregap = 0;
	prestart = 0;
	
	// Use Audio CD settings by default, otherwise data mode will be specified
	cs->raw_sector_size = 2352;
	cs->cooked_sector_size = 2352;
	cs->header_size = 0;
	cs->big_endian_audio = false;

	while (fgets(line, MAXLINE, fh) != NULL) {
		Track *curr = &cs->tracks[cs->tcnt];

		// check for CUE file

		if (!i_line && (strncmp("FILE", line, 4) != 0)) {
			return false;
		}
		i_line++;

		// extract keyword

		if (NULL != (keyword = strtok(line, " \t\n\t"))) {
			if (!strcmp("FILE", keyword)) {
				char *filename;
				char *filetype;

				if (i_line > 1) {
					D(bug("More than one FILE token\n"));
					goto fail;	
				}	
				filename = strtok(NULL, "\"\t\n\r");
				filetype = strtok(NULL, " \"\t\n\r");
				if (strcmp("BINARY", filetype) && strcmp("MOTOROLA", filetype)) {
					D(bug("Not binary file %s\n", filetype));
					goto fail;
				}
				else {
					if (!strcmp("MOTOROLA", filetype))
						cs->big_endian_audio = true;
					char *tmp = strdup(cuefile);
					char *b = dirname(tmp);
					cs->binfile = (char *) malloc(strlen(b) + strlen(filename) + 2);
					sprintf(cs->binfile, "%s/%s", b, filename);
					free(tmp);
				}
			} else if (!strcmp("TRACK", keyword)) {
				char *field;
				int i_track;

				if (seen1st) {
					if (!AddTrack(cs)){
						D(bug("AddTrack failed \n"));
						goto fail;
					}
					curr = &cs->tracks[cs->tcnt];
				}

				seen1st = true;

				// parse track number

				field = strtok(NULL, " \t\n\r");
				if (1 != sscanf(field, "%d", &i_track)) {
					D(bug("Expected  track number\n"));
					goto fail;		
				}
				curr->number = i_track;

				// parse track type and update sector size for data discs if applicable

				field = strtok(NULL, " \t\n\r");
				if (!strcmp("MODE1/2352", field)) { // red-book CD-ROM standard
					curr->tcf = DATA;
					cs->raw_sector_size = 2352;
					cs->cooked_sector_size = 2048;
					cs->header_size = 16; // remaining 288 bytes for error detection
				} else if (!strcmp("MODE2/2352", field)) { // yellow-book CD-ROM standard
					curr->tcf = DATA;
					cs->raw_sector_size = 2352;
					cs->cooked_sector_size = 2336; // no error bytes at end
					cs->header_size = 16;
				} else if (!strcmp("MODE1/2048", field)) { // pure data CD-ROM
					curr->tcf = DATA;
					cs->raw_sector_size = 2048;
					cs->cooked_sector_size = 2048;
					cs->header_size = 0; // no header or error bytes
				} else if (!strcmp("AUDIO", field)) {
					curr->tcf = AUDIO;
				} else {
					D(bug("Unexpected track type %s\n", field));
					goto fail;
				}

			} else if (!strcmp("INDEX", keyword)) {
				char *field;
				int i_index;
				MSF msf;

				// parse INDEX number

				field = strtok(NULL, " \t\n\r");
				if (1 != sscanf(field, "%d", &i_index)) {
					D(bug("Expected index number\n"));
					goto fail;
				}

				// parse INDEX start

				field = strtok(NULL, " \t\n\r");
				if (3 != sscanf(field, "%d:%d:%d", 
								 &msf.m, &msf.s, &msf.f)) {
					D(bug("Expected index start frame\n"));
					goto fail;
				}

				if (i_index == 1)
					curr->start = MSFToFrames(msf);
				else if (i_index == 0)
					prestart = MSFToFrames(msf);
			} else if (!strcmp("PREGAP", keyword)) {
				MSF msf;
				char *field = strtok(NULL, " \t\n\r");
				if (3 != sscanf(field, "%d:%d:%d", 
								 &msf.m, &msf.s, &msf.f)) {
					D(bug("Expected pregap frame\n"));
					goto fail;	
				}
				curr->pregap = MSFToFrames(msf);

			} else if (!strcmp("POSTGAP", keyword)) {
				MSF msf;
				char *field = strtok(NULL, " \t\n\r");
				if (3 != sscanf(field, "%d:%d:%d",
								&msf.m, &msf.s, &msf.f)) {
					D(bug("Expected postgap frame\n"));
					goto fail;
				}
				curr->postgap = MSFToFrames(msf);
				
				// Ignored directives
				
			} else if (!strcmp("TITLE", keyword)) {
			} else if (!strcmp("PERFORMER", keyword)) {
			} else if (!strcmp("REM", keyword)) {
			} else if (!strcmp("ISRC", keyword)) {
			} else if (!strcmp("SONGWRITER", keyword)) {
			} else {
				D(bug("Unexpected keyword %s\n", keyword));
				goto fail;		
			}
		}
	}

	AddTrack(cs); // add final track
	return true;
  fail:
	return false;
}

static bool LoadCueSheet(const char *cuefile, CueSheet *cs)
{
	FILE *fh = NULL;
	int binfh = -1;
	struct stat buf;
	Track *tlast = NULL;

	if (cs) {
		bzero(cs, sizeof(*cs));
		if (!(fh = fopen(cuefile, "r")))
			return false;

		if (!ParseCueSheet(fh, cs, cuefile)) goto fail;

		// Open bin file and find length
		#ifdef WIN32
			binfh = open(cs->binfile,O_RDONLY|O_BINARY);
		#else
			binfh = open(cs->binfile,O_RDONLY);
		#endif
		if (binfh < 0) {
			D(bug("Can't read bin file %s\n", cs->binfile));
			goto fail;
		}

		if (fstat(binfh, &buf)) {
			D(bug("fstat returned error\n"));
			goto fail;
		}

		// compute length of final track


		tlast = &cs->tracks[cs->tcnt - 1];
		tlast->length = buf.st_size/cs->raw_sector_size
						- tlast->start + totalPregap;

		D(bug("CD audio data file %s size %ld\n", cs->binfile, buf.st_size));

		if (tlast->length < 0) {
			D(bug("Binary file too short \n"));
 		  	goto fail;	
   	    }

		// save bin file length and pointer

		cs->length = buf.st_size/cs->raw_sector_size;
		cs->binfh = binfh;

		fclose(fh);
		return true;

	  fail:
		if (binfh >= 0)
			close(binfh);	
		fclose(fh);
		free(cs->binfile);
		return false;

    }
	return false;
}

#ifdef USE_SDL_AUDIO
	static void OpenPlayerStream(CDPlayer * player);
	static void ClosePlayerStream(CDPlayer * player);
#endif


void *open_bincue(const char *name)
{
	D(bug("open_bincue\n"));

#ifdef USE_SDL_AUDIO
	if (bincue_mutex == NULL)
		bincue_mutex = SDL_CreateMutex();
	assert(bincue_mutex != NULL);
#endif

	CueSheet *cs = (CueSheet *) malloc(sizeof(CueSheet));
	if (!cs) {
		D(bug("malloc failed\n"));
		return NULL;
	}

	if (LoadCueSheet(name, cs)) {
		CDPlayer *player = (CDPlayer *) malloc(sizeof(CDPlayer));
		player->cs = cs;
		player->volume_left = 0;
		player->volume_right = 0;
		player->volume_mono = 0;
		D(bug(" player=0x%p volume=%d\n", player, player->volume_mono));
#ifdef OSX_CORE_AUDIO
		player->audio_enabled = true;
#endif
		if (player->audio_enabled)
			player->audiostatus = CDROM_AUDIO_NO_STATUS;
		else
			player->audiostatus = CDROM_AUDIO_INVALID;
		player->audiofh = dup(cs->binfh);

		LOCK_BINCUE;
#ifdef USE_SDL_AUDIO
		OpenPlayerStream(player);
#endif
		// add to list of available CD players
		players.push_back(player);
		UNLOCK_BINCUE;

		return cs;
	}
	else
		free(cs);

	return NULL;
}

void close_bincue(void *fh)
{
	D(bug("close_bincue\n"));

	LOCK_BINCUE;
	CueSheet *cs = (CueSheet *) fh;
	CDPlayer *player = CSToPlayer(cs);

	if (cs && player) {
		if (player == currently_playing) {
			CDStop_bincue(fh);
			assert(currently_playing == NULL);
		}

		players.remove(player);
		UNLOCK_BINCUE;

		free(cs);
#ifdef USE_SDL_AUDIO
		ClosePlayerStream(player);
#endif
		free(player);
	}
}

/*
 * File read (cooked)
 * Data are stored in raw sectors of which only COOKED_SECTOR_SIZE
 * bytes are valid -- the remaining include header bytes at the beginning
 * of each raw sector and RAW_SECTOR_SIZE - COOKED_SECTOR_SIZE - bytes
 * at the end for error correction
 *
 * The actual number of bytes used for header, raw, cooked, error depend
 * on mode specified in the cuesheet
 *
 * We assume that a read request can land in the middle of
 * sector.  We compute the byte address of that sector (sec)
 * and the offset of the first byte we want within that sector (secoff)
 *
 * Reading is performed one raw sector at a time, extracting as many
 * valid bytes as possible from that raw sector (available)
 */

size_t read_bincue(void *fh, void *b, loff_t offset, size_t len)
{
	CueSheet *cs = (CueSheet *) fh;
	
	size_t bytes_read = 0;						// bytes read so far
	unsigned char *buf = (unsigned char *) b;	// target buffer
	unsigned char secbuf[cs->raw_sector_size];		// temporary buffer

	off_t sec = ((offset/cs->cooked_sector_size) * cs->raw_sector_size);
	off_t secoff = offset % cs->cooked_sector_size;

	// sec contains location (in bytes) of next raw sector to read
	// secoff contains offset within that sector at which to start
	// reading since we can request a read that starts in the middle
	// of a sector

	if (cs == NULL || lseek(cs->binfh, sec, SEEK_SET) < 0) {
		return -1;
	}
	while (len) {

		// bytes available in next raw sector or len (bytes)
		// we want whichever is less

		size_t available = cs->cooked_sector_size - secoff;
		available = (available > len) ? len : available;

		// read the next raw sector

		if (read(cs->binfh, secbuf, cs->raw_sector_size) != cs->raw_sector_size) {
			return bytes_read;
		}

		// copy cooked sector bytes (skip header if needed, typically 16 bytes)
		// we want out of those available

		bcopy(&secbuf[cs->header_size+secoff], &buf[bytes_read], available);

		// next sector we start at the beginning

		secoff = 0;

		// increment running count decrement request

		bytes_read += available;
		len -= available;
	}
	return bytes_read;
}

loff_t size_bincue(void *fh)
{
	if (fh) {
		return ((CueSheet *)fh)->length * ((CueSheet *)fh)->cooked_sector_size;
	}
	return 0;
}

bool readtoc_bincue(void *fh, unsigned char *toc)
{
	CueSheet *cs = (CueSheet *) fh;
	if (cs) {

		MSF msf;
		unsigned char *p = toc + 2;
		*p++ = cs->tracks[0].number;
		*p++ = cs->tracks[cs->tcnt - 1].number;
		for (int i = 0; i < cs->tcnt; i++) {

			FramesToMSF(cs->tracks[i].start, &msf);
			*p++ = 0;
			*p++ = 0x10 | cs->tracks[i].tcf;
			*p++ = cs->tracks[i].number;
			*p++ = 0;
			*p++ = 0;
			*p++ = msf.m;
			*p++ = msf.s;
			*p++ = msf.f;
		}
		FramesToMSF(cs->length, &msf);
		*p++ = 0;
		*p++ = 0x14;
		*p++ = 0xAA;
		*p++ = 0;
		*p++ = 0;
		*p++ = msf.m;
		*p++ = msf.s;
		*p++ = msf.f;

		int toc_size = p - toc;
		*toc++ = toc_size >> 8;
		*toc++ = toc_size & 0xff;
		return true;
	}
	return false;
}

bool GetPosition_bincue(void *fh, uint8 *pos)
{
	CueSheet *cs = (CueSheet *) fh;
	LOCK_BINCUE;
	CDPlayer *player = CSToPlayer(cs);
	
	if (cs && player) {
		MSF abs, rel;
		int fpos = player->audioposition / cs->raw_sector_size + player->audiostart;
		int trackno = PositionToTrack(cs, fpos);

		if (!(player->audio_enabled)) {
			UNLOCK_BINCUE;
			return false;
		}

		FramesToMSF(fpos, &abs);
		if (trackno < cs->tcnt) {
			// compute position relative to start of frame

			unsigned int position =  player->audioposition/cs->raw_sector_size +
				player->audiostart - player->cs->tracks[trackno].start;

			FramesToMSF(position, &rel);
		}
		else
			FramesToMSF(0, &rel);

		*pos++ = 0;
		*pos++ = player->audiostatus;
		*pos++ = 0;
		*pos++ = 12; // Sub-Q data length
		*pos++ = 0;
		if (trackno < cs->tcnt)
			*pos++ = 0x10 | cs->tracks[trackno].tcf;
		*pos++ = (trackno < cs->tcnt) ? cs->tracks[trackno].number : 0xAA;
		*pos++ = 1;  // track index
		*pos++ = 0;
		*pos++ = abs.m;
		*pos++ = abs.s;
		*pos++ = abs.f;
		*pos++ = 0;
		*pos++ = rel.m;
		*pos++ = rel.s;
		*pos++ = rel.f;
//		*pos++ = 0;

		static int last_fpos = 0;
		if (last_fpos != fpos) {
			D(bug("CDROM position %02d:%02d:%02d track %02d fpos %d\n", abs.m, abs.s, abs.f, trackno, fpos));
		}
		last_fpos = fpos;
		UNLOCK_BINCUE;
		return true;
	}
	else {
		UNLOCK_BINCUE;
		return false;
	}
}


static void CDPause_other_playing(CDPlayer* player) {
	if (currently_playing && currently_playing != player) {
		currently_playing->audiostatus = CDROM_AUDIO_PAUSED;
		set_currently_playing(NULL);
	}
}

bool CDPause_bincue(void *fh)
{
	CueSheet *cs = (CueSheet *) fh;
	LOCK_BINCUE;
	CDPlayer *player = CSToPlayer(cs);
	debug_identify("CDPause_bincue  ", player);

	if (cs && player) {
		player->scanning = false;
		// doesn't matter if it was playing, just ensure it's now paused
		if (currently_playing == player) {
			player->audiostatus = CDROM_AUDIO_PAUSED;
			set_currently_playing(NULL);
		}
		UNLOCK_BINCUE;
		return true;
	}
	UNLOCK_BINCUE;
	return false;
}

bool CDStop_bincue(void *fh)
{
	CueSheet *cs = (CueSheet *) fh;
	LOCK_BINCUE;
	CDPlayer *player = CSToPlayer(cs);
	debug_identify("CDStop_bincue  ", player);

	if (cs && player) {
		if (player == currently_playing) {
			set_currently_playing(NULL);
		}

#ifdef OSX_CORE_AUDIO
		player->soundoutput.stop();
#endif
		if (player->audiostatus != CDROM_AUDIO_INVALID)
			player->audiostatus = CDROM_AUDIO_NO_STATUS;

		UNLOCK_BINCUE;
		return true;
	}
	UNLOCK_BINCUE;
	return false;
}

bool CDResume_bincue(void *fh)
{
	CueSheet *cs = (CueSheet *) fh;
	LOCK_BINCUE;
	CDPlayer *player = CSToPlayer(cs);
	debug_identify("CDResume_bincue  ", player);

	if (cs && player) {
		// Pause another player if needed
		CDPause_other_playing(player);
		player->scanning = false;

		// doesn't matter if it was paused, just ensure this one plays now
		player->audiostatus = CDROM_AUDIO_PLAY;
		set_currently_playing(player);
		UNLOCK_BINCUE;
		return true;
	}
	UNLOCK_BINCUE;
	return false;
}

bool static PreparePlayOrScanAudio(CDPlayer *player) {
	if (player->audio_enabled) {
		player->audiostatus = CDROM_AUDIO_PLAY;
#ifdef OSX_CORE_AUDIO
		D(bug("starting os x sound\n"));
		player->soundoutput.setCallback(bincue_core_audio_callback);
		// should be from current track !
		player->soundoutput.start(16, 2, 44100);
#endif
		set_currently_playing(player);
		return true;
	} else {
		D(bug("play but player audio not enabled\n"));
		return false;
	}
}

bool CDPlay_bincue(void *fh, uint8 start_m, uint8 start_s, uint8 start_f,
				   uint8 end_m, uint8 end_s, uint8 end_f)
{
	CueSheet *cs = (CueSheet *) fh;
	LOCK_BINCUE;
	CDPlayer *player = CSToPlayer(cs);
	debug_identify("CDPlay_bincue ", player);

	if (cs && player) {
		// Pause another player if needed
		CDPause_other_playing(player);
		player->scanning = false;

		int track;
		MSF msf;

		player->audiostatus = CDROM_AUDIO_NO_STATUS;

		int cur_position_frames = (player->audioposition / cs->raw_sector_size) + player->audiostart;

		player->audiostart = MSFToFrames((MSF){start_m, start_s, start_f});
		player->audioend   = MSFToFrames((MSF){end_m, end_s, end_f});

		track = PositionToTrack(player->cs, player->audiostart);

		int cur_track = PositionToTrack(player->cs, cur_position_frames);
		MSF cur_msf;
		FramesToMSF(cur_position_frames, &cur_msf);
		D(bug("Track position check: requested play start %d m %d s %d f == track %d, current pos %d m %d s %d f == track %d\n",
			start_m, start_s, start_f, track, cur_msf.m, cur_msf.s, cur_msf.f, cur_track));

		if (track < player->cs->tcnt) {
			player->audioposition = 0;

			// here we need to compute silence

			if (player->audiostart - player->cs->tracks[track].start >
				player->cs->tracks[track].pregap)
				player->silence = 0;
			else
				player->silence = (player->cs->tracks[track].pregap -
								   player->audiostart +
								   player->cs->tracks[track].start) * cs->raw_sector_size;

			player->fileoffset = player->cs->tracks[track].fileoffset;

			D(bug("  file offset %d\n", (unsigned int) player->fileoffset));

			// fix up file offset if beyond the silence bytes

			if (!player->silence) // not at the beginning
				player->fileoffset += (player->audiostart -
									   player->cs->tracks[track].start -
									   player->cs->tracks[track].pregap) * cs->raw_sector_size;

			FramesToMSF(player->cs->tracks[track].start, &msf);
			D(bug("CDPlay_bincue: track %02d start %02d:%02d:%02d silence %d",
				player->cs->tracks[track].number, msf.m, msf.s, msf.f,
				player->silence/cs->raw_sector_size));
			D(bug(" Stop %02u:%02u:%02u\n", end_m, end_s, end_f));
		}
		else
			D(bug("CDPlay_bincue: play beyond last track !\n"));

		if (PreparePlayOrScanAudio(player)) {
			UNLOCK_BINCUE;
			return true;
		}

	}
	UNLOCK_BINCUE;
	return false;
}

bool CDScan_bincue(void *fh, uint8 start_m, uint8 start_s, uint8 start_f, bool reverse) {
	D(bug("CDScan_bincue start %d m %d s %d f, reverse=%d\n", start_m, start_s, start_f, reverse));
	CueSheet *cs = (CueSheet *) fh;
	LOCK_BINCUE;
	CDPlayer *player = CSToPlayer(cs);
	
	if (cs && player) {
		int goto_frame = MSFToFrames((MSF){start_m, start_s, start_f});

		int scan_starting_track = PositionToTrack(cs, goto_frame);
		if (cs->tracks[scan_starting_track].tcf != AUDIO) {
			D(bug(" scan starting from non-audio track\n"));
			UNLOCK_BINCUE;
			return false;
		}

		/* Figure out the bounds of this audio region */
		int first_audio_track = scan_starting_track;
		int last_audio_track = scan_starting_track;

		for (int track = scan_starting_track - 1; track >= 0               && cs->tracks[track].tcf == AUDIO; track--)
			first_audio_track--;
		for (int track = scan_starting_track + 1; track < player->cs->tcnt && cs->tracks[track].tcf == AUDIO; track++)
			last_audio_track++;

		player->audiostart = cs->tracks[first_audio_track].start;
		player->fileoffset = cs->tracks[first_audio_track].fileoffset;
		player->audioend = cs->tracks[last_audio_track].start + cs->tracks[last_audio_track].length;

		player->silence = 0;

		player->audioposition = (goto_frame - player->audiostart) * player->cs->raw_sector_size;
		player->reverse = reverse;
		player->scanning = true;


		if (PreparePlayOrScanAudio(player)) {
			UNLOCK_BINCUE;
			return true;
		}

	}
	UNLOCK_BINCUE;
    return false;
}

void CDSetVol_bincue(void* fh, uint8 left, uint8 right) {
	CueSheet *cs = (CueSheet *) fh;
	LOCK_BINCUE;
	CDPlayer *player = CSToPlayer(cs);
	debug_identify("CDSetVol_bincue ", player);
	D(bug(" channels set to %d, %d\n", left, right));

	if (cs && player) {
		// Convert from classic Mac's 0-255 to 0-128;
		// calculate mono mix as well in place of panning
		player->volume_left = (left*128)/255;
		player->volume_right = (right*128)/255;
		player->volume_mono = (player->volume_left + player->volume_right)/2; // use avg
		D(bug(" player=0x%p volume=%d\n", player, player->volume_mono));
	}
	UNLOCK_BINCUE;
}

void CDGetVol_bincue(void* fh, uint8* left, uint8* right) {
	CueSheet *cs = (CueSheet *) fh;
	LOCK_BINCUE;
	CDPlayer *player = CSToPlayer(cs);
	
	if (cs && player) {		// Convert from 0-128 to 0-255 scale
		*left = (player->volume_left*255)/128;
		*right = (player->volume_right*255)/128;
	}
	UNLOCK_BINCUE;
}

static uint8 *fill_buffer(int stream_len, CDPlayer* player)
{
	static uint8 *buf = 0;
	static int bufsize = 0;
	int offset = 0;

	if (bufsize < stream_len) {
		free(buf);
		buf = (uint8 *) malloc(stream_len);
		if (buf) {
			bufsize = stream_len;
		}
		else {
			D(bug("malloc failed \n"));
			return NULL;
		}
	}

	memset(buf, silence_byte, stream_len);
		
	if (player->audiostatus == CDROM_AUDIO_PLAY) {
		int remaining_silence = player->silence - player->audioposition;

		int current_read_bytes_limit = -1;
		int full_read_bytes_limit = -1;
		int jump_bytes_after = 0;

		if (player->scanning) {
			/* In a scan we alternate playing frames and jumping frames */
			int play_frames = 6;
			int jump_frames = player->reverse? -150 : 190;

			/* For testing, use some big values so you can tell what's going on: */
			/*
			int play_frames = 75*2;
			int jump_frames = player->reverse? -75*5 : 75*5;
			*/

			/* Let's call one block of frames to play and one gap in the direction we're seeking a cycle */
			int cycle_size_frames = std::abs(play_frames + jump_frames);

			full_read_bytes_limit = play_frames * player->cs->raw_sector_size;
			jump_bytes_after = jump_frames * player->cs->raw_sector_size;

			/* Let's make the cycles aligned to audiostart for convenience.
			   Handle where we are in the cycle */
			int cycle_size_bytes = cycle_size_frames * player->cs->raw_sector_size;
			int cycle_bytes_offset = player->audioposition % cycle_size_bytes;
			if (cycle_bytes_offset < full_read_bytes_limit) {
				// in a play block
				current_read_bytes_limit = full_read_bytes_limit - cycle_bytes_offset;
			} else {
				// currently in a gap, move to the start of the next play block
				int delta = - cycle_bytes_offset + full_read_bytes_limit + jump_bytes_after;
				if ((int)player->audioposition + delta < 0) {
					player->audiostatus = CDROM_AUDIO_COMPLETED;
					return buf;
				}
				player->audioposition += delta;
				current_read_bytes_limit = full_read_bytes_limit;
			}
		}

		int available;
		do {
			if (player->audiostart + player->audioposition/player->cs->raw_sector_size >= player->audioend) {
				player->audiostatus = CDROM_AUDIO_COMPLETED;
				return buf;
			}

			if (remaining_silence >= stream_len) {
				player->audioposition += stream_len;
				return buf;
			}

			if (remaining_silence > 0) {
				offset += remaining_silence;
				player->audioposition += remaining_silence;
			}

			available = ((player->audioend - player->audiostart) *
						player->cs->raw_sector_size) - player->audioposition;
			if (available > (stream_len - offset))
				available = stream_len - offset;

			bool hit_read_limit = false;
			if (current_read_bytes_limit != -1) {
				if (available >= current_read_bytes_limit) {
					available = current_read_bytes_limit;
					hit_read_limit = true;
				}
			}
			current_read_bytes_limit = full_read_bytes_limit;

			if (lseek(player->audiofh,
					  player->fileoffset + player->audioposition - player->silence,
						  SEEK_SET) < 0)
				return NULL;

			if (available < 0) {
				player->audioposition += available; // correct end !;
				available = 0;
			}

			ssize_t ret = 0;
			if ((ret = read(player->audiofh, &buf[offset], available)) >= 0) {
				player->audioposition += ret;
				offset += ret;
				available -= ret;
			}

			if ((int)player->audioposition + jump_bytes_after < 0) {
				player->audiostatus = CDROM_AUDIO_COMPLETED;
				return buf;
			}
			if (hit_read_limit)
				player->audioposition += jump_bytes_after;
		} while (player->scanning && offset < stream_len);

		while (offset < stream_len) {
			buf[offset++] = silence_byte;
			if (available-- > 0){
				player->audioposition++;
			}
		}
	}
	return buf;
}


#ifdef USE_SDL_AUDIO

bool HaveAudioToMix_bincue() {
	return currently_playing != NULL;
}

void MixAudio_bincue(uint8 *stream, int stream_len)
{
	// Threads:
	// Called from SDL playback callbacks thread e.g. SDLAudioP15

	if (!stream_len) return;

	LOCK_BINCUE;
	if (currently_playing) {
		if (locked) {calls_since_lock_count ++; } else {calls_since_unlock_count ++;}
		if (calls_since_unlock_count > 30 || calls_since_lock_count > 1) {
			D(bug("MixAudio mixing playing audio %d bytes, l %d u %d\n", stream_len, calls_since_lock_count, calls_since_unlock_count));
		}
		
		CDPlayer *player = currently_playing;
		
		if (player->audiostatus == CDROM_AUDIO_PLAY) {
			//D(bug("MixAudio cd playing, player=0x%p\n", player));
			uint8 *buf = fill_buffer(stream_len, player);
#if SDL_VERSION_ATLEAST(3, 0, 0)
			if (buf)
				SDL_PutAudioStreamData(player->stream, buf, stream_len);
			int avail = SDL_GetAudioStreamAvailable(player->stream);
			if (avail >= stream_len) {
				//D(bug("have bytes avail %d stream len %d\n", avail, stream_len));
				extern SDL_AudioSpec audio_spec;
				uint8 converted[stream_len];
				SDL_GetAudioStreamData(player->stream, converted, stream_len);
				float volume = (float)player->volume_mono/128;
				if (player->scanning) volume *= 0.6;
				SDL_MixAudio(stream, converted, audio_spec.format, stream_len, volume);
			}
#else
			if (buf)
				SDL_AudioStreamPut(player->stream, buf, stream_len);
			int avail = SDL_AudioStreamAvailable(player->stream);
			if (avail >= stream_len) {
				//D(bug("have bytes avail %d stream len %d\n", avail, stream_len));
				uint8 converted[stream_len];
				SDL_AudioStreamGet(player->stream, converted, stream_len);
				int volume = player->volume_mono;
				if (player->scanning) volume = volume * 3 / 5;
				SDL_MixAudio(stream, converted, stream_len, volume);
			}
#endif
		}
		
	}
	UNLOCK_BINCUE;
}

static void OpenPlayerStream(CDPlayer * player) {
	if (!have_current_output_settings) {
		player->stream = NULL;
		return;
	}
	OutputSettings & o = current_output_settings;

	// set player volume based on SDL volume
	player->volume_left = player->volume_right = player->volume_mono = o.default_cd_player_volume;
	// audio stream handles converting cd audio to destination output
	D(bug("Opening player stream\n"))
	D(bug(" player=0x%p volume=%d\n", player, player->volume_mono));
#if SDL_VERSION_ATLEAST(3, 0, 0)
	SDL_AudioSpec src = { player->cs->big_endian_audio? SDL_AUDIO_S16BE : SDL_AUDIO_S16LE, 2, 44100 };
	SDL_AudioSpec dst = { (SDL_AudioFormat)o.format, o.channels, o.freq };
	player->stream = SDL_CreateAudioStream(&src, &dst);
#else
	player->stream = SDL_NewAudioStream(player->cs->big_endian_audio? AUDIO_S16MSB : AUDIO_S16LSB, 2, 44100,
										o.format, o.channels, o.freq);
#endif
	if (player->stream == NULL) {
		D(bug("Failed to open CD player audio stream using SDL!\n"));
	}
	else {
		player->audio_enabled = true;
	}
}

static void ClosePlayerStream(CDPlayer * player)
{
#if !SDL_VERSION_ATLEAST(3, 0, 0)
#define SDL_DestroyAudioStream	SDL_FreeAudioStream
#endif
	if (player->stream) // if audiostream has been opened, free it as well
		SDL_DestroyAudioStream(player->stream);
	player->stream = NULL;
}

void OpenAudio_bincue(int freq, int format, int channels, uint8 silence, int volume)
{
	LOCK_BINCUE;
	// save output audio params
	current_output_settings = (OutputSettings){freq, format, channels, volume};
	have_current_output_settings = true;
	// setup silence at init
	silence_byte = silence;

	// init players for these settings
	for (std::list<CDPlayer*>::iterator it = players.begin(); it != players.end(); ++it)
	{
		CDPlayer *player = *it;
		OpenPlayerStream(player);
	}
	UNLOCK_BINCUE;
}

void CloseAudio_bincue() {
	LOCK_BINCUE;
	have_current_output_settings = false;
	for (std::list<CDPlayer*>::iterator it = players.begin(); it != players.end(); ++it)
	{
		CDPlayer *player = *it;
		ClosePlayerStream(player);
	}
	UNLOCK_BINCUE;
}
#endif

#ifdef OSX_CORE_AUDIO
static int bincue_core_audio_callback(void)
{
	for (std::list<CDPlayer*>::iterator it = players.begin(); it != players.end(); ++it)
	{
		CDPlayer *player = *it;
		
		int frames = player->soundoutput.bufferSizeFrames();
		uint8 *buf = fill_buffer(frames*4);

		//  D(bug("Audio request %d\n", stream_len));

		player->soundoutput.sendAudioBuffer((void *) buf, (buf ? frames : 0));

		return 1;
	}
}
#endif
