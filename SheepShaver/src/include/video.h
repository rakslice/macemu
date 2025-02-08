/*
 *  video.h - Video/graphics emulation
 *
 *  SheepShaver (C) 1997-2008 Marc Hellwig and Christian Bauer
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef VIDEO_H
#define VIDEO_H

#include <vector>

#ifndef NO_STD_NAMESPACE
using std::vector;
#endif


extern bool VideoActivated(void);
extern bool VideoSnapshot(int xsize, int ysize, uint8 *p);

extern int16 VideoDoDriverIO(uint32 spaceID, uint32 commandID, uint32 commandContents, uint32 commandCode, uint32 commandKind);

// System specific and internal functions/data
struct VideoInfo {
	int	viType;				// Screen/Window
	uint32 viRowBytes;		// width of each row in memory
	uint16 viXsize,viYsize;	// Window
	uint32 viAppleMode;		// Screen Color Depth
	uint32 viAppleID;		// Screen DisplayID
};

enum {	// viAppleMode
	APPLE_1_BIT = 0x80,
	APPLE_2_BIT,
	APPLE_4_BIT,
	APPLE_8_BIT,
	APPLE_16_BIT,
	APPLE_32_BIT
};

// 1, 2, 4 and 8 bit depths use a color palette
inline bool IsDirectMode(int mode)
{
	return mode == APPLE_16_BIT || mode == APPLE_32_BIT;
}

// 1, 2, 4 and 8 bit depths use a color palette
static inline bool IsDirectMode(VideoInfo const & mode)
{
	return IsDirectMode(mode.viAppleMode);
}

// Return the depth code that corresponds to the specified bits-per-pixel value
inline int DepthModeForPixelDepth(int depth)
{
	switch (depth) {
	case 1: return APPLE_1_BIT;
	case 2: return APPLE_2_BIT;
	case 4: return APPLE_4_BIT;
	case 8: return APPLE_8_BIT;
	case 15: case 16: return APPLE_16_BIT;
	case 24: case 32: return APPLE_32_BIT;
	default: return APPLE_1_BIT;
	}
}

// Return a bytes-per-row value (assuming enough bytes to fit the bits but no further padding) for the specified depth and pixel width
inline uint32 TrivialBytesPerRow(uint32 width, int mode)
{
	switch (mode) {
	case APPLE_1_BIT: return (width + 7) / 8;
	case APPLE_2_BIT: return (width + 3) / 4;
	case APPLE_4_BIT: return (width + 1) / 2;
	case APPLE_8_BIT: return width;
	case APPLE_16_BIT: return width * 2;
	case APPLE_32_BIT: return width * 4;
	default: return width;
	}
}

enum {	// viAppleID
	APPLE_640x480 = 0x81,
	APPLE_W_640x480,
	APPLE_800x600,
	APPLE_W_800x600,
	APPLE_1024x768,
	APPLE_1152x768,
	APPLE_1152x900,
	APPLE_1280x1024,
	APPLE_1600x1200,
	APPLE_CUSTOM,
	APPLE_ID_MIN = APPLE_640x480,
	APPLE_ID_MAX = APPLE_CUSTOM
};

enum {	// Display type
	DIS_INVALID,
	DIS_SCREEN,
	DIS_WINDOW,
	DIS_CHROMAKEY
};

extern bool video_activated;		// Flag: video display activated, mouse and keyboard data valid
extern int display_type;			// Current display type (see above)
extern rgb_color mac_pal[256];
extern rgb_color mac_gamma[256];
extern uint8 remap_mac_be[256];
extern uint8 MacCursor[68];

struct VidLocals{
	uint16	saveMode;
	uint32	saveData;
	uint16	savePage;
	uint32	saveBaseAddr;
	uint32	gammaTable;			// Mac address of gamma tble
	uint32	maxGammaTableSize;	// Biggest gamma table allocated
	uint32	saveVidParms;
	bool	luminanceMapping;	// Luminance mapping on/off
	bool	cursorHardware;		// True if using hardware cursor
	int32	cursorX;			// Hardware cursor state
	int32	cursorY;
	uint32	cursorVisible;
	uint32	cursorSet;
	bool	cursorHotFlag;
	uint8	cursorHotX;
	uint8	cursorHotY;
	uint32	vslServiceID;		// VSL interrupt service ID
	bool	interruptsEnabled;	// VBL interrupts on/off
	uint32	regEntryID;			// Mac address of the service owner
};

//extern VidLocals *private_data;	// Pointer to driver local variables (there is only one display, so this is ok)


// Color depth modes type
typedef int video_depth;

class monitor_desc {
public:

	monitor_desc(const vector<VideoInfo> &available_modes, video_depth default_depth, uint32 default_id);
	virtual ~monitor_desc() {}
	int16 getRefNum() const { return refNum; }
	void clear_data();
	bool init_data(uint32 commandContents);
	int16 video_open(uint32 pb);
	int16 video_close(uint32 pb);
	int16 video_status(uint32 pb);
	int16 video_control(uint32 pb);
	bool VideoSnapshot(int xsize, int ysize, uint8 *p);
	void setRefNum(int16 newRefNum) { refNum = newRefNum; }

	void setHardwareCursor(bool newVal) { if (csSave != NULL) { csSave->cursorHardware = newVal; } }

	void set_mode(int new_mode_index) { cur_mode = new_mode_index; }

	uint32 get_screen_base() { return screen_base; }

	bool interruptsEnabled() {
		if (csSave != NULL)
			return csSave->interruptsEnabled;
		else
			return false;
	}

	uint32 vslServiceID() {
		if (csSave != NULL)
			return csSave->vslServiceID;
		else
			return 0;
	}

	bool cursorVisible() {
		if (csSave == NULL)
			return false;
		else
			return csSave->cursorVisible;
	}

	// Get current Mac frame buffer base address
	uint32 get_mac_frame_base(void) const {return screen_base;}

	// Set Mac frame buffer base address (called from switch_to_mode())
	void set_mac_frame_base(uint32 base) {screen_base = base;}

	// Get current video mode
	const VideoInfo &get_current_mode(void) const {return modes[cur_mode];}

	// Look for a matching video mode, or -1 if not found
	int find_mode(uint32 appleMode, uint32 appleID) {
		int count = 0;
		for (vector<VideoInfo>::iterator i = modes.begin(); i != modes.end(); i++) {
			if (i->viAppleMode == appleMode && i->viAppleID == appleID)
				return count;
			count++;
		}
		return -1;
	}

	// Called by the video driver to switch the video mode on this display
	// (must call set_mac_frame_base())
	virtual void switch_to_current_mode(void) = 0;

	// Called by the video driver to set the color palette (in indexed modes)
	virtual void set_palette(uint8 *pal, int num) = 0;
	
	// Called by the video driver to set the gamma table
	virtual void set_gamma(uint8 *gamma, int num) = 0;

	bool has_mode(uint32 id) const;
	uint32 max_depth(uint32 id) const;
	void get_size_of_resolution(int id, uint32 &x, uint32 &y) const;

protected:
	vector<VideoInfo> modes;                         // List of supported video modes
	int16 refNum;		// Driver reference number
	VidLocals *csSave;	// Pointer to driver local variables

	uint32 screen_base = 0;				// Frame buffer base address
	int cur_mode;						// Number of current video mode (index in modes vector)
	int display_type = DIS_INVALID;		// Current display type
};

// Vector of pointers to available monitor descriptions, filled by VideoInit()
extern vector<monitor_desc *> VideoMonitors;

extern bool VideoInit(void);
extern void VideoExit(void);
extern void VideoVBL(void);
extern void VideoInstallAccel(void);
extern void VideoQuitFullScreen(void);

extern void video_set_palette(monitor_desc *);
extern void video_set_gamma(monitor_desc *, int n_colors);
extern void video_set_cursor(void);
extern bool video_can_change_cursor(void);
extern int16 video_mode_change(monitor_desc * monitor, VidLocals *csSave, uint32 ParamPtr);
extern void video_set_dirty_area(int x, int y, int w, int h);

extern int16 VSLDoInterruptService(uint32 arg1);
extern void NQDMisc(uint32 arg1, uintptr arg2);

// Native QuickDraw acceleration callbacks
extern bool NQD_sync_hook(uint32);
extern bool NQD_bitblt_hook(uint32);
extern bool NQD_fillrect_hook(uint32);
extern bool NQD_unknown_hook(uint32);
extern void NQD_bitblt(uint32);
extern void NQD_invrect(uint32);
extern void NQD_fillrect(uint32);

extern bool keyfile_valid;

#endif
