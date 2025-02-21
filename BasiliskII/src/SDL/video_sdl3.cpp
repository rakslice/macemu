/*
 *  video_sdl3.cpp - Video/graphics emulation, SDL 3.x specific stuff
 *
 *  Basilisk II (C) 1997-2008 Christian Bauer
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

/*
 *  NOTES:
 *    The Ctrl key works like a qualifier for special actions:
 *      Ctrl-Tab = suspend DGA mode (TODO)
 *      Ctrl-Esc = emergency quit
 *      Ctrl-F1 = mount floppy
 *      Ctrl-F5 = grab mouse (in windowed mode)
 *
 *  FIXMEs and TODOs:
 *  - Windows requires an extra mouse event to update the actual cursor image?
 *  - Ctr-Tab for suspend/resume but how? SDL does not support that for non-Linux
 *  - Ctrl-Fn doesn't generate SDL_KEYDOWN events (SDL bug?)
 *  - Mouse acceleration, there is no API in SDL yet for that
 *  - Gamma tables support is likely to be broken here
 *  - Events processing is bound to the general emulation thread as SDL requires
 *    to PumpEvents() within the same thread as the one that called SetVideoMode().
 *    Besides, there can't seem to be a way to call SetVideoMode() from a child thread.
 *  - Backport hw cursor acceleration to Basilisk II?
 *  - Factor out code
 */

#include "sysdeps.h"

#include <SDL.h>
#if SDL_VERSION_ATLEAST(3, 0, 0)

#include <errno.h>
#include <vector>
#include <string>
#include <math.h>
#include <inttypes.h>

#ifdef SDL_PLATFORM_MACOS
#include "utils_macosx.h"
#endif

#ifdef WIN32
#include <malloc.h> /* alloca() */
#endif

#include <cpu_emulation.h>
#include "main.h"
#include "adb.h"
#include "macos_util.h"
#include "prefs.h"
#include "user_strings.h"
#include "video.h"
#include "video_defs.h"
#include "video_blit.h"
#include "vm_alloc.h"
#include "cdrom.h"

#define DEBUG 0
#include "debug.h"

#define CODE_INVALID -1
#define CODE_HOTKEY  -2

using std::vector;

// Display types
#ifdef SHEEPSHAVER
enum {
	DISPLAY_WINDOW = DIS_WINDOW,					// windowed display
	DISPLAY_SCREEN = DIS_SCREEN,					// fullscreen display
	DISPLAY_CHROMAKEY = DIS_CHROMAKEY
};
#else
enum {
	DISPLAY_WINDOW,									// windowed display
	DISPLAY_SCREEN,									// fullscreen display
	DISPLAY_CHROMAKEY
};
#endif

// Display clone related state
struct DisplayClone {
	DisplayClone(SDL_Window * w, SDL_Renderer * r, SDL_Texture * t, int d) : window(w), renderer(r), texture(t), display_num(d) { }
	SDL_Window * window;
	SDL_Renderer * renderer;
	SDL_Texture * texture;
	int display_num;
};

// Constants
#if defined(SDL_PLATFORM_MACOS) || defined(WIN32)
const char KEYCODE_FILE_NAME[] = "keycodes";
const char KEYCODE_FILE_NAME2[] = "BasiliskII_keycodes";
#else
const char KEYCODE_FILE_NAME[] = DATADIR "/keycodes";
const char KEYCODE_FILE_NAME2[] = DATADIR "/BasiliskII_keycodes";
#endif


// Global variables
static uint32 frame_skip;							// Prefs items
static int16 mouse_wheel_mode;
static int16 mouse_wheel_lines;
static bool mouse_wheel_reverse;

#ifdef ENABLE_VOSF
static bool use_vosf = false;						// Flag: VOSF enabled
#else
static const bool use_vosf = false;					// VOSF not possible
#endif

static bool ctrl_down = false;						// Flag: Ctrl key pressed (for use with hotkeys)
static bool opt_down = false;						// Flag: Opt/Alt key pressed (for use with hotkeys)
static bool cmd_down = false;						// Flag: Cmd/Super/Win key pressed (for use with hotkeys)
static bool quit_full_screen = false;				// Flag: DGA close requested from redraw thread
static bool emerg_quit = false;						// Flag: Ctrl-Esc pressed, emergency quit requested from MacOS thread
static bool emul_suspended = false;					// Flag: Emulator suspended

static bool classic_mode = false;					// Flag: Classic Mac video mode

static bool use_keycodes = false;					// Flag: Use keycodes rather than keysyms
static int keycode_table[256];						// X keycode -> Mac keycode translation table

class driver_base;
class SDL_monitor_desc;

// SDL variables
class SDLDisplayInstance {
public:
	bool init_locks();
	void set_window_name();
	void delete_sdl_video_window();
	void delete_sdl_video_surfaces();
	void shutdown_sdl_video();
	SDL_Surface * init_sdl_video(int width, int height, int depth, Uint32 flags, int pitch);
	int present_sdl_video();
	void update_sdl_video(SDL_Surface *s, int numrects, SDL_Rect *rects);
	void update_sdl_video(SDL_Surface *s, Sint32 x, Sint32 y, Sint32 w, Sint32 h);
	void update_mouse_grab();
	void do_toggle_fullscreen(void);
	bool is_fullscreen(SDL_Window * window);
	void interrupt_time();
	bool has_window_id(SDL_WindowID win_id) const;
	inline void handle_palette_changes(void);
	void clear_buffers();
	void init_buffers(int pitch, int aligned_height, int monitor_desc_num);
	void release_buffers();
	void destroy_locks();
	void force_complete_window_refresh();

	void VideoRefresh();
	void do_video_refresh();
	int instance_redraw_func();

#ifdef SHEEPSHAVER
	SDL_Cursor *MagCursor(bool hot);
	void create_cursor();
	bool is_cursor_in_display();
	void set_cursor();
#endif

	SDLDisplayInstance() : sdl_palette(NULL),
							sdl_palette_changed(false),
							sdl_update_video_mutex(NULL),
							tick_counter(0),
							Screen_blit(0),
							frame_buffer_lock(NULL),
							sdl_window(NULL),
							host_surface(NULL),
							guest_surface(NULL),
							sdl_renderer(NULL),
							sdl_renderer_thread_id(0),
							sdl_texture(NULL),
							redraw_thread_active(false),
#ifndef USE_CPU_EMUL_SERVICES
							redraw_thread(NULL),
							thread_stop_req(false),
							thread_stop_ack(false),
#endif
#ifdef SHEEPSHAVER
							sdl_cursor(NULL),
#endif
							toggle_fullscreen(false),
							_drv(NULL),
							the_buffer(NULL),
							the_buffer_copy(NULL),
							display_type(DISPLAY_WINDOW)
	{
		sdl_update_video_rect.x = 0;
		sdl_update_video_rect.y = 0;
		sdl_update_video_rect.w = 0;
		sdl_update_video_rect.h = 0;
	}

	SDL_Renderer * renderer() const { return sdl_renderer; }

	SDL_Palette *sdl_palette;				// Color palette to be used as CLUT and gamma table
	bool sdl_palette_changed;			// Flag: Palette changed, redraw thread must set new colors

	SDL_Rect sdl_update_video_rect;  // Union of all rects to update, when updating sdl_texture
	SDL_Mutex * sdl_update_video_mutex;   // Mutex to protect sdl_update_video_rect

	void set_driver_base(driver_base * new_drv) { _drv = new_drv; }
	driver_base * drv() const { return _drv; }
	SDL_Window * get_sdl_window() { return sdl_window; }
	void set_toggle_fullscreen() { toggle_fullscreen = true; }
	uint8 *host_buffer() const { return the_buffer; }
	uint8 *host_buffer_copy() const { return the_buffer_copy; }

	int get_display_type() const { return display_type; }
	void set_display_type(int new_display_type) { display_type = new_display_type; }

	void LOCK_FRAME_BUFFER() { SDL_LockMutex(frame_buffer_lock); }
	void UNLOCK_FRAME_BUFFER() { SDL_UnlockMutex(frame_buffer_lock); }

	bool start_redraw();
	void stop_redraw();

	void pause_redraw();
	void resume_redraw();

	uint32 tick_counter;

	Screen_blit_func Screen_blit;
protected:
	// Mutex to protect frame buffer
	SDL_Mutex *frame_buffer_lock;

	SDL_Window * sdl_window;              // Wraps an OS-native window
	SDL_Surface * host_surface;           // Surface in host-OS display format
	SDL_Surface * guest_surface;          // Surface in guest-OS display format
	SDL_Renderer * sdl_renderer;          // Handle to SDL2 renderer
	SDL_ThreadID sdl_renderer_thread_id;  // Thread ID where the SDL_renderer was created, and SDL_renderer ops should run (for compatibility w/ d3d9)
	SDL_Texture * sdl_texture;            // Handle to a GPU texture, with which to draw guest_surface to
	//int screen_depth;                   // Depth of current screen

	bool redraw_thread_active;            // Flag: Redraw thread installed
#ifndef USE_CPU_EMUL_SERVICES
	volatile bool redraw_thread_cancel;   // Flag: Cancel Redraw thread
	SDL_Thread *redraw_thread;            // Redraw thread
	volatile bool thread_stop_req;
	volatile bool thread_stop_ack;        // Acknowledge for thread_stop_req
#endif

#ifdef SHEEPSHAVER
	SDL_Cursor *sdl_cursor;               // Copy of Mac cursor
#endif
	bool toggle_fullscreen;
	driver_base *_drv;


	uint8 *the_buffer;                    // Mac frame buffer (where MacOS draws into)
	uint8 *the_buffer_copy;               // Copy of Mac frame buffer (for refreshed modes)
	uint32 the_buffer_size;               // Size of allocated the_buffer

	int display_type;                     // See Display Types enum above
};

static bool did_add_event_watch = false;

vector <DisplayClone> clones;

static bool mouse_grabbed = false;

// Mutex to protect SDL events
static SDL_Mutex *sdl_events_lock = NULL;
#define LOCK_EVENTS SDL_LockMutex(sdl_events_lock)
#define UNLOCK_EVENTS SDL_UnlockMutex(sdl_events_lock)

// Mutex to protect palette
static SDL_Mutex *sdl_palette_lock = NULL;
#define LOCK_PALETTE SDL_LockMutex(sdl_palette_lock)
#define UNLOCK_PALETTE SDL_UnlockMutex(sdl_palette_lock)

// Previously set gamma tables
static uint16 last_gamma_red[256];
static uint16 last_gamma_green[256];
static uint16 last_gamma_blue[256];

// Video refresh function
static void VideoRefreshInit(void);
static void (*video_refresh)(SDL_monitor_desc * sdm );


// Prototypes
static int redraw_func(void *arg);
static bool SDLCALL on_sdl_event_generated(void *userdata, SDL_Event *event);
static bool is_fullscreen(SDL_Window *);
static SDLDisplayInstance * display_instance_for_windowID(SDL_WindowID win_id);
static driver_base * first_drv();
static void toggle_all_fullscreen();
static void do_power_key_shutdown();
static void release_hotkey();
static std::string monitor_ident(SDLDisplayInstance & monitor);

// From sys_unix.cpp
extern void SysMountFirstFloppy(void);


/*
 *  SDL surface locking glue
 */

#ifdef ENABLE_VOSF
#define SDL_VIDEO_LOCK_VOSF_SURFACE(SURFACE) do {				\
	if (sdl_window && SDL_GetWindowFlags(sdl_window) & (SDL_WINDOW_FULLSCREEN))	\
		the_host_buffer = (uint8 *)(SURFACE)->pixels;			\
} while (0)
#else
#define SDL_VIDEO_LOCK_VOSF_SURFACE(SURFACE)
#endif

#define SDL_VIDEO_LOCK_SURFACE(SURFACE) do {	\
	if (SDL_MUSTLOCK(SURFACE)) {				\
		SDL_LockSurface(SURFACE);				\
		SDL_VIDEO_LOCK_VOSF_SURFACE(SURFACE);	\
	}											\
} while (0)

#define SDL_VIDEO_UNLOCK_SURFACE(SURFACE) do {	\
	if (SDL_MUSTLOCK(SURFACE))					\
		SDL_UnlockSurface(SURFACE);				\
} while (0)

static void * permanent_fb = VM_MAP_FAILED;

#define USE_PERMANENT_FB 1
#define MAX_SCREENS 2
static const size_t RESERVED_SIZE = 80 * 1024 * 1024; // for 6K Retina

/*
 *  Framebuffer allocation routines
 */

static void *vm_acquire_framebuffer(uint32 size, int monitor_desc_num)
{
#if defined(HAVE_MACH_VM) || defined(HAVE_MMAP_VM) && defined(__aarch64__)
	return vm_acquire_reserved(size);
#elif USE_PERMANENT_FB
	if (monitor_desc_num >= MAX_SCREENS) return VM_MAP_FAILED;
	if (size > RESERVED_SIZE) return VM_MAP_FAILED;
	if (permanent_fb == VM_MAP_FAILED) {
		permanent_fb = vm_acquire(RESERVED_SIZE * MAX_SCREENS, VM_MAP_DEFAULT | VM_MAP_32BIT);
		if (permanent_fb == VM_MAP_FAILED) return VM_MAP_FAILED;
	}
	return (void *)((uintptr)permanent_fb + monitor_desc_num * RESERVED_SIZE);
#else
	// always try to reallocate framebuffer at the same address
	static void *fb = VM_MAP_FAILED;
	if (fb != VM_MAP_FAILED) {
		if (vm_acquire_fixed(fb, size) < 0) {
#ifndef SHEEPSHAVER
			printf("FATAL: Could not reallocate framebuffer at previous address\n");
#endif
			fb = VM_MAP_FAILED;
		}
	}
	if (fb == VM_MAP_FAILED)
		fb = vm_acquire(size, VM_MAP_DEFAULT | VM_MAP_32BIT);
	return fb;
#endif
}

static inline void vm_release_framebuffer(void *fb, uint32 size)
{
#if !((defined(HAVE_MACH_VM) || defined(HAVE_MMAP_VM) && defined(__aarch64__))) && !USE_PERMANENT_FB
	vm_release(fb, size);
#endif
}

static inline int get_customized_color_depth(int default_depth)
{
	int display_color_depth = PrefsFindInt32("displaycolordepth");

	D(bug("Get displaycolordepth %d\n", display_color_depth));

	if(0 == display_color_depth)
		return default_depth;
	else{
		switch (display_color_depth) {
		case 8:
			return VIDEO_DEPTH_8BIT;
		case 15: case 16:
			return VIDEO_DEPTH_16BIT;
		case 24: case 32:
			return VIDEO_DEPTH_32BIT;
		default:
			return default_depth;
		}
	}
}

/*
 *  Windows message handler
 */

#ifdef WIN32
#include <dbt.h>
static WNDPROC sdl_window_proc = NULL;				// Window proc used by SDL

extern void SysMediaArrived(void);
extern void SysMediaRemoved(void);
extern HWND GetMainWindowHandle(void);

static LRESULT CALLBACK windows_message_handler(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch (msg) {
	case WM_DEVICECHANGE:
		if (wParam == DBT_DEVICEREMOVECOMPLETE) {
			DEV_BROADCAST_HDR *p = (DEV_BROADCAST_HDR *)lParam;
			if (p->dbch_devicetype == DBT_DEVTYP_VOLUME)
				SysMediaRemoved();
		}
		else if (wParam == DBT_DEVICEARRIVAL) {
			DEV_BROADCAST_HDR *p = (DEV_BROADCAST_HDR *)lParam;
			if (p->dbch_devicetype == DBT_DEVTYP_VOLUME)
				SysMediaArrived();
		}
		return 0;

	default:
		if (sdl_window_proc)
			return CallWindowProc(sdl_window_proc, hwnd, msg, wParam, lParam);
	}

	return DefWindowProc(hwnd, msg, wParam, lParam);
}
#endif


/*
 *  SheepShaver glue
 */

#ifdef SHEEPSHAVER

// Find Apple mode matching best specified dimensions
static int find_apple_resolution(int xsize, int ysize)
{
	if (xsize == 640 && ysize == 480)
		return APPLE_640x480;
	if (xsize == 800 && ysize == 600)
		return APPLE_800x600;
	if (xsize == 1024 && ysize == 768)
		return APPLE_1024x768;
	if (xsize == 1152 && ysize == 768)
		return APPLE_1152x768;
	if (xsize == 1152 && ysize == 900)
		return APPLE_1152x900;
	if (xsize == 1280 && ysize == 1024)
		return APPLE_1280x1024;
	if (xsize == 1600 && ysize == 1200)
		return APPLE_1600x1200;
	return APPLE_CUSTOM;
}

// Display error alert
static void ErrorAlert(int error)
{
	ErrorAlert(GetString(error));
}
#endif

/*
 *  monitor_desc subclass for SDL display
 */

class SDL_monitor_desc : public monitor_desc {
public:
	SDLDisplayInstance sdl_display;

	SDL_monitor_desc(const vector<VIDEO_MODE> &available_modes, video_depth default_depth, uint32 default_id, int monitor_desc_num) : monitor_desc(available_modes, default_depth, default_id),
																																	  monitor_desc_num(monitor_desc_num),
																																	  drv(NULL) { }
	~SDL_monitor_desc() {}

	virtual void switch_to_current_mode(void);
	virtual void set_palette(uint8 *pal, int num);
	virtual void set_gamma(uint8 *gamma, int num);

	bool video_open(void);
	void video_close(void);
	void emergency_stop();
	int get_display_num_pref();
	int display_instance_num() const { return monitor_desc_num; }
protected:
	driver_base *drv;	// Pointer to currently used driver object
	int monitor_desc_num;
};


/*
 *  Utility functions
 */

#ifdef SHEEPSHAVER
// Find palette size for given color depth
static int palette_size(int mode)
{
	switch (mode) {
	case VIDEO_DEPTH_1BIT: return 2;
	case VIDEO_DEPTH_2BIT: return 4;
	case VIDEO_DEPTH_4BIT: return 16;
	case VIDEO_DEPTH_8BIT: return 256;
	case VIDEO_DEPTH_16BIT: return 32;
	case VIDEO_DEPTH_32BIT: return 256;
	default: return 0;
	}
}
#endif

// Map video_mode depth ID to numerical depth value
static int mac_depth_of_video_depth(int video_depth)
{
	int depth = -1;
	switch (video_depth) {
	case VIDEO_DEPTH_1BIT:
		depth = 1;
		break;
	case VIDEO_DEPTH_2BIT:
		depth = 2;
		break;
	case VIDEO_DEPTH_4BIT:
		depth = 4;
		break;
	case VIDEO_DEPTH_8BIT:
		depth = 8;
		break;
	case VIDEO_DEPTH_16BIT:
		depth = 16;
		break;
	case VIDEO_DEPTH_32BIT:
		depth = 32;
		break;
	default:
		abort();
	}
	return depth;
}

// Map video_mode depth ID to SDL screen depth
static int sdl_depth_of_video_depth(int video_depth)
{
	return (video_depth <= VIDEO_DEPTH_8BIT) ? 8 : mac_depth_of_video_depth(video_depth);
}

// Get the display num for the given SDL display ID
// Returns 1-based display index of the display, or 0 if the display was not found.
static int display_num_for_id(SDL_DisplayID displayID) {
	if (displayID == 0) return 0;

	int num_displays;
	SDL_DisplayID * displays = SDL_GetDisplays(&num_displays);
	for (int i = 0; i < num_displays; i++) {
		if (displays[i] == displayID) {
			return i + 1;
		}
	}

	return 0;
}

// Get screen dimensions
static void sdl_display_dimensions(int display_num, int &width, int &height)
{
	SDL_Rect rect = { 0 };

	SDL_DisplayID displayID;

	// For the purposes of display_num values from prefs, displays start from 1

	if (display_num < 1) {
		displayID = SDL_GetPrimaryDisplay();
	} else {
		int display_count;
		SDL_DisplayID * displays = SDL_GetDisplays(&display_count);
		if (display_num - 1 < display_count) {
			displayID = displays[display_num - 1];
		} else {
			displayID = SDL_GetPrimaryDisplay();
		}
	}
	if (displayID) SDL_GetDisplayBounds(displayID, &rect);
	width = rect.w;
	height = rect.h;
}

static inline int sdl_display_width(int display_num)
{
	int width, height;
	sdl_display_dimensions(display_num, width, height);
	return width;
}

static inline int sdl_display_height(int display_num)
{
	int width, height;
	sdl_display_dimensions(display_num, width, height);
	return height;
}

// Check whether specified mode is available
static bool has_mode(int display_num, int type, int width, int height, int depth)
{
	// Filter out out-of-bounds resolutions
	if (width > sdl_display_width(display_num) || height > sdl_display_height(display_num))
		return false;

	// Whatever size it is, beyond what we've checked, we'll scale to/from as appropriate.
	return true;
}

// Add mode to list of supported modes
static void add_mode(vector<VIDEO_MODE> & VideoModes, int display_num, int type, int width, int height, int resolution_id, int bytes_per_row, int depth, int custom_id = 0)
{
	// Filter out unsupported modes
	if (!has_mode(display_num, type, width, height, depth))
		return;

	// Fill in VideoMode entry
	VIDEO_MODE mode;
#ifdef SHEEPSHAVER
	resolution_id = find_apple_resolution(width, height);
	if (resolution_id == APPLE_CUSTOM && custom_id != 0)
		resolution_id = custom_id;
	mode.viType = type;
#endif
	VIDEO_MODE_X = width;
	VIDEO_MODE_Y = height;
	VIDEO_MODE_RESOLUTION = resolution_id;
	VIDEO_MODE_ROW_BYTES = bytes_per_row;
	VIDEO_MODE_DEPTH = (video_depth)depth;
	VideoModes.push_back(mode);
}

// Set Mac frame layout and base address (uses the_buffer/MacFrameBaseMac)
static void set_mac_frame_buffer(SDL_monitor_desc &monitor, int depth, bool native_byte_order)
{
#if !REAL_ADDRESSING && !DIRECT_ADDRESSING
	int layout = FLAYOUT_DIRECT;
	if (depth == VIDEO_DEPTH_16BIT)
		layout = (screen_depth == 15) ? FLAYOUT_HOST_555 : FLAYOUT_HOST_565;
	else if (depth == VIDEO_DEPTH_32BIT)
		layout = (screen_depth == 24) ? FLAYOUT_HOST_888 : FLAYOUT_DIRECT;
	if (native_byte_order)
		MacFrameLayout = layout;
	else
		MacFrameLayout = FLAYOUT_DIRECT;
	monitor.set_mac_frame_base(MacFrameBaseMac);

	// Set variables used by UAE memory banking
	const VIDEO_MODE &mode = monitor.get_current_mode();
	MacFrameBaseHost = the_buffer;
	MacFrameSize = VIDEO_MODE_ROW_BYTES * VIDEO_MODE_Y;
	InitFrameBufferMapping();
#else
	monitor.set_mac_frame_base(Host2MacAddr(monitor.sdl_display.host_buffer())); // the_buffer
#endif
	D(bug("monitor.mac_frame_base = %08x\n", monitor.get_mac_frame_base()));
}

// Set window name and class
void SDLDisplayInstance::set_window_name() {
	if (!sdl_window) return;

	bool multi = VideoMonitors.size() > 1;

	const char *title = PrefsFindString("title");
	std::string s = title ? title : GetString(STR_WINDOW_TITLE);
	if (multi)
	{
		s += " - Monitor " + monitor_ident(*this);
	}
    if (mouse_grabbed)
    {
        s += GetString(STR_WINDOW_TITLE_GRABBED_PRE);
		int hotkey = PrefsFindInt32("hotkey");
		hotkey = hotkey ? hotkey : 1;
		if (hotkey & 1) s += GetString(STR_WINDOW_TITLE_GRABBED1);
        if (hotkey & 2) s += GetString(STR_WINDOW_TITLE_GRABBED2);
        if (hotkey & 4) s += GetString(STR_WINDOW_TITLE_GRABBED4);
        s += GetString(STR_WINDOW_TITLE_GRABBED_POST);
	}
	SDL_SetWindowTitle(sdl_window, s.c_str());
}

// Migrate preferences items (XXX to be handled in MigratePrefs())
static void migrate_screen_prefs(void)
{
#ifdef SHEEPSHAVER
	// Look-up priorities are: "screen", "screenmodes", "windowmodes".
	if (PrefsFindString("screen"))
		return;

	uint32 window_modes = PrefsFindInt32("windowmodes");
	uint32 screen_modes = PrefsFindInt32("screenmodes");
	int width = 0, height = 0;
	if (screen_modes) {
		static const struct {
			int id;
			int width;
			int height;
		}
		modes[] = {
			{  1,	 640,	 480 },
			{  2,	 800,	 600 },
			{  4,	1024,	 768 },
			{ 64,	1152,	 768 },
			{  8,	1152,	 900 },
			{ 16,	1280,	1024 },
			{ 32,	1600,	1200 },
			{ 0, }
		};
		for (int i = 0; modes[i].id != 0; i++) {
			if (screen_modes & modes[i].id) {
				if (width < modes[i].width && height < modes[i].height) {
					width = modes[i].width;
					height = modes[i].height;
				}
			}
		}
	} else {
		if (window_modes & 1)
			width = 640, height = 480;
		if (window_modes & 2)
			width = 800, height = 600;
	}
	if (width && height) {
		char str[32];
		sprintf(str, "%s/%d/%d", screen_modes ? "dga" : "win", width, height);
		PrefsReplaceString("screen", str);
	}
#endif
}


/*
 *  Display "driver" classes
 */

class driver_base {
public:
	driver_base(SDL_monitor_desc &m);
	~driver_base();

	void init(int monitor_desc_num); // One-time init
	void set_video_mode(int flags, int pitch);
	void adapt_to_video_mode();

	void update_palette(void);
	void suspend(void) {}
	void resume(void) {}
	void toggle_mouse_grab(void);
	void mouse_moved(int x, int y) {
		int display_num;
		display_num = monitor.getRefNum();
		ADBMouseMoved(x, y, display_num);
	}

	void disable_mouse_accel(void);
	void restore_mouse_accel(void);

	void grab_mouse(void);
	void ungrab_mouse(void);

public:
	SDL_monitor_desc &monitor; // Associated video monitor
	const VIDEO_MODE &mode;    // Video mode handled by the driver

	bool init_ok;	// Initialization succeeded (we can't use exceptions because of -fomit-frame-pointer)
	SDL_Surface *s;	// The surface we draw into
};

#ifdef ENABLE_VOSF
static void update_display_window_vosf(driver_base *drv);
#endif
static void update_display_static(driver_base *drv);

#ifdef ENABLE_VOSF
# include "video_vosf.h"
#endif

driver_base::driver_base(SDL_monitor_desc &m)
	: monitor(m), mode(m.get_current_mode()), init_ok(false), s(NULL)
{
	m.sdl_display.clear_buffers();
}

static std::string monitor_ident(SDLDisplayInstance & sdi)
{
	std::string out;
	char buf[10];
	snprintf(buf, 10, "%d", sdi.drv()->monitor.display_instance_num() + 1);
	out += buf;
	return out;
}

void SDLDisplayInstance::clear_buffers() {
	the_buffer = NULL;
	the_buffer_copy = NULL;
}

void SDLDisplayInstance::delete_sdl_video_surfaces()
{
	if (sdl_texture) {
		SDL_DestroyTexture(sdl_texture);
		sdl_texture = NULL;
	}

	for (vector<DisplayClone>::iterator i = clones.begin(); i != clones.end(); i++) {
		if (i->texture) {
			SDL_DestroyTexture(sdl_texture);
			i->texture = NULL;
		}
	}

	if (host_surface) {
		if (host_surface == guest_surface) {
			guest_surface = NULL;
		}
		
		SDL_DestroySurface(host_surface);
		host_surface = NULL;
	}
	
	if (guest_surface) {
		SDL_DestroySurface(guest_surface);
		guest_surface = NULL;
	}
}

void SDLDisplayInstance::delete_sdl_video_window()
{
	if (sdl_renderer) {
		SDL_DestroyRenderer(sdl_renderer);
		sdl_renderer = NULL;
	}
	
	if (sdl_window) {
		SDL_DestroyWindow(sdl_window);
		sdl_window = NULL;
	}

	for (vector<DisplayClone>::iterator i = clones.begin(); i != clones.end(); i++) {
		if (i->renderer) {
			SDL_DestroyRenderer(i->renderer);
			i->renderer = NULL;
		}
		if (i->window) {
			SDL_DestroyWindow(i->window);
			i->window = NULL;
		}
	}
	clones.clear();
}

void SDLDisplayInstance::shutdown_sdl_video()
{
	delete_sdl_video_surfaces();
	delete_sdl_video_window();
}

static float get_mag_rate()
{
	const char *s = PrefsFindString("mag_rate");
	float m;
	if (s == NULL || sscanf(s, "%f", &m) != 1) return 1;
	return m < 1 ? 1 : m > 4 ? 4 : m;
}

int SDL_monitor_desc::get_display_num_pref() {
	if (monitor_desc_num == 0)
		return PrefsFindInt32("display_num");
	else
		return PrefsFindInt32("add_display");

}

SDL_Surface * SDLDisplayInstance::init_sdl_video(int width, int height, int depth, Uint32 flags, int pitch)
{
    if (guest_surface) {
        delete_sdl_video_surfaces();
    }
    
	bool want_fullscreen = (flags & SDL_WINDOW_FULLSCREEN) != 0;

	float m = get_mag_rate();

	int window_width = width * m;
	int window_height = height * m;

	SDL_DisplayID old_window_display_id = 0;
	if (sdl_window) {
		old_window_display_id = SDL_GetDisplayForWindow(sdl_window);
	}

	int display_num = drv()->monitor.get_display_num_pref();
	D(bug("configured display num %d\n", display_num));
	// For the purposes of this pref, displays start from 1

	if ((display_num < 1) && (old_window_display_id != 0)) {
		// prefs display is unspecified but keep the window on the display it's on
		display_num = display_num_for_id(old_window_display_id);
	}

	if (want_fullscreen) {
		window_width = sdl_display_width(display_num);
		window_height = sdl_display_height(display_num);
	}

	if (sdl_window) {
		int old_window_width, old_window_height, old_window_flags;
		SDL_GetWindowSize(sdl_window, &old_window_width, &old_window_height);
		old_window_flags = SDL_GetWindowFlags(sdl_window);
		bool old_fullscreen = ((old_window_flags & SDL_WINDOW_FULLSCREEN) != 0);
		D(bug("Checking if we can reuse existing window\n"));
		D(bug("Existing: %dx%d fs %s (flags 0x%x), want: %dx%d fs %s\n",
			old_window_width, old_window_height, (old_fullscreen? "yes": "no"), old_window_flags,
			window_width, window_height, (want_fullscreen? "yes": "no")
			));

		if ((old_window_width != window_width ||
			old_window_height != window_height)
			 && !want_fullscreen)
		{
			// If we are in windowed mode and the resolution is changing
			// we need a window at a new size, or else the scale will change
			D(bug("Can't reuse old window\n"));
			delete_sdl_video_window();
		} else {
			D(bug("Reusing old window\n"));
		}
	}
	
	if (!sdl_window) {
#ifdef VIDEO_CHROMAKEY
		if (display_type == DISPLAY_CHROMAKEY) {
			SDL_SetHint("SDL_MOUSE_FOCUS_CLICKTHROUGH", "1");
		}
#endif

		int x, y;
		if (display_num < 1) {
			x = SDL_WINDOWPOS_UNDEFINED;
			y = SDL_WINDOWPOS_UNDEFINED;
		} else {
			// In SDL 3 the display indexes used with the special window position macros appear to start from 1
			x = SDL_WINDOWPOS_UNDEFINED_DISPLAY(display_num);
			y = SDL_WINDOWPOS_UNDEFINED_DISPLAY(display_num);
		}

		SDL_PropertiesID sdl_props = SDL_CreateProperties();
		if(sdl_props == 0) {
			SDL_Log("Unable to create properties: %s", SDL_GetError());
			return NULL;
		}

		if (SDL_SetStringProperty(sdl_props, SDL_PROP_WINDOW_CREATE_TITLE_STRING, "") &&
			SDL_SetNumberProperty(sdl_props, SDL_PROP_WINDOW_CREATE_WIDTH_NUMBER, window_width) &&
			SDL_SetNumberProperty(sdl_props, SDL_PROP_WINDOW_CREATE_HEIGHT_NUMBER, window_height) &&
			SDL_SetNumberProperty(sdl_props, SDL_PROP_WINDOW_CREATE_X_NUMBER, x) &&
			SDL_SetNumberProperty(sdl_props, SDL_PROP_WINDOW_CREATE_Y_NUMBER, y) &&
#ifdef VIDEO_CHROMAKEY
			SDL_SetBooleanProperty(sdl_props, SDL_PROP_WINDOW_CREATE_BORDERLESS_BOOLEAN, display_type == DISPLAY_CHROMAKEY) &&
			SDL_SetBooleanProperty(sdl_props, SDL_PROP_WINDOW_CREATE_TRANSPARENT_BOOLEAN, display_type == DISPLAY_CHROMAKEY) &&
#endif
			SDL_SetBooleanProperty(sdl_props, SDL_PROP_WINDOW_CREATE_HIGH_PIXEL_DENSITY_BOOLEAN, true) &&
#ifdef SDL_PLATFORM_MACOS
			SDL_SetBooleanProperty(sdl_props, SDL_PROP_WINDOW_CREATE_METAL_BOOLEAN, MetalIsAvailable()) &&
#endif
			SDL_SetBooleanProperty(sdl_props, SDL_PROP_WINDOW_CREATE_FULLSCREEN_BOOLEAN, (flags & SDL_WINDOW_FULLSCREEN) != 0)) {

			D(bug("Creating window %dx%d fs %d\n", window_width, window_height, (flags & SDL_WINDOW_FULLSCREEN) != 0));
			sdl_window = SDL_CreateWindowWithProperties(sdl_props);

			if (!sdl_window) {
				shutdown_sdl_video();
				return NULL;
			}

			D(bug("Flags 0x%lx\n", SDL_GetWindowFlags(sdl_window)));
		} else {
			// property setting error
			SDL_Log("Unable to set properties: %s", SDL_GetError());
			SDL_DestroyProperties(sdl_props);
			return NULL;
		}
		SDL_SyncWindow(sdl_window); // needed for fullscreen

		int clone_display_num = PrefsFindInt32("clone_to");
		if (clone_display_num >= 1) {
			// For the purposes of this pref display numbers start from 1

			int x, y;
			// SDL 3, 1-based
			x = SDL_WINDOWPOS_UNDEFINED_DISPLAY(clone_display_num);
			y = SDL_WINDOWPOS_UNDEFINED_DISPLAY(clone_display_num);

			if (SDL_SetNumberProperty(sdl_props, SDL_PROP_WINDOW_CREATE_X_NUMBER, x) &&
				SDL_SetNumberProperty(sdl_props, SDL_PROP_WINDOW_CREATE_Y_NUMBER, y)) {

				SDL_Window * clone_window = SDL_CreateWindowWithProperties(sdl_props);
				if (clone_window) {
					clones.push_back(DisplayClone(clone_window, NULL, NULL, clone_display_num));
					SDL_SyncWindow(clone_window); // needed for fullscreen
				} else {
					D(bug("Error creating clone SDL window\n"));
				}
			}
		}

		SDL_DestroyProperties(sdl_props);

		set_window_name();
	}
	
	// Some SDL events (regarding some native-window events), need processing
	// as they are generated.  SDL2 has a facility, SDL_AddEventWatch(), which
	// allows events to be processed as they are generated.
	if (!did_add_event_watch) {
		SDL_AddEventWatch(&on_sdl_event_generated, NULL);
		did_add_event_watch = true;
	}

	if (!sdl_renderer) {
		const char *render_driver = PrefsFindString("sdlrender");
		if (render_driver) {
			SDL_SetHint(SDL_HINT_RENDER_DRIVER, render_driver);
		}
		else {
#ifdef WIN32
			SDL_SetHint(SDL_HINT_RENDER_DRIVER, "software");
#elif defined(SDL_PLATFORM_MACOS)
			SDL_SetHint(SDL_HINT_RENDER_DRIVER, window_flags & SDL_WINDOW_METAL ? "metal" : "opengl");
#else
			SDL_SetHint(SDL_HINT_RENDER_DRIVER, "opengl");
#endif
	    }

		bool sdl_vsync = PrefsFindBool("sdl_vsync");
		if (sdl_vsync) {
			SDL_SetHint(SDL_HINT_RENDER_VSYNC, "1");
		}

		sdl_renderer = SDL_CreateRenderer(sdl_window, NULL);

		if (!sdl_renderer) {
			shutdown_sdl_video();
			return NULL;
		}

		int clone_num = 0;
		for (vector<DisplayClone>::iterator i = clones.begin(); i != clones.end(); i++) {
			SDL_Renderer * clone_renderer = SDL_CreateRenderer(i->window, NULL);
			if (!clone_renderer) {
				printf("Error creating renderer for clone %d\n", clone_num);
				SDL_DestroyWindow(i->window);
				i->window = NULL;
			} else {
				i->renderer = clone_renderer;
			}
			clone_num++;
		}

		sdl_renderer_thread_id = SDL_ThreadID();

		printf("Using SDL_Renderer driver: %s\n", SDL_GetRendererName(sdl_renderer));
	}
    
    if (!sdl_update_video_mutex) {
        sdl_update_video_mutex = SDL_CreateMutex();
    }

	SDL_assert(sdl_texture == NULL);
#ifdef ENABLE_VOSF
	sdl_texture = SDL_CreateTexture(sdl_renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, width, height);
#else
	sdl_texture = SDL_CreateTexture(sdl_renderer, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, width, height);
#endif
    if (!sdl_texture) {
        shutdown_sdl_video();
        return NULL;
    }
	SDL_SetTextureBlendMode(sdl_texture, SDL_BLENDMODE_NONE);

	for (vector<DisplayClone>::iterator i = clones.begin(); i != clones.end(); i++) {
		if (i->renderer) {
#ifdef ENABLE_VOSF
			i->texture = SDL_CreateTexture(i->renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, width, height);
#else
			i->texture = SDL_CreateTexture(i->renderer, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, width, height);
#endif
			if (i->texture)
				SDL_SetTextureBlendMode(i->texture, SDL_BLENDMODE_NONE);
		} else {
			i->texture = NULL;
		}
	}

    sdl_update_video_rect.x = 0;
    sdl_update_video_rect.y = 0;
    sdl_update_video_rect.w = 0;
    sdl_update_video_rect.h = 0;

	SDL_assert(guest_surface == NULL);
	SDL_assert(host_surface == NULL);
    switch (depth) {
		case VIDEO_DEPTH_1BIT:
		case VIDEO_DEPTH_2BIT:
		case VIDEO_DEPTH_4BIT:
            guest_surface = SDL_CreateSurface(width, height, SDL_GetPixelFormatForMasks(8, 0, 0, 0, 0));
            break;
		case VIDEO_DEPTH_8BIT:
#ifdef ENABLE_VOSF
			guest_surface = SDL_CreateSurface(width, height, SDL_GetPixelFormatForMasks(8, 0, 0, 0, 0));
#else
			guest_surface = SDL_CreateSurfaceFrom(width, height, SDL_GetPixelFormatForMasks(8, 0, 0, 0, 0), the_buffer, pitch);
#endif
			break;
		case VIDEO_DEPTH_16BIT:
			guest_surface = SDL_CreateSurface(width, height, SDL_GetPixelFormatForMasks(16, 0xf800, 0x07e0, 0x001f, 0));
			break;
		case VIDEO_DEPTH_32BIT:
#ifdef ENABLE_VOSF
			guest_surface = SDL_CreateSurface(width, height, SDL_GetPixelFormatForMasks(32, 0x00ff0000, 0x0000ff00, 0x000000ff, 0xff000000));
#else
			guest_surface = SDL_CreateSurfaceFrom(width, height, SDL_GetPixelFormatForMasks(32, 0xff000000, 0x00ff0000, 0x0000ff00, 0x000000ff), the_buffer, pitch);
#endif
			host_surface = guest_surface;
			break;
        default:
            printf("WARNING: An unsupported depth of %d was used\n", depth);
            break;
    }
    if (!guest_surface) {
        shutdown_sdl_video();
        return NULL;
    }

    if (!host_surface) {
		SDL_PropertiesID props = SDL_GetTextureProperties(sdl_texture);
		SDL_PixelFormat texture_format = (SDL_PixelFormat)SDL_GetNumberProperty(props, SDL_PROP_TEXTURE_FORMAT_NUMBER, 0);
    	int bpp;
    	Uint32 Rmask, Gmask, Bmask, Amask;
    	if (!SDL_GetMasksForPixelFormat(texture_format, &bpp, &Rmask, &Gmask, &Bmask, &Amask)) {
    		printf("ERROR: Unable to determine format for host SDL_surface: %s\n", SDL_GetError());
    		shutdown_sdl_video();
    		return NULL;
    	}
        host_surface = SDL_CreateSurface(width, height, SDL_GetPixelFormatForMasks(bpp, Rmask, Gmask, Bmask, Amask));
        if (!host_surface) {
        	printf("ERROR: Unable to create host SDL_surface: %s\n", SDL_GetError());
            shutdown_sdl_video();
            return NULL;
        }
    }

	if (!SDL_SetRenderLogicalPresentation(sdl_renderer, width, height,
		PrefsFindBool("scale_integer") ? SDL_LOGICAL_PRESENTATION_INTEGER_SCALE : SDL_LOGICAL_PRESENTATION_LETTERBOX)) {
		printf("ERROR: Unable to set SDL rendeer's logical size (to %dx%d): %s\n",
			   width, height, SDL_GetError());
		shutdown_sdl_video();
		return NULL;
	}
	if (PrefsFindBool("scale_nearest"))
		SDL_SetTextureScaleMode(sdl_texture, SDL_SCALEMODE_NEAREST);

	for (vector<DisplayClone>::iterator i = clones.begin(); i != clones.end(); i++) {
		if (i->renderer) {
			SDL_SetRenderLogicalPresentation(i->renderer, width, height,
				PrefsFindBool("scale_integer") ? SDL_LOGICAL_PRESENTATION_INTEGER_SCALE : SDL_LOGICAL_PRESENTATION_LETTERBOX);
		}
	}

    return guest_surface;
}

int SDLDisplayInstance::present_sdl_video()
{
	if (SDL_RectEmpty(&sdl_update_video_rect)) return 0;
	
	if (!sdl_renderer || !sdl_texture || !guest_surface) {
		printf("WARNING: A video mode does not appear to have been set.\n");
		return -1;
	}

	// Some systems, such as D3D9, can fail if and when they are used across
	// certain operations.  To address this, only utilize SDL_Renderer in a
	// single thread, preferably the main thread.
	//
	// This was added as part of a fix for https://github.com/DavidLudwig/macemu/issues/21
	// "BasiliskII, Win32: resizing a window does not stretch "
	SDL_assert(SDL_ThreadID() == sdl_renderer_thread_id);

	// Make sure the display's internal (to SDL, possibly the OS) buffer gets
	// cleared.  Not doing so can, if and when letterboxing is applied (whereby
	// colored bars are drawn on the screen's sides to help with aspect-ratio
	// correction), the colored bars can be an unknown color.
	SDL_SetRenderDrawColor(sdl_renderer, 0, 0, 0, 0);	// Use black
	SDL_RenderClear(sdl_renderer);						// Clear the display
	
	// We're about to work with sdl_update_video_rect, so stop other threads from
	// modifying it!
	LOCK_PALETTE;
	SDL_LockMutex(sdl_update_video_mutex);
    // Convert from the guest OS' pixel format, to the host OS' texture, if necessary.
    if (host_surface != guest_surface &&
		host_surface != NULL &&
		guest_surface != NULL)
	{
		SDL_Rect destRect = sdl_update_video_rect;
		int result = SDL_BlitSurface(guest_surface, &sdl_update_video_rect, host_surface, &destRect);
		if (!result) {
			SDL_UnlockMutex(sdl_update_video_mutex);
			UNLOCK_PALETTE;
			return -1;
		}
	}
	UNLOCK_PALETTE; // passed potential deadlock, can unlock palette
	
	// Update the host OS' texture
	uint8_t *srcPixels = (uint8_t *)host_surface->pixels +
		sdl_update_video_rect.y * host_surface->pitch +
		sdl_update_video_rect.x * SDL_GetPixelFormatDetails(host_surface->format)->bytes_per_pixel;

	uint8_t *dstPixels;
	int dstPitch;
	if (!SDL_LockTexture(sdl_texture, &sdl_update_video_rect, (void **)&dstPixels, &dstPitch)) {
		SDL_UnlockMutex(sdl_update_video_mutex);
		return -1;
	}
#ifdef VIDEO_CHROMAKEY
	if (display_type == DISPLAY_CHROMAKEY)
		for (int y = 0; y < sdl_update_video_rect.h; y++) {
			uint32_t *src = (uint32_t *)srcPixels, *dst = (uint32_t *)dstPixels;
			for (int i = 0; i < sdl_update_video_rect.w; i++) {
				uint32 d = *src++;
				*dst++ = d | (d == VIDEO_CHROMAKEY ? 0 : 0xff); // alpha value
			}
			srcPixels += host_surface->pitch;
			dstPixels += dstPitch;
		}
	else
#endif
		for (int y = 0; y < sdl_update_video_rect.h; y++) {
			memcpy(dstPixels, srcPixels, sdl_update_video_rect.w << 2);
			srcPixels += host_surface->pitch;
			dstPixels += dstPitch;
		}
	SDL_UnlockTexture(sdl_texture);

	for (vector<DisplayClone>::iterator i = clones.begin(); i != clones.end(); i++) {
		if (i->texture) {
			srcPixels = (uint8_t *)host_surface->pixels +
			sdl_update_video_rect.y * host_surface->pitch +
			sdl_update_video_rect.x * SDL_GetPixelFormatDetails(host_surface->format)->bytes_per_pixel;

			if (SDL_LockTexture(i->texture, &sdl_update_video_rect, (void **)&dstPixels, &dstPitch) >= 0) {

					for (int y = 0; y < sdl_update_video_rect.h; y++) {
						memcpy(dstPixels, srcPixels, sdl_update_video_rect.w << 2);
						srcPixels += host_surface->pitch;
						dstPixels += dstPitch;
					}
					SDL_UnlockTexture(i->texture);
			}
		}
	}


    // We are done working with pixels in host_surface.  Reset sdl_update_video_rect, then let
    // other threads modify it, as-needed.
    sdl_update_video_rect.x = 0;
    sdl_update_video_rect.y = 0;
    sdl_update_video_rect.w = 0;
    sdl_update_video_rect.h = 0;
    SDL_UnlockMutex(sdl_update_video_mutex);

    // Copy the texture to the display
    if (!SDL_RenderTexture(sdl_renderer, sdl_texture, NULL, NULL)) {
		return -1;
	}
	
    // Update the display
	SDL_RenderPresent(sdl_renderer);

	for (vector<DisplayClone>::iterator i = clones.begin(); i != clones.end(); i++) {
		if (i->renderer) {
			SDL_SetRenderDrawColor(i->renderer, 0, 0, 0, 0);	// Use black
			SDL_RenderClear(i->renderer);
			SDL_RenderTexture(i->renderer, i->texture, NULL, NULL);
			SDL_RenderPresent(i->renderer);
		}
	}

    // Indicate success to the caller!
    return 0;
}

void SDLDisplayInstance::update_sdl_video(SDL_Surface *s, int numrects, SDL_Rect *rects)
{
    // TODO: make sure SDL_Renderer resources get displayed, if and when
    // MacsBug is running (and VideoInterrupt() might not get called)
    
    SDL_LockMutex(sdl_update_video_mutex);
    for (int i = 0; i < numrects; ++i) {
		SDL_GetRectUnion(&sdl_update_video_rect, &rects[i], &sdl_update_video_rect);
    }
    SDL_UnlockMutex(sdl_update_video_mutex);
}

void SDLDisplayInstance::update_sdl_video(SDL_Surface *s, Sint32 x, Sint32 y, Sint32 w, Sint32 h)
{
    SDL_Rect temp = {x, y, w, h};
    update_sdl_video(s, 1, &temp);
}

#ifdef SHEEPSHAVER
static void MagBits(Uint8 *dst, Uint8 *src, int size) {
	float s = 16.f / size;
	for (int y = 0; y < size; y++)
		for (int x = 0; x < size; x++) {
			int sa = 16 * int(y * s) + int(x * s);
			if (src[sa >> 3] & 0x80 >> (sa & 7)) {
				int da = (size + 7 & ~7) * y + x;
				dst[da >> 3] |= 0x80 >> (da & 7);
			}
		}
}
SDL_Cursor *SDLDisplayInstance::MagCursor(bool hot) {
	int w, h;
	SDL_GetWindowSize(sdl_window, &w, &h);
	float mag = std::min((float)w / drv()->VIDEO_MODE_X, (float)h / drv()->VIDEO_MODE_Y);
	int size = ceilf(16 * mag), n = ((size + 7) >> 3) * size;
	Uint8 *data = (Uint8 *)SDL_calloc(n, 2);
	Uint8 *mask = (Uint8 *)SDL_calloc(n, 2);
	MagBits(data, &MacCursor[4], size);
	MagBits(mask, &MacCursor[36], size);
	SDL_Cursor *cursor = SDL_CreateCursor(data, mask, size, size, hot ? MacCursor[2] * mag : 0, hot ? MacCursor[3] * mag : 0);
	SDL_free(data);
	SDL_free(mask);
	return cursor;
}
#endif

void driver_base::set_video_mode(int flags, int pitch)
{
	if ((s = monitor.sdl_display.init_sdl_video(VIDEO_MODE_X, VIDEO_MODE_Y, VIDEO_MODE_DEPTH, flags, pitch)) == NULL)
		return;
#ifdef ENABLE_VOSF
	the_host_buffer = (uint8 *)s->pixels;
#endif
}

void SDLDisplayInstance::init_buffers(int pitch, int aligned_height, int monitor_desc_num) {
#ifdef ENABLE_VOSF
	use_vosf = true;
	// Allocate memory for frame buffer (SIZE is extended to page-boundary)
	the_buffer_size = page_extend((aligned_height + 2) * pitch);
	the_buffer = (uint8 *)vm_acquire_framebuffer(the_buffer_size);
	the_buffer_copy = (uint8 *)malloc(the_buffer_size);
	D(bug("monitor %d (using VOSF) the_buffer = %p, the_buffer_copy = %p, the_host_buffer = %p\n", drv()->monitor.display_instance_num(), the_buffer, the_buffer_copy, the_host_buffer));

	// Check whether we can initialize the VOSF subsystem and it's profitable
	if (!video_vosf_init(monitor)) {
		WarningAlert(GetString(STR_VOSF_INIT_ERR));
		use_vosf = false;
	}
	else if (!video_vosf_profitable()) {
		video_vosf_exit();
		printf("VOSF acceleration is not profitable on this platform, disabling it\n");
		use_vosf = false;
	}
    if (!use_vosf) {
		free(the_buffer_copy);
		vm_release(the_buffer, the_buffer_size);
		the_host_buffer = NULL;
	}
#endif
	if (!use_vosf) {
		// Allocate memory for frame buffer
		the_buffer_size = (aligned_height + 2) * pitch;
		the_buffer_copy = (uint8 *)calloc(1, the_buffer_size);
		the_buffer = (uint8 *)vm_acquire_framebuffer(the_buffer_size, monitor_desc_num);
		memset(the_buffer, 0, the_buffer_size);
		D(bug("monitor %d the_buffer = %p, the_buffer_copy = %p (size 0x%x)\n", drv()->monitor.display_instance_num(), the_buffer, the_buffer_copy, the_buffer_size));
	}
}

void driver_base::init(int monitor_desc_num)
{
	int pitch = VIDEO_MODE_X;
	switch (VIDEO_MODE_DEPTH) {
		case VIDEO_DEPTH_16BIT: pitch <<= 1; break;
		case VIDEO_DEPTH_32BIT: pitch <<= 2; break;
	}

	int aligned_height = (VIDEO_MODE_Y + 15) & ~15;

	monitor.sdl_display.init_buffers(pitch, aligned_height, monitor_desc_num);

	set_video_mode(monitor.sdl_display.get_display_type() == DISPLAY_SCREEN ? SDL_WINDOW_FULLSCREEN : 0, pitch);

	// Set frame buffer base
	set_mac_frame_buffer(monitor, VIDEO_MODE_DEPTH, true);

	adapt_to_video_mode();
	
	// set default B/W palette
	monitor.sdl_display.sdl_palette = SDL_CreatePalette(256);
	monitor.sdl_display.sdl_palette->colors[1] = (SDL_Color){ .r = 0, .g = 0, .b = 0, .a = 255 };
	SDL_SetSurfacePalette(s, monitor.sdl_display.sdl_palette);

	if (PrefsFindBool("init_grab") && !PrefsFindBool("hardcursor")) grab_mouse();
}

#ifdef SHEEPSHAVER
void SDLDisplayInstance::create_cursor() {
		// Create cursor
		if ((sdl_cursor = MagCursor(false)) != NULL) {
			SDL_SetCursor(sdl_cursor);
		}
}
#endif

void driver_base::adapt_to_video_mode() {
	ADBSetRelMouseMode(mouse_grabbed);

	// Init blitting routines
	if (!s) return;
	const SDL_PixelFormatDetails *f = SDL_GetPixelFormatDetails(s->format);
	VisualFormat visualFormat;
	visualFormat.depth = sdl_depth_of_video_depth(VIDEO_MODE_DEPTH);
	visualFormat.Rmask = f->Rmask;
	visualFormat.Gmask = f->Gmask;
	visualFormat.Bmask = f->Bmask;
	Screen_blitter_init(visualFormat, true, mac_depth_of_video_depth(VIDEO_MODE_DEPTH), monitor.sdl_display.Screen_blit);

	// Load gray ramp to 8->16/32 expand map
	if (!IsDirectMode(mode))
		for (int i=0; i<256; i++)
			ExpandMap[i] = SDL_MapRGB(f, NULL, i, i, i);


	bool hardware_cursor = false;
#ifdef SHEEPSHAVER
	hardware_cursor = video_can_change_cursor();
	if (hardware_cursor) {
		monitor.sdl_display.create_cursor();
	}
	// Tell the video driver there's a change in cursor type
	monitor.setHardwareCursor(hardware_cursor);
#endif
	SDL_LockMutex(monitor.sdl_display.sdl_update_video_mutex);
	monitor.sdl_display.sdl_update_video_rect.x = 0;
	monitor.sdl_display.sdl_update_video_rect.y = 0;
	monitor.sdl_display.sdl_update_video_rect.w = VIDEO_MODE_X;
	monitor.sdl_display.sdl_update_video_rect.h = VIDEO_MODE_Y;
	SDL_UnlockMutex(monitor.sdl_display.sdl_update_video_mutex);
	
	// Hide cursor
	hardware_cursor ? SDL_ShowCursor() : SDL_HideCursor();

	// Set window name/class
	monitor.sdl_display.set_window_name();

	// Everything went well
	init_ok = true;
}

void SDLDisplayInstance::release_buffers() {

	// the_buffer shall always be mapped through vm_acquire_framebuffer()
	if (the_buffer != VM_MAP_FAILED) {
		D(bug(" releasing the_buffer at %p (%d bytes)\n", the_buffer, the_buffer_size));
		vm_release_framebuffer(the_buffer, the_buffer_size);
		the_buffer = NULL;
	}

	// Free frame buffer(s)
	if (!use_vosf) {
		if (the_buffer_copy) {
			D(bug(" release the_buffer_copy at %p\n", the_buffer_copy));
			free(the_buffer_copy);
			the_buffer_copy = NULL;
		}
	}
#ifdef ENABLE_VOSF
	else {
		if (the_buffer_copy) {
			D(bug(" freeing the_buffer_copy at %p\n", the_buffer_copy));
			free(the_buffer_copy);
			the_buffer_copy = NULL;
		}

		// Deinitialize VOSF
		video_vosf_exit();
	}
#endif
}

driver_base::~driver_base()
{
	ungrab_mouse();
	restore_mouse_accel();

	monitor.sdl_display.delete_sdl_video_surfaces();	// This deletes instances of SDL_Surface and SDL_Texture

	// shutdown_sdl_video() would delete SDL_Window, SDL_Renderer, in addition to
	// instances of SDL_Surface and SDL_Texture.

	// Do not shut down the window here, in case we are just changing modes and it can be reused.
	// - If we are changing modes the init_sdl_video() of the next driver is responsible for
	// cleaning up the old window if it cannot be reused.
	// - If we are shutting down, VideoExit() will cleanup the window after the driver.

	monitor.sdl_display.release_buffers();

	SDL_ShowCursor();
}

// Palette has changed
void driver_base::update_palette(void)
{
	const VIDEO_MODE &mode = monitor.get_current_mode();

	if ((int)VIDEO_MODE_DEPTH <= VIDEO_DEPTH_8BIT) {
		SDL_SetSurfacePalette(s, monitor.sdl_display.sdl_palette);
		SDL_LockMutex(monitor.sdl_display.sdl_update_video_mutex);
		monitor.sdl_display.sdl_update_video_rect.x = 0;
		monitor.sdl_display.sdl_update_video_rect.y = 0;
		monitor.sdl_display.sdl_update_video_rect.w = VIDEO_MODE_X;
		monitor.sdl_display.sdl_update_video_rect.h = VIDEO_MODE_Y;
		SDL_UnlockMutex(monitor.sdl_display.sdl_update_video_mutex);
	}
}

// Disable mouse acceleration
void driver_base::disable_mouse_accel(void)
{
}

// Restore mouse acceleration to original value
void driver_base::restore_mouse_accel(void)
{
}

// Toggle mouse grab
void driver_base::toggle_mouse_grab(void)
{
	if (mouse_grabbed)
		ungrab_mouse();
	else
		grab_mouse();
}

void SDLDisplayInstance::update_mouse_grab()
{
	SDL_SetWindowRelativeMouseMode(sdl_window, mouse_grabbed);
}

// Grab mouse, switch to relative mouse mode
void driver_base::grab_mouse(void)
{
	if (!mouse_grabbed) {
		mouse_grabbed = true;
		monitor.sdl_display.update_mouse_grab();
		monitor.sdl_display.set_window_name();
		disable_mouse_accel();
		ADBSetRelMouseMode(true);
	}
}

// Ungrab mouse, switch to absolute mouse mode
void driver_base::ungrab_mouse(void)
{
	if (mouse_grabbed) {
		mouse_grabbed = false;
		monitor.sdl_display.update_mouse_grab();
		monitor.sdl_display.set_window_name();
		restore_mouse_accel();
		ADBSetRelMouseMode(false);
	}
}

/*
 *  Initialization
 */

// Init keycode translation table
static void keycode_init(void)
{
	bool use_kc = PrefsFindBool("keycodes");
	if (use_kc) {

		// Get keycode file path from preferences
		const char *kc_path = PrefsFindString("keycodefile");

		// Open keycode table
		FILE *f = fopen(kc_path && *kc_path ? kc_path : KEYCODE_FILE_NAME, "r");
		if (f == NULL) f = fopen(KEYCODE_FILE_NAME2, "r");
		if (f == NULL) {
			char str[256];
			snprintf(str, sizeof(str), GetString(STR_KEYCODE_FILE_WARN), kc_path ? kc_path : KEYCODE_FILE_NAME, strerror(errno));
			WarningAlert(str);
			return;
		}

		// Default translation table
		for (int i=0; i<256; i++)
			keycode_table[i] = CODE_INVALID;

		// Search for server vendor string, then read keycodes
		const char * video_driver = SDL_GetCurrentVideoDriver();
		bool video_driver_found = false;
		char line[256];
		int n_keys = 0;
		while (fgets(line, sizeof(line) - 1, f)) {
			// Read line
			int len = strlen(line);
			if (len == 0)
				continue;
			line[len-1] = 0;

			// Comments begin with "#" or ";"
			if (line[0] == '#' || line[0] == ';' || line[0] == 0)
				continue;

			if (video_driver_found) {
				// Skip aliases as long as we have read keycodes yet
				// Otherwise, it's another mapping and we have to stop
				static const char sdl_str[] = "sdl";
				if (strncmp(line, sdl_str, sizeof(sdl_str) - 1) == 0 && n_keys == 0)
					continue;

				// Read keycode
				int x_code, mac_code;
				if (sscanf(line, "%d %d", &x_code, &mac_code) == 2)
					keycode_table[x_code & 0xff] = mac_code, n_keys++;
				else
					break;
			} else {
				// Search for SDL video driver string
				static const char sdl_str[] = "sdl";
				if (strncmp(line, sdl_str, sizeof(sdl_str) - 1) == 0) {
					char *p = line + sizeof(sdl_str);
					if (video_driver && strstr(video_driver, p) == video_driver)
						video_driver_found = true;
				}
			}
		}

		// Keycode file completely read
		fclose(f);
		use_keycodes = video_driver_found;

		// Vendor not found? Then display warning
		if (!video_driver_found) {
			char str[256];
			snprintf(str, sizeof(str), GetString(STR_KEYCODE_VENDOR_WARN), video_driver ? video_driver : "", kc_path ? kc_path : KEYCODE_FILE_NAME);
			WarningAlert(str);
			return;
		}

		D(bug("Using SDL/%s keycodes table, %d key mappings\n", video_driver ? video_driver : "", n_keys));
	}
}

// Open display for current mode
bool SDL_monitor_desc::video_open(void)
{
	D(bug("video_open(%d) %p\n", monitor_desc_num, this));
#if DEBUG
	const VIDEO_MODE &mode = get_current_mode();
	D(bug("Current video mode:\n"));
	D(bug(" %dx%d (ID %02x), %d bpp\n", VIDEO_MODE_X, VIDEO_MODE_Y, VIDEO_MODE_RESOLUTION, 1 << (VIDEO_MODE_DEPTH & 0x0f)));
#endif

	// Create display driver object of requested type
	drv = new(std::nothrow) driver_base(*this);
	if (drv == NULL)
		return false;
	sdl_display.set_driver_base(drv);
	drv->init(monitor_desc_num);
	if (!drv->init_ok) {
		delete drv;
		drv = NULL;
		sdl_display.set_driver_base(NULL);
		return false;
	}

	// FIXME Is this right for this Windows message handler
	if (monitor_desc_num == 0) {

#ifdef WIN32
		// Chain in a new message handler for WM_DEVICECHANGE
		HWND the_window = GetMainWindowHandle();
		sdl_window_proc = (WNDPROC)GetWindowLongPtr(the_window, GWLP_WNDPROC);
		SetWindowLongPtr(the_window, GWLP_WNDPROC, (LONG_PTR)windows_message_handler);
#endif
	}

	// Initialize VideoRefresh function
	VideoRefreshInit();

	// Lock down frame buffer
	sdl_display.LOCK_FRAME_BUFFER();

	if (!sdl_display.start_redraw()) {
		printf("FATAL: cannot create redraw thread\n");
		return false;
	}

	return true;
}

bool SDLDisplayInstance::start_redraw()
{
	// Start redraw/input thread
#ifndef USE_CPU_EMUL_SERVICES
	redraw_thread_cancel = false;
	redraw_thread_active = ((redraw_thread = SDL_CreateThread(redraw_func, "Redraw Thread", this)) != NULL);
	if (!redraw_thread_active) {
		return false;
	}
#else
	redraw_thread_active = true;
#endif
	return true;
}

static void apply_max_display_dimensions(int display_num, int & default_width, int & default_height)
{
	int max_width, max_height;
	sdl_display_dimensions(display_num, max_width, max_height);
	if (default_width <= 0)
		default_width = max_width;
	else if (default_width > max_width)
		default_width = max_width;
	if (default_height <= 0)
		default_height = max_height;
	else if (default_height > max_height)
		default_height = max_height;
}


#ifdef SHEEPSHAVER
bool VideoInit(void)
{
	const bool classic = false;
#else
bool VideoInit(bool classic)
{
#endif
	classic_mode = classic;

#ifdef ENABLE_VOSF
	// Zero the mainBuffer structure
	mainBuffer.dirtyPages = NULL;
	mainBuffer.pageInfo = NULL;
#endif

	// Create Mutexes
	if ((sdl_events_lock = SDL_CreateMutex()) == NULL)
		return false;
	if ((sdl_palette_lock = SDL_CreateMutex()) == NULL)
		return false;

	// Init keycode translation
	keycode_init();

	// Read prefs
	frame_skip = PrefsFindInt32("frameskip");
	mouse_wheel_mode = PrefsFindInt32("mousewheelmode");
	mouse_wheel_lines = PrefsFindInt32("mousewheellines");
	mouse_wheel_reverse = mouse_wheel_lines < 0;
	if (mouse_wheel_reverse) mouse_wheel_lines = -mouse_wheel_lines;

	// Get screen mode from preferences
	migrate_screen_prefs();
	const char *mode_str = NULL;
	if (classic_mode)
		mode_str = "win/512/342";
	else
		mode_str = PrefsFindString("screen");

	// Determine display type and default dimensions
	int default_width, default_height;
	if (classic) {
		default_width = 512;
		default_height = 384;
	}
	else {
		default_width = 640;
		default_height = 480;
	}
	int display_type = DISPLAY_WINDOW;
	if (mode_str) {
		if (sscanf(mode_str, "win/%d/%d", &default_width, &default_height) == 2)
			display_type = DISPLAY_WINDOW;
		else if (sscanf(mode_str, "dga/%d/%d", &default_width, &default_height) == 2)
			display_type = DISPLAY_SCREEN;
#ifdef VIDEO_CHROMAKEY
		else if (strncmp(mode_str, "chromakey", 9) == 0) {
			display_type = DISPLAY_CHROMAKEY;
			default_width = sdl_display_width();
			default_height = sdl_display_height();
		}
#endif
	}

	// Initialize list of video modes to try
	struct {
		int w;
		int h;
	}
#ifdef SHEEPSHAVER
	// Omit Classic resolutions
	video_modes[] = {
		{  640,  480 },
		{  800,  600 },
		{ 1024,  768 },
		{ 1152,  870 },
		{ 1280, 1024 },
		{ 1600, 1200 },
		{ 0, }
	};
#else
	video_modes[] = {
		{  512,  384 },
		{  640,  480 },
		{  800,  600 },
		{ 1024,  768 },
		{ 1152,  870 },
		{ 1280, 1024 },
		{ 1600, 1200 },
		{ 0, }
	};
#endif

	vector<int> displays;
	displays.push_back(PrefsFindInt32("display_num"));

	int add_display = PrefsFindInt32("add_display");
	if (add_display > 0) {
		displays.push_back(add_display);
	}

	bool ret = true;

	int monitor_desc_num = 0;
	for (vector<int>::iterator display_i = displays.begin(); display_i != displays.end(); display_i++) {
		int display_num = *display_i;

		vector<VIDEO_MODE> VideoModes;
		int default_depth;
		uint32 default_id;
		int color_depth;


		// Mac screen depth follows X depth
		int screen_depth = 32;
		const SDL_DisplayMode *desktop_mode = SDL_GetDesktopDisplayMode(display_num);
		if (desktop_mode != NULL) {
			screen_depth = SDL_BITSPERPIXEL(desktop_mode->format);
		}
		switch (screen_depth) {
		case 8:
			default_depth = VIDEO_DEPTH_8BIT;
			break;
		case 15: case 16:
			default_depth = VIDEO_DEPTH_16BIT;
			break;
		case 24: case 32:
			default_depth = VIDEO_DEPTH_32BIT;
			break;
		default:
			default_depth =  VIDEO_DEPTH_1BIT;
			break;
		}

		// If we had a separate prefs configured resolution for each monitor, we would need to have a separate
		// value for each here instead of default_width/default_height
		int cur_width = default_width;
		int cur_height = default_height;
		apply_max_display_dimensions(display_num, cur_width, cur_height);

		// Construct list of supported modes
		if ((display_type == DISPLAY_WINDOW) && classic) {
			add_mode(VideoModes, display_num, display_type, 512, 342, 0x80, 64, VIDEO_DEPTH_1BIT);
		} else {
			int custom_id = 0;
			int next_resolution_id = 0x80;
#ifdef SHEEPSHAVER
			int next_custom_id = APPLE_CUSTOM;
#endif

			// mode specified in screen pref (or for default, based on display size)
			int resolution_id = next_resolution_id++;
#ifdef SHEEPSHAVER
			custom_id = next_custom_id++;
#endif
			for (int d = VIDEO_DEPTH_1BIT; d <= default_depth; d++)
				add_mode(VideoModes, display_num, display_type, cur_width, cur_height, resolution_id, TrivialBytesPerRow(cur_width, (video_depth)d), d, custom_id);

			// additional modes specified in add_mode prefs
			int add_mode_index = 0;
			const char *add_mode_str;
			while ((add_mode_str = PrefsFindString("add_mode", add_mode_index++)) != NULL) {
				int w, h;
				if (sscanf(add_mode_str, "%d/%d", &w, &h) == 2) {
					resolution_id = next_resolution_id++;
#ifdef SHEEPSHAVER
					custom_id = next_custom_id++;
#endif
					for (int d = VIDEO_DEPTH_1BIT; d <= default_depth; d++)
						add_mode(VideoModes, display_num, display_type, w, h, resolution_id, TrivialBytesPerRow(w, (video_depth)d), d, custom_id);
				}
			}

			// modes based on video_modes[] table above
			for (int i = 0; video_modes[i].w != 0; i++) {
				const int w = video_modes[i].w;
				const int h = video_modes[i].h;
				if (i > 0 && (w >= cur_width || h >= cur_height))
					continue;
				resolution_id = next_resolution_id++;
				for (int d = VIDEO_DEPTH_1BIT; d <= default_depth; d++)
					add_mode(VideoModes, display_num, display_type, w, h, resolution_id, TrivialBytesPerRow(w, (video_depth)d), d);
			}
		}

		if (VideoModes.empty()) {
			ErrorAlert(STR_NO_XVISUAL_ERR);
			return false;
		}

		// Find requested default mode with specified dimensions
		std::vector<VIDEO_MODE>::const_iterator i, end = VideoModes.end();
		for (i = VideoModes.begin(); i != end; ++i) {
			const VIDEO_MODE & mode = (*i);
			if (VIDEO_MODE_X == cur_width && VIDEO_MODE_Y == cur_height && VIDEO_MODE_DEPTH == default_depth) {
				default_id = VIDEO_MODE_RESOLUTION;
				break;
			}
		}
		if (i == end) { // not found, use first available mode
			const VIDEO_MODE & mode = VideoModes[0];
			default_depth = VIDEO_MODE_DEPTH;
			default_id = VIDEO_MODE_RESOLUTION;
		}

	#if DEBUG
		D(bug("Available video modes:\n"));
		for (i = VideoModes.begin(); i != end; ++i) {
			const VIDEO_MODE & mode = (*i);
			int bits = 1 << VIDEO_MODE_DEPTH;
			if (bits == 16)
				bits = 15;
			else if (bits == 32)
				bits = 24;
			D(bug(" %dx%d (ID %02x), %d colors\n", VIDEO_MODE_X, VIDEO_MODE_Y, VIDEO_MODE_RESOLUTION, 1 << bits));
		}
	#endif

		color_depth = get_customized_color_depth(default_depth);

		D(bug("Return get_customized_color_depth %d\n", color_depth));

		// Create SDL_monitor_desc for this display
		SDL_monitor_desc *monitor = new SDL_monitor_desc(VideoModes, (video_depth)color_depth, default_id, monitor_desc_num++);
		if (!monitor) {	ret = false; break;	}
		if (!monitor->sdl_display.init_locks()) { delete monitor; ret = false; break; }
		monitor->sdl_display.set_display_type(display_type);
		VideoMonitors.push_back(monitor);

		// Open display
		ret = monitor->video_open();
		if (!ret) break;
	}

	if (!ret) {
		// Cleanup partially initialized monitors
		for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); i++) {
			SDL_monitor_desc *monitor = static_cast<SDL_monitor_desc *>(*(i));
			monitor->video_close();
			monitor->sdl_display.destroy_locks();
			delete monitor;
		}
		VideoMonitors.clear();
	}

	return ret;
}

bool SDLDisplayInstance::init_locks() {
	if ((frame_buffer_lock = SDL_CreateMutex()) == NULL)
		return false;
	return true;
}


/*
 *  Deinitialization
 */

// Close display
void SDL_monitor_desc::video_close(void)
{
	D(bug("video_close(%d)\n", monitor_desc_num));

	if (monitor_desc_num == 0) {

#ifdef WIN32
		// Remove message handler for WM_DEVICECHANGE
		HWND the_window = GetMainWindowHandle();
		SetWindowLongPtr(the_window, GWLP_WNDPROC, (LONG_PTR)sdl_window_proc);
#endif
	}

	sdl_display.stop_redraw();

	// Unlock frame buffer
	sdl_display.UNLOCK_FRAME_BUFFER();
	D(bug(" frame buffer unlocked\n"));

	// Close display
	D(bug("video_close(%d) deleting drv\n", monitor_desc_num));
	delete drv;
	drv = NULL;
	sdl_display.set_driver_base(NULL);
}

void SDLDisplayInstance::stop_redraw()
{
	// Stop redraw thread
#ifndef USE_CPU_EMUL_SERVICES
	if (redraw_thread_active) {
		redraw_thread_cancel = true;
		SDL_WaitThread(redraw_thread, NULL);
	}
#endif
	redraw_thread_active = false;
}

void SDLDisplayInstance::destroy_locks()
{
	if (frame_buffer_lock)
		SDL_DestroyMutex(frame_buffer_lock);
	frame_buffer_lock = NULL;
}

void VideoExit(void)
{
	// Close displays
	vector<monitor_desc *>::iterator i, end = VideoMonitors.end();
	for (i = VideoMonitors.begin(); i != end; ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		sdm->video_close();
		sdm->sdl_display.shutdown_sdl_video();
		sdm->sdl_display.destroy_locks();
	}

	// Destroy locks
	if (sdl_palette_lock)
		SDL_DestroyMutex(sdl_palette_lock);
	if (sdl_events_lock)
		SDL_DestroyMutex(sdl_events_lock);
}


/*
 *  Close down full-screen mode (if bringing up error alerts is unsafe while in full-screen mode)
 */

void VideoQuitFullScreen(void)
{
	D(bug("VideoQuitFullScreen()\n"));
	quit_full_screen = true;
}

static void ApplyGammaRamp() {
// gamma functionarity has been removed from SDL3
}

void SDLDisplayInstance::do_toggle_fullscreen(void)
{
	D(bug("starting toggle fullscreen\n"));
#ifndef USE_CPU_EMUL_SERVICES
	// pause redraw thread
	thread_stop_ack = false;
	thread_stop_req = true;
	while (!thread_stop_ack) ;
#endif

	// Apply fullscreen
	if (sdl_window) {
		if (display_type == DISPLAY_SCREEN) {
			display_type = DISPLAY_WINDOW;
			SDL_SetWindowFullscreen(sdl_window, false);
			const VIDEO_MODE &mode = drv()->mode;
			float m = get_mag_rate();
			SDL_SetWindowSize(sdl_window, m * VIDEO_MODE_X, m * VIDEO_MODE_Y);
#ifndef SDL_PLATFORM_MACOS
			SDL_SetWindowPosition(sdl_window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
#endif

			for (vector<DisplayClone>::iterator i = clones.begin(); i != clones.end(); i++) {
				if (i->window) {
					SDL_SetWindowFullscreen(i->window, false);
					SDL_SetWindowSize(i->window, m * VIDEO_MODE_X, m * VIDEO_MODE_Y);
#ifndef __MACOSX__
					if (i->display_num < 1) {
						SDL_SetWindowPosition(i->window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
					} else {
						// In SDL 3 the displays seem to be numbered from 1
						SDL_SetWindowPosition(i->window, SDL_WINDOWPOS_CENTERED_DISPLAY(i->display_num),
															SDL_WINDOWPOS_CENTERED_DISPLAY(i->display_num));
					}
#endif

				}
			}

		} else {
			display_type = DISPLAY_SCREEN;
			SDL_SetWindowFullscreen(sdl_window, true);
#ifdef SDL_PLATFORM_MACOS
			set_menu_bar_visible_osx(false);
#endif

			for (vector<DisplayClone>::iterator i = clones.begin(); i != clones.end(); i++) {
				if (i->window) {
					SDL_SetWindowFullscreen(i->window, true);

					if (i->display_num < 1) {
						SDL_SetWindowPosition(i->window, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED);
					} else {
						// In SDL 3 the displays seem to be numbered from 1
						SDL_SetWindowPosition(i->window, SDL_WINDOWPOS_UNDEFINED_DISPLAY(i->display_num),
															SDL_WINDOWPOS_UNDEFINED_DISPLAY(i->display_num));
					}
				}
			}
		}
		SDL_SyncWindow(sdl_window);

		for (vector<DisplayClone>::iterator i = clones.begin(); i != clones.end(); i++) {
			if (i->window) {
				SDL_SyncWindow(i->window);
			}
		}
	}

	// switch modes
	drv()->adapt_to_video_mode();

	// reset the palette
#ifdef SHEEPSHAVER
	video_set_palette(&drv()->monitor);
#endif
	ApplyGammaRamp();
	drv()->update_palette();

	// reset the video refresh handler
	VideoRefreshInit();

	// while SetVideoMode is happening, control key up may be missed
	ADBKeyUp(0x36);
	
	// resume redraw thread
	toggle_fullscreen = false;
#ifndef USE_CPU_EMUL_SERVICES
	thread_stop_req = false;
#endif
	D(bug("finished window toggle fullscreen\n"));
}

/*
 *  Mac VBL interrupt
 */

/*
 *  Execute video VBL routine
 */

bool SDLDisplayInstance::is_fullscreen(SDL_Window * window)
{
#ifdef SDL_PLATFORM_MACOS
	// On OSX, SDL, at least as of 2.0.5 (and possibly beyond), does not always
	// report changes to fullscreen via the SDL_WINDOW_FULLSCREEN flag.
	// (Example: https://bugzilla.libsdl.org/show_bug.cgi?id=3766 , which
	// involves fullscreen/windowed toggles via window-manager UI controls).
	// Until it does, or adds a facility to do so, we'll use a platform-specific
	// code path to detect fullscreen changes.
	return is_fullscreen_osx(sdl_window);
#else
	if (!window) {
		return false;
	}
	const Uint32 sdl_window_flags = SDL_GetWindowFlags(sdl_window);
	return (sdl_window_flags & SDL_WINDOW_FULLSCREEN) != 0;
#endif
}

#ifdef SHEEPSHAVER
void VideoVBL(void)
{
	// Emergency quit requested? Then quit
	if (emerg_quit)
		QuitEmulator();

	for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		if (sdm && sdm->sdl_display.drv() && sdm->sdl_display.drv()->init_ok) {
			sdm->sdl_display.interrupt_time();
		}
	}

	// Temporarily give up frame buffer lock (this is the point where
	// we are suspended when the user presses Ctrl-Tab)
	for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		if (sdm && sdm->sdl_display.drv() && sdm->sdl_display.drv()->init_ok) {
			sdm->sdl_display.UNLOCK_FRAME_BUFFER();
			sdm->sdl_display.LOCK_FRAME_BUFFER();
		}
	}

	// Execute video VBL
	for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		if (sdm && sdm->sdl_display.drv() && sdm->sdl_display.drv()->init_ok) {
			if (sdm->interruptsEnabled())
				VSLDoInterruptService(sdm->vslServiceID());
		}
	}
}
#else
void VideoInterrupt(void)
{
	// We must fill in the events queue in the same thread that did call SDL_SetVideoMode()
	SDL_PumpEvents();

	// Emergency quit requested? Then quit
	if (emerg_quit)
		QuitEmulator();

#ifdef VIDEO_CHROMAKEY
	if (display_type == DISPLAY_CHROMAKEY)
		make_window_transparent(sdl_window);
	else
#endif

	for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		if (sdm && sdm->sdl_display.drv() && sdm->sdl_display.drv()->init_ok)
			sdm->sdl_display.interrupt_time();
	}

	// Temporarily give up frame buffer lock (this is the point where
	// we are suspended when the user presses Ctrl-Tab)
	for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		if (sdm && sdm->sdl_display.drv() && sdm->sdl_display.drv()->init_ok) {
			sdm->sdl_display.UNLOCK_FRAME_BUFFER();
			sdm->sdl_display.LOCK_FRAME_BUFFER();
		}
	}
}
#endif

void SDLDisplayInstance::interrupt_time() {
#ifdef SHEEPSHAVER
#ifdef VIDEO_CHROMAKEY
	if (display_type == DISPLAY_CHROMAKEY)
		make_window_transparent(sdl_window);
	else
#endif
#endif
	if (toggle_fullscreen)
		do_toggle_fullscreen();

	present_sdl_video();
}


/*
 *  Set palette
 */

#ifdef SHEEPSHAVER
void video_set_palette(monitor_desc * monitor)
{
	int n_colors = palette_size(monitor->get_current_mode().viAppleMode);
	uint8 pal[256 * 3];
	for (int c = 0; c < n_colors; c++) {
		pal[c*3 + 0] = mac_pal[c].red;
		pal[c*3 + 1] = mac_pal[c].green;
		pal[c*3 + 2] = mac_pal[c].blue;
	}
	monitor->set_palette(pal, n_colors);
}
	
void video_set_gamma(monitor_desc * monitor, int n_colors)
{
	uint8 gamma[256 * 3];
	for (int c = 0; c < n_colors; c++) {
		gamma[c*3 + 0] = mac_gamma[c].red;
		gamma[c*3 + 1] = mac_gamma[c].green;
		gamma[c*3 + 2] = mac_gamma[c].blue;
	}
	monitor->set_gamma(gamma, n_colors);
}
#endif
	
void SDL_monitor_desc::set_palette(uint8 *pal, int num_in)
{
	
	const VIDEO_MODE &mode = get_current_mode();
	D(bug("SDL_monitor_desc::set_palette(pal, %d)\n", num_in));
	
	LOCK_PALETTE;

	// Convert colors to XColor array
	int num_out = 256;
	bool stretch = false;
	
	if (!sdl_display.sdl_palette) {
		sdl_display.sdl_palette = SDL_CreatePalette(num_out);
	}
	
	SDL_Color *p = sdl_display.sdl_palette->colors;
	for (int i=0; i<num_out; i++) {
		int c = (stretch ? (i * num_in) / num_out : i);
		p->r = pal[c*3 + 0] * 0x0101;
		p->g = pal[c*3 + 1] * 0x0101;
		p->b = pal[c*3 + 2] * 0x0101;
		p++;
	}

	// Recalculate pixel color expansion map
	if (!IsDirectMode(mode)) {
		for (int i=0; i<256; i++) {
			int c = i & (num_in-1); // If there are less than 256 colors, we repeat the first entries (this makes color expansion easier)
			ExpandMap[i] = SDL_MapRGB(SDL_GetPixelFormatDetails(drv->s->format), NULL, pal[c*3+0], pal[c*3+1], pal[c*3+2]);
		}

#ifdef ENABLE_VOSF
		if (use_vosf) {
			// We have to redraw everything because the interpretation of pixel values changed
			LOCK_VOSF;
			PFLAG_SET_ALL;
			UNLOCK_VOSF;
			memset(the_buffer_copy, 0, VIDEO_MODE_ROW_BYTES * VIDEO_MODE_Y);
		}
#endif
	}

	// Tell redraw thread to change palette
	sdl_display.sdl_palette_changed = true;

	UNLOCK_PALETTE;
}
	
void SDL_monitor_desc::set_gamma(uint8 *gamma, int num_in)
{
	// handle the gamma ramp
		
	if (gamma[0] == 127 && gamma[num_in*3-1] == 127) // solid grey
		return; // ignore

	uint16 red[256];
	uint16 green[256];
	uint16 blue[256];
	
	int repeats = 256 / num_in;
			
	for (int i = 0; i < num_in; i++) {
		for (int j = 0; j < repeats; j++) {
			red[i*repeats + j] = gamma[i*3 + 0] << 8;
			green[i*repeats + j] = gamma[i*3 + 1] << 8;
			blue[i*repeats + j] = gamma[i*3 + 2] << 8;
		}
	}

	// fill remaining entries (if any) with last value
	for (int i = num_in * repeats; i < 256; i++) {
		red[i] = gamma[(num_in - 1) * 3] << 8;
		green[i] = gamma[(num_in - 1) * 3 + 1] << 8;
		blue[i] = gamma[(num_in - 1) * 3 + 2] << 8;
	}
	
	bool changed = (memcmp(red, last_gamma_red, 512) != 0 ||
					memcmp(green, last_gamma_green, 512) != 0 ||
					memcmp(blue, last_gamma_blue, 512) != 0);
	
	if (changed) {
		memcpy(last_gamma_red, red, 512);
		memcpy(last_gamma_green, green, 512);
		memcpy(last_gamma_blue, blue, 512);
		ApplyGammaRamp();
	}

}


void SDLDisplayInstance::pause_redraw()
{
	thread_stop_ack = false;
	thread_stop_req = true;
	while (!thread_stop_ack) ;
}

void SDLDisplayInstance::resume_redraw()
{
	thread_stop_req = false;
}


/*
 *  Switch video mode
 */

#ifdef SHEEPSHAVER
int16 video_mode_change(monitor_desc * monitor, VidLocals *csSave, uint32 ParamPtr)
{
	/* return if no mode change */
	if ((csSave->saveData == ReadMacInt32(ParamPtr + csData)) &&
	    (csSave->saveMode == ReadMacInt16(ParamPtr + csMode))) return noErr;

	/* first find video mode in table */
	int i = monitor->find_mode(ReadMacInt16(ParamPtr + csMode), ReadMacInt32(ParamPtr + csData));
	if (i >= 0) {
			csSave->saveMode = ReadMacInt16(ParamPtr + csMode);
			csSave->saveData = ReadMacInt32(ParamPtr + csData);
			csSave->savePage = ReadMacInt16(ParamPtr + csPage);

			// Disable interrupts and pause redraw thread
			DisableInterrupt();
			SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(monitor);
			sdm->sdl_display.pause_redraw();

			monitor->set_mode(i);
			monitor->switch_to_current_mode();

			WriteMacInt32(ParamPtr + csBaseAddr, monitor->get_screen_base());
			csSave->saveBaseAddr=monitor->get_screen_base();
			const VideoInfo & mode = monitor->get_current_mode();
			csSave->saveData=mode.viAppleID;/* First mode ... */
			csSave->saveMode=mode.viAppleMode;

			// Enable interrupts and resume redraw thread
			sdm->sdl_display.resume_redraw();
			EnableInterrupt();
			return noErr;
	}
	return paramErr;
}
#endif

#ifdef SHEEPSHAVER
bool SDLDisplayInstance::is_cursor_in_display()
{
	int windowX, windowY;
	float cursorX, cursorY;
	int deltaX, deltaY;
	bool out;
	
	// TODO figure out a check for full screen mode
	if (display_type == DISPLAY_SCREEN)
		return true; 

	if (display_type == DISPLAY_WINDOW) {

		if (sdl_window == NULL || SDL_GetMouseFocus() != sdl_window)
			return false;

		SDL_GetWindowPosition(sdl_window, &windowX, &windowY);
		SDL_GetGlobalMouseState(&cursorX, &cursorY);
		deltaX = (int)cursorX - windowX;
		deltaY = (int)cursorY - windowY;
		D(bug("cursor relative {%d,%d}\n", deltaX, deltaY));
		const VIDEO_MODE &mode = drv()->mode;
		float m = get_mag_rate();
		out = deltaX >= 0 && deltaX < VIDEO_MODE_X * m &&
				deltaY >= 0 && deltaY < VIDEO_MODE_Y * m;
		D(bug("cursor in window? %s\n", out? "yes" : "no"));
		return out;
	}

	return false;
}
#endif
	
void SDL_monitor_desc::switch_to_current_mode(void)
{
	// Close and reopen display
	LOCK_EVENTS;
	video_close();
	video_open();
	UNLOCK_EVENTS;

	if (drv == NULL) {
		ErrorAlert(STR_OPEN_WINDOW_ERR);
		QuitEmulator();
	}
}


/*
 *  Can we set the MacOS cursor image into the window?
 */

#ifdef SHEEPSHAVER
bool video_can_change_cursor(void)
{
	return PrefsFindBool("hardcursor");
}
#endif


/*
 *  Set cursor image for window
 */

#ifdef SHEEPSHAVER
void video_set_cursor(void)
{
	for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		if (sdm)
			sdm->sdl_display.set_cursor();
	}
}

void SDLDisplayInstance::set_cursor()
{
	// Set new cursor image if it was changed
	if (sdl_cursor) {
		SDL_DestroyCursor(sdl_cursor);
		sdl_cursor = MagCursor(true);
		if (sdl_cursor) {
			if (drv() && drv()->init_ok) { drv()->monitor.cursorVisible() ? SDL_ShowCursor() : SDL_HideCursor(); }
			SDL_SetCursor(sdl_cursor);

			// XXX Windows apparently needs an extra mouse event to
			// make the new cursor image visible.
			// On Mac, if mouse is grabbed, SDL_ShowCursor() recenters the
			// mouse, we have to put it back.
			bool move = false;
#ifdef WIN32
			move = true;
#elif defined(__APPLE__)
			move = mouse_grabbed;
#endif
			if (move) {
				if (SDL_CursorVisible()) {
					bool cursor_in_window = is_cursor_in_display();

					if (cursor_in_window) {
						float x, y;
						SDL_GetMouseState(&x, &y);
						D(bug("WarpMouse to {%f,%f} via video_set_cursor\n", x, y));
						SDL_WarpMouseInWindow(sdl_window, x, y);
					}
				}
			}
		}
	}
}
#endif


/*
 *  Keyboard-related utilify functions
 */

static bool is_hotkey_down(SDL_KeyboardEvent const &key)
{
	int hotkey = PrefsFindInt32("hotkey");
	if (!hotkey) hotkey = 1;
	return (ctrl_down || (key.mod & SDL_KMOD_CTRL) || !(hotkey & 1)) &&
			(opt_down || (key.mod & SDL_KMOD_ALT) || !(hotkey & 2)) &&
			(cmd_down || (key.mod & SDL_KMOD_GUI) || !(hotkey & 4));
}

static void release_hotkey() {
	int hotkey = PrefsFindInt32("hotkey");
	if (!hotkey) hotkey = 1;

	if (hotkey & 1) ADBKeyUp(0x36); // ctrl
	if (hotkey & 2) ADBKeyUp(0x3a); // opt
	if (hotkey & 4) ADBKeyUp(0x37); // cmd
}

static int modify_opt_cmd(int code) {
	static bool f, c;
	if (!f) {
		f = true;
		c = PrefsFindBool("swap_opt_cmd");
	}
	if (c) {
		switch (code) {
			case 0x37: return 0x3a;
			case 0x3a: return 0x37;
		}
	}
	return code;
}

/*
 *  Translate key event to Mac keycode, returns CODE_INVALID if no keycode was found
 *  and CODE_HOTKEY if the key was recognized as a hotkey
 */

static int event2keycode(SDL_KeyboardEvent const &key, bool key_down)
{
	switch (key.key) {
	case SDLK_A: return 0x00;
	case SDLK_B: return 0x0b;
	case SDLK_C: return 0x08;
	case SDLK_D: return 0x02;
	case SDLK_E: return 0x0e;
	case SDLK_F: return 0x03;
	case SDLK_G: return 0x05;
	case SDLK_H: return 0x04;
	case SDLK_I: return 0x22;
	case SDLK_J: return 0x26;
	case SDLK_K: return 0x28;
	case SDLK_L: return 0x25;
	case SDLK_M: return 0x2e;
	case SDLK_N: return 0x2d;
	case SDLK_O: return 0x1f;
	case SDLK_P: if (is_hotkey_down(key)) {if (!key_down) { release_hotkey(); do_power_key_shutdown(); } return CODE_HOTKEY; } else return 0x23;
	case SDLK_Q: return 0x0c;
	case SDLK_R: return 0x0f;
	case SDLK_S: return 0x01;
	case SDLK_T: return 0x11;
	case SDLK_U: return 0x20;
	case SDLK_V: return 0x09;
	case SDLK_W: return 0x0d;
	case SDLK_X: return 0x07;
	case SDLK_Y: return 0x10;
	case SDLK_Z: return 0x06;

	case SDLK_1: case SDLK_EXCLAIM: return 0x12;
	case SDLK_2: case SDLK_AT: return 0x13;
	case SDLK_3: case SDLK_HASH: return 0x14;
	case SDLK_4: case SDLK_DOLLAR: return 0x15;
	case SDLK_5: return 0x17;
	case SDLK_6: return 0x16;
	case SDLK_7: return 0x1a;
	case SDLK_8: return 0x1c;
	case SDLK_9: return 0x19;
	case SDLK_0: return 0x1d;

	case SDLK_GRAVE: case 167: return 0x32;
	case SDLK_MINUS: case SDLK_UNDERSCORE: return 0x1b;
	case SDLK_EQUALS: case SDLK_PLUS: return 0x18;
	case SDLK_LEFTBRACKET: return 0x21;
	case SDLK_RIGHTBRACKET: return 0x1e;
	case SDLK_BACKSLASH: return 0x2a;
	case SDLK_SEMICOLON: case SDLK_COLON: return 0x29;
	case SDLK_APOSTROPHE: case SDLK_DBLAPOSTROPHE: return 0x27;
	case SDLK_COMMA: case SDLK_LESS: return 0x2b;
	case SDLK_PERIOD: case SDLK_GREATER: return 0x2f;
	case SDLK_SLASH: case SDLK_QUESTION: return 0x2c;

	case SDLK_TAB: if (is_hotkey_down(key)) {if (!key_down) first_drv()->suspend(); return CODE_HOTKEY;} else return 0x30;
	case SDLK_RETURN: if (is_hotkey_down(key)) {if (!key_down) toggle_all_fullscreen(); return CODE_HOTKEY;} else return 0x24;
	case SDLK_SPACE: return 0x31;
	case SDLK_BACKSPACE: return 0x33;

	case SDLK_DELETE: return 0x75;
	case SDLK_INSERT: return 0x72;
	case SDLK_HOME: case SDLK_HELP: return 0x73;
	case SDLK_END: return 0x77;
	case SDLK_PAGEUP: return 0x74;
	case SDLK_PAGEDOWN: return 0x79;

	case SDLK_LCTRL: return 0x36;
	case SDLK_RCTRL: return 0x36;
	case SDLK_LSHIFT: return 0x38;
	case SDLK_RSHIFT: return 0x38;
	case SDLK_LALT: case SDLK_RALT: return 0x3a;
	case SDLK_LGUI: case SDLK_RGUI: return 0x37;
	case SDLK_MENU: return 0x32;
	case SDLK_CAPSLOCK: return 0x39;
	case SDLK_NUMLOCKCLEAR: return 0x47;

	case SDLK_UP: return 0x3e;
	case SDLK_DOWN: return 0x3d;
	case SDLK_LEFT: return 0x3b;
	case SDLK_RIGHT: return 0x3c;

	case SDLK_ESCAPE: if (is_hotkey_down(key)) {if (!key_down) { quit_full_screen = true; emerg_quit = true; } return CODE_HOTKEY;} else return 0x35;

	case SDLK_F1: if (is_hotkey_down(key)) {if (!key_down) SysMountFirstFloppy(); return CODE_HOTKEY;} else return 0x7a;
	case SDLK_F2: return 0x78;
	case SDLK_F3: return 0x63;
	case SDLK_F4: return 0x76;
	case SDLK_F5: return 0x60;
	case SDLK_F6: return 0x61;
	case SDLK_F7: return 0x62;
	case SDLK_F8: return 0x64;
	case SDLK_F9: return 0x65;
	case SDLK_F10: return 0x6d;
	case SDLK_F11: return 0x67;
	case SDLK_F12: return 0x6f;

	case SDLK_PRINTSCREEN: return 0x69;
	case SDLK_SCROLLLOCK: return 0x6b;
	case SDLK_PAUSE: return 0x71;

	case SDLK_KP_0: return 0x52;
	case SDLK_KP_1: return 0x53;
	case SDLK_KP_2: return 0x54;
	case SDLK_KP_3: return 0x55;
	case SDLK_KP_4: return 0x56;
	case SDLK_KP_5: return 0x57;
	case SDLK_KP_6: return 0x58;
	case SDLK_KP_7: return 0x59;
	case SDLK_KP_8: return 0x5b;
	case SDLK_KP_9: return 0x5c;
	case SDLK_KP_PERIOD: return 0x41;
	case SDLK_KP_PLUS: return 0x45;
	case SDLK_KP_MINUS: return 0x4e;
	case SDLK_KP_MULTIPLY: return 0x43;
	case SDLK_KP_DIVIDE: return 0x4b;
	case SDLK_KP_ENTER: return 0x4c;
	case SDLK_KP_EQUALS: return 0x51;
	}
	D(bug("Unhandled SDL keysym: %d\n", key.key));
	return CODE_INVALID;
}

void SDLDisplayInstance::force_complete_window_refresh()
{
	if (!drv()) return;
	if (!drv()->init_ok) return;

	if (display_type == DISPLAY_WINDOW) {
#ifdef ENABLE_VOSF
		if (use_vosf) {	// VOSF refresh
			LOCK_VOSF;
			PFLAG_SET_ALL;
			UNLOCK_VOSF;
		}
#endif
		// Ensure each byte of the_buffer_copy differs from the_buffer to force a full update.
		const VIDEO_MODE &mode = drv()->monitor.get_current_mode();
		const int len = VIDEO_MODE_ROW_BYTES * VIDEO_MODE_Y;
		for (int i = 0; i < len; i++)
			the_buffer_copy[i] = !the_buffer[i];
	}
}

/*
 *  SDL event handling
 */

// possible return codes for SDL-registered event watches
enum {
	EVENT_DROP_FROM_QUEUE = 0,
	EVENT_ADD_TO_QUEUE    = 1
};

// Some events need to be processed in the host-app's main thread, due to
// host-OS requirements.
//
// This function is called by SDL, whenever it generates an SDL_Event.  It has
// the ability to process events, and optionally, to prevent them from being
// added to SDL's event queue (and retrieve-able via SDL_PeepEvents(), etc.)
static bool SDLCALL on_sdl_event_generated(void *userdata, SDL_Event *event)
{
	switch (event->type) {
		case SDL_EVENT_KEY_UP: {
			SDL_KeyboardEvent const &key = event->key;
			switch (key.key) {
				case SDLK_F5: {
					if (is_hotkey_down(key) && !PrefsFindBool("hardcursor")) {
						first_drv()->toggle_mouse_grab();
						return EVENT_DROP_FROM_QUEUE;
					}
				} break;
			}
		} break;

		case SDL_EVENT_DROP_FILE:
			CDROMDrop(event->drop.data);
			return EVENT_DROP_FROM_QUEUE;
			break;
	}
	
	return EVENT_ADD_TO_QUEUE;
}

static driver_base * first_drv() {
	for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		return sdm->sdl_display.drv();
	}
	return NULL;
}


SDL_Window * get_main_sdl_window() {
		if (VideoMonitors.begin() == VideoMonitors.end()) {
			return NULL;
		} else {
			return static_cast<SDL_monitor_desc *>(*VideoMonitors.begin())->sdl_display.get_sdl_window();
		}
}

static void toggle_all_fullscreen() {
	int num = 0;
	for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		if (sdm) {
			D(bug("Set toggle fullscreen %d\n", num));
			sdm->sdl_display.set_toggle_fullscreen();
		}
		num ++;
	}
}


bool SDLDisplayInstance::has_window_id(SDL_WindowID win_id) const
{
	return sdl_window && SDL_GetWindowID(sdl_window) == win_id;
}

static SDLDisplayInstance * display_instance_for_windowID(SDL_WindowID win_id)
{
	for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		if (sdm && sdm->sdl_display.has_window_id(win_id))
			return &sdm->sdl_display;
	}
	return NULL;
}

static void do_power_key_shutdown()
{
	if (SDL_GetModState() & (SDL_KMOD_LALT | SDL_KMOD_RALT)) return;
	ADBKeyDown(0x7f);	// Power key
	ADBKeyUp(0x7f);
}

static void handle_events(void)
{
	SDL_Event events[10];
	const int n_max_events = sizeof(events) / sizeof(events[0]);
	int n_events;

	while ((n_events = SDL_PeepEvents(events, n_max_events, SDL_GETEVENT, SDL_EVENT_FIRST, SDL_EVENT_LAST)) > 0) {
		for (int i = 0; i < n_events; i++) {
			SDL_Event & event = events[i];
			
			switch (event.type) {

			// Mouse button
			case SDL_EVENT_MOUSE_BUTTON_DOWN: {
				unsigned int button = event.button.button;
				if (button == SDL_BUTTON_LEFT)
					ADBMouseDown(0);
				else if (button == SDL_BUTTON_RIGHT)
					ADBMouseDown(1);
				else if (button == SDL_BUTTON_MIDDLE)
					ADBMouseDown(2);
				break;
			}
			case SDL_EVENT_MOUSE_BUTTON_UP: {
				unsigned int button = event.button.button;
				if (button == SDL_BUTTON_LEFT)
					ADBMouseUp(0);
				else if (button == SDL_BUTTON_RIGHT)
					ADBMouseUp(1);
				else if (button == SDL_BUTTON_MIDDLE)
					ADBMouseUp(2);
				break;
			}

			// Mouse moved
			case SDL_EVENT_MOUSE_MOTION:
				if (mouse_grabbed) {
					// for the purpose of grabbed we can just always put the events through the first window
					first_drv()->mouse_moved(event.motion.xrel, event.motion.yrel);
				} else {
					SDLDisplayInstance * sdi = display_instance_for_windowID(event.motion.windowID);
					if (!sdi) {
						// event wasn't associated with any window
						break;
					}
					SDL_ConvertEventToRenderCoordinates(sdi->renderer(), &event);
#ifdef VIDEO_CHROMAKEY
					if (display_type == DISPLAY_CHROMAKEY) {
						int xl, yl;
						ADBGetPosition(xl, yl);
						uint8 *p = (uint8 *)host_surface->pixels;
						const int OFS = 2;
						if (xl >= OFS) p += (xl - OFS) * SDL_GetPixelFormatDetails(host_surface->format)->bytes_per_pixel;
						if (yl >= OFS) p += (yl - OFS) * host_surface->pitch;
						set_mouse_ignore(sdl_window, *(uint32 *)p == VIDEO_CHROMAKEY);
					}
#endif
					//D(bug("move: %d, %f,%f\n", event.motion.windowID, event.motion.x, event.motion.y));

					// Some mouse motion events are for parts of the window outside of the monitor video,
					// like the area of the letterbox in a letterboxed full screen mode.
					//
					// Clip the motion events we handle to the actual part of the window with monitor video
					// This will limit the coordinates we send to the emulated mac to the range of the window
					static bool prev_clipped = false;
					static bool prev_cursor_visible;

					if (sdi && sdi->drv() && sdi->drv()->init_ok &&
						event.motion.x >= 0 && event.motion.x < sdi->drv()->VIDEO_MODE_X &&
						event.motion.y >= 0 && event.motion.y < sdi->drv()->VIDEO_MODE_Y) {
							// Cursor in range
							sdi->drv()->mouse_moved(event.motion.x, event.motion.y);
							if (prev_clipped) {
								// Turn the mouse cursor back off it was off before
								if (!prev_cursor_visible)
									SDL_HideCursor();
								prev_clipped = false;
							}
					} else {
						if (!prev_clipped) {
							// Turn the host mouse cursor on so that the user can see it to move it
							// across the letterbox
							prev_cursor_visible = SDL_CursorVisible();
							SDL_ShowCursor();
							prev_clipped = true;
						}
					}
				}
				break;

			case SDL_EVENT_MOUSE_WHEEL:
				if (!event.wheel.y) break;
				if (!mouse_wheel_mode) {
					int key = (event.wheel.y < 0) ^ mouse_wheel_reverse ? 0x79 : 0x74;	// Page up/down
					ADBKeyDown(key);
					ADBKeyUp(key);
				}
				else {
					int key = (event.wheel.y < 0) ^ mouse_wheel_reverse ? 0x3d : 0x3e;	// Cursor up/down
					for (int i = 0; i < mouse_wheel_lines; i++) {
						ADBKeyDown(key);
						ADBKeyUp(key);
					}
				}
			break;

			// Keyboard
			case SDL_EVENT_KEY_DOWN: {
				if (event.key.repeat)
					break;
				int code = CODE_INVALID;
				if (use_keycodes && event2keycode(event.key, true) != CODE_HOTKEY)
					code = keycode_table[event.key.scancode & 0xff];
				if (code == CODE_INVALID)
					code = event2keycode(event.key, true);
				if (code >= 0) {
					if (!emul_suspended) {
						code = modify_opt_cmd(code);
						if (code == 0x39)
							(SDL_GetModState() & SDL_KMOD_CAPS ? ADBKeyDown : ADBKeyUp)(code);
						else
							ADBKeyDown(code);
						if (code == 0x36)
							ctrl_down = true;
						if (code == 0x3a)
							opt_down = true;
						if (code == 0x37)
							cmd_down = true;
						
					} else {
						if (code == 0x31)
							first_drv()->resume();	// Space wakes us up
					}
				}
				break;
			}
			case SDL_EVENT_KEY_UP: {
				int code = CODE_INVALID;
				if (use_keycodes && event2keycode(event.key, false) != CODE_HOTKEY)
					code = keycode_table[event.key.scancode & 0xff];
				if (code == CODE_INVALID)
					code = event2keycode(event.key, false);
				if (code >= 0) {
					code = modify_opt_cmd(code);
					if (code != 0x39)
						ADBKeyUp(code);
					if (code == 0x36)
						ctrl_down = false;
					if (code == 0x3a)
						opt_down = false;
					if (code == 0x37)
						cmd_down = false;
				}
				break;
			}
			
			case SDL_EVENT_WINDOW_EXPOSED: {
				// Hidden parts exposed, force complete refresh of window
				SDLDisplayInstance * sdi = display_instance_for_windowID(event.window.windowID);
				if (sdi)
					sdi->force_complete_window_refresh();
				break;
			}
					
			case SDL_EVENT_WINDOW_RESTORED: {
				// Force a complete window refresh when activating, to avoid redraw artifacts otherwise.
				SDLDisplayInstance * sdi = display_instance_for_windowID(event.window.windowID);
				if (sdi)
					sdi->force_complete_window_refresh();
				break;
			}

			// Window "close" widget clicked
			case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
				do_power_key_shutdown();
				break;
			}
		}
	}
}


/*
 *  Window display update
 */

// Static display update (fixed frame rate, but incremental)
static void update_display_static(driver_base *drv)
{
	if (!drv || !drv->init_ok) return;
	// Incremental update code
	int wide = 0, high = 0;
	uint32 x1, x2, y1, y2;
	const VIDEO_MODE &mode = drv->mode;
	int bytes_per_row = VIDEO_MODE_ROW_BYTES;
	uint8 *p, *p2;
	uint32 x2_clipped, wide_clipped;

	uint8 *the_buffer = drv->monitor.sdl_display.host_buffer();
	uint8 *the_buffer_copy = drv->monitor.sdl_display.host_buffer_copy();
	Screen_blit_func Screen_blit = drv->monitor.sdl_display.Screen_blit;

	// Check for first line from top and first line from bottom that have changed
	y1 = 0;
	for (uint32 j = 0; j < VIDEO_MODE_Y; j++) {
		if (memcmp(&the_buffer[j * bytes_per_row], &the_buffer_copy[j * bytes_per_row], bytes_per_row)) {
			y1 = j;
			break;
		}
	}
	y2 = y1 - 1;
	for (uint32 j = VIDEO_MODE_Y; j-- > y1; ) {
		if (memcmp(&the_buffer[j * bytes_per_row], &the_buffer_copy[j * bytes_per_row], bytes_per_row)) {
			y2 = j;
			break;
		}
	}
	high = y2 - y1 + 1;

	// Check for first column from left and first column from right that have changed
	if (high) {
		if ((int)VIDEO_MODE_DEPTH < (int)VIDEO_DEPTH_8BIT) {
			const int src_bytes_per_row = bytes_per_row;
			const int dst_bytes_per_row = drv->s->pitch;
			const int pixels_per_byte = 8/mac_depth_of_video_depth(VIDEO_MODE_DEPTH);

			const uint32 line_len = TrivialBytesPerRow(VIDEO_MODE_X, VIDEO_MODE_DEPTH);
			
			x1 = line_len;
			for (uint32 j = y1; j <= y2; j++) {
				p = &the_buffer[j * bytes_per_row];
				p2 = &the_buffer_copy[j * bytes_per_row];
				for (uint32 i = 0; i < x1; i++) {
					if (*p != *p2) {
						x1 = i;
						break;
					}
					p++; p2++;
				}
			}
			x2 = x1;
			for (uint32 j = y1; j <= y2; j++) {
				p = &the_buffer[j * bytes_per_row];
				p2 = &the_buffer_copy[j * bytes_per_row];
				p += bytes_per_row;
				p2 += bytes_per_row;
				for (uint32 i = line_len; i > x2; i--) {
					p--; p2--;
					if (*p != *p2) {
						x2 = i;
						break;
					}
				}
			}

			x1 *= pixels_per_byte;
			x2 *= pixels_per_byte;
			wide = x2 - x1;
			x2_clipped = x2 > VIDEO_MODE_X? VIDEO_MODE_X : x2;
			wide_clipped = x2_clipped - x1;

			// Update copy of the_buffer
			if (high && wide) {

				// Lock surface, if required
				if (SDL_MUSTLOCK(drv->s))
					SDL_LockSurface(drv->s);

				// Blit to screen surface
				int si = y1 * src_bytes_per_row + (x1 / pixels_per_byte);
				int di = y1 * dst_bytes_per_row + x1;
				for (uint32 j = y1; j <= y2; j++) {
					memcpy(the_buffer_copy + si, the_buffer + si, wide / pixels_per_byte);
					Screen_blit((uint8 *)drv->s->pixels + di, the_buffer + si, wide / pixels_per_byte);
					si += src_bytes_per_row;
					di += dst_bytes_per_row;
				}

				// Unlock surface, if required
				if (SDL_MUSTLOCK(drv->s))
					SDL_UnlockSurface(drv->s);

				// Refresh display
				drv->monitor.sdl_display.update_sdl_video(drv->s, x1, y1, wide_clipped, high);
			}

		} else {
			const int bytes_per_pixel = VIDEO_MODE_ROW_BYTES / VIDEO_MODE_X;
			const int dst_bytes_per_row = drv->s->pitch;

			x1 = VIDEO_MODE_X;
			for (uint32 j = y1; j <= y2; j++) {
				p = &the_buffer[j * bytes_per_row];
				p2 = &the_buffer_copy[j * bytes_per_row];
				for (uint32 i = 0; i < x1 * bytes_per_pixel; i++) {
					if (*p != *p2) {
						x1 = i / bytes_per_pixel;
						break;
					}
					p++; p2++;
				}
			}
			x2 = x1;
			for (uint32 j = y1; j <= y2; j++) {
				p = &the_buffer[j * bytes_per_row];
				p2 = &the_buffer_copy[j * bytes_per_row];
				p += bytes_per_row;
				p2 += bytes_per_row;
				for (uint32 i = VIDEO_MODE_X * bytes_per_pixel; i > x2 * bytes_per_pixel; i--) {
					p--;
					p2--;
					if (*p != *p2) {
						x2 = i / bytes_per_pixel;
						break;
					}
				}
			}
			wide = x2 - x1;

			// Update copy of the_buffer
			if (high && wide) {

				// Lock surface, if required
				if (SDL_MUSTLOCK(drv->s))
					SDL_LockSurface(drv->s);

				// Blit to screen surface
				for (uint32 j = y1; j <= y2; j++) {
					uint32 i = j * bytes_per_row + x1 * bytes_per_pixel;
					int dst_i = j * dst_bytes_per_row + x1 * bytes_per_pixel;
					memcpy(the_buffer_copy + i, the_buffer + i, bytes_per_pixel * wide);
					Screen_blit((uint8 *)drv->s->pixels + dst_i, the_buffer + i, bytes_per_pixel * wide);
				}

				// Unlock surface, if required
				if (SDL_MUSTLOCK(drv->s))
					SDL_UnlockSurface(drv->s);

				// Refresh display
				drv->monitor.sdl_display.update_sdl_video(drv->s, x1, y1, wide, high);
			}
		}
	}
}

// Static display update (fixed frame rate, bounding boxes based)
// XXX use NQD bounding boxes to help detect dirty areas?
static void update_display_static_bbox(driver_base *drv)
{
	if (!drv || !drv->init_ok) return;
	const VIDEO_MODE &mode = drv->mode;
	bool blit = (int)VIDEO_MODE_DEPTH == VIDEO_DEPTH_16BIT;

	// Allocate bounding boxes for SDL_UpdateRects()
	const uint32 N_PIXELS = 64;
	const uint32 n_x_boxes = (VIDEO_MODE_X + N_PIXELS - 1) / N_PIXELS;
	const uint32 n_y_boxes = (VIDEO_MODE_Y + N_PIXELS - 1) / N_PIXELS;
	SDL_Rect *boxes = (SDL_Rect *)alloca(sizeof(SDL_Rect) * n_x_boxes * n_y_boxes);
	uint32 nr_boxes = 0;

	assert(drv->init_ok);

	uint8 *the_buffer = drv->monitor.sdl_display.host_buffer();
	uint8 *the_buffer_copy = drv->monitor.sdl_display.host_buffer_copy();
	Screen_blit_func Screen_blit = drv->monitor.sdl_display.Screen_blit;

	// FIXME precaution for surface not init
	if (!drv->s) return;

	// Lock surface, if required
	if (SDL_MUSTLOCK(drv->s))
		SDL_LockSurface(drv->s);

	// Update the surface from Mac screen
	const uint32 bytes_per_row = VIDEO_MODE_ROW_BYTES;
	const uint32 bytes_per_pixel = bytes_per_row / VIDEO_MODE_X;
	const uint32 dst_bytes_per_row = drv->s->pitch;
	for (uint32 y = 0; y < VIDEO_MODE_Y; y += N_PIXELS) {
		uint32 h = N_PIXELS;
		if (h > VIDEO_MODE_Y - y)
			h = VIDEO_MODE_Y - y;
		for (uint32 x = 0; x < VIDEO_MODE_X; x += N_PIXELS) {
			uint32 w = N_PIXELS;
			if (w > VIDEO_MODE_X - x)
				w = VIDEO_MODE_X - x;
			const int xs = w * bytes_per_pixel;
			const int xb = x * bytes_per_pixel;
			bool dirty = false;
			for (uint32 j = y; j < (y + h); j++) {
				const uint32 yb = j * bytes_per_row;
				const uint32 dst_yb = j * dst_bytes_per_row;
				if (memcmp(&the_buffer[yb + xb], &the_buffer_copy[yb + xb], xs) != 0) {
					memcpy(&the_buffer_copy[yb + xb], &the_buffer[yb + xb], xs);
					if (blit) Screen_blit((uint8 *)drv->s->pixels + dst_yb + xb, the_buffer + yb + xb, xs);
					dirty = true;
				}
			}
			if (dirty) {
				boxes[nr_boxes].x = x;
				boxes[nr_boxes].y = y;
				boxes[nr_boxes].w = w;
				boxes[nr_boxes].h = h;
				nr_boxes++;
			}
		}
	}

	// Unlock surface, if required
	if (SDL_MUSTLOCK(drv->s))
		SDL_UnlockSurface(drv->s);

	// Refresh display
	if (nr_boxes)
		drv->monitor.sdl_display.update_sdl_video(drv->s, nr_boxes, boxes);
}


// We suggest the compiler to inline the next two functions so that it
// may specialise the code according to the current screen depth and
// display type. A clever compiler would do that job by itself though...

// NOTE: update_display_vosf is inlined too

static inline void possibly_quit_dga_mode()
{
	// Quit DGA mode if requested (something terrible has happened and we
	// want to give control back to the user)
	if (quit_full_screen) {
		quit_full_screen = false;

		for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
			SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
			if (sdm)
				sdm->emergency_stop();
		}
	}
}

void SDL_monitor_desc::emergency_stop()
{
	delete drv;
	drv = NULL;
	sdl_display.set_driver_base(NULL);
}

static inline void possibly_ungrab_mouse()
{
	// Ungrab mouse if requested (something terrible has happened and we
	// want to give control back to the user)
	if (quit_full_screen) {
		quit_full_screen = false;
		if (first_drv())
			first_drv()->ungrab_mouse();
	}
}

inline void SDLDisplayInstance::handle_palette_changes(void)
{
	LOCK_PALETTE;

	if (sdl_palette_changed) {
		sdl_palette_changed = false;
		drv()->update_palette();
	}

	UNLOCK_PALETTE;
}

static void video_refresh_window_static(SDL_monitor_desc * desc);

static void video_refresh_dga(SDL_monitor_desc * desc)
{
	// Quit DGA mode if requested
	possibly_quit_dga_mode();
	video_refresh_window_static(desc);
}

#ifdef ENABLE_VOSF
#if REAL_ADDRESSING || DIRECT_ADDRESSING
static void video_refresh_dga_vosf(void)
{
	// Quit DGA mode if requested
	possibly_quit_dga_mode();
	
	// Update display (VOSF variant)
	static uint32 tick_counter = 0;
	if (++tick_counter >= frame_skip) {
		tick_counter = 0;
		if (mainBuffer.dirty) {
			LOCK_VOSF;
			update_display_dga_vosf(drv);
			UNLOCK_VOSF;
		}
	}
}
#endif

static void video_refresh_window_vosf(void)
{
	// Ungrab mouse if requested
	possibly_ungrab_mouse();
	
	// Update display (VOSF variant)
	uint32 & tick_counter = sdm->sdl_display.tick_counter;
	if (++tick_counter >= frame_skip) {
		tick_counter = 0;
		if (mainBuffer.dirty) {
			LOCK_VOSF;
			update_display_window_vosf(drv);
			UNLOCK_VOSF;
		}
	}
}
#endif // def ENABLE_VOSF

static void video_refresh_window_static(SDL_monitor_desc * sdm)
{
	// Ungrab mouse if requested
	possibly_ungrab_mouse();

	if (sdm) {
		driver_base * drv = sdm->sdl_display.drv();

		if (drv && drv->init_ok) {
			// Update display (static variant)
			uint32 & tick_counter = sdm->sdl_display.tick_counter;
			if (++tick_counter >= frame_skip) {
				tick_counter = 0;
				const VIDEO_MODE &mode = drv->mode;
				if ((int)VIDEO_MODE_DEPTH >= VIDEO_DEPTH_8BIT)
					update_display_static_bbox(drv);
				else
					update_display_static(drv);
			}
		}
	}
}


/*
 *  Thread for screen refresh, input handling etc.
 */

static void VideoRefreshInit(void)
{
	// TODO: set up specialised 8bpp VideoRefresh handlers ?
	if (first_drv()->monitor.sdl_display.get_display_type() == DISPLAY_SCREEN) {
#if ENABLE_VOSF && (REAL_ADDRESSING || DIRECT_ADDRESSING)
		if (use_vosf)
			video_refresh = video_refresh_dga_vosf;
		else
#endif
			video_refresh = video_refresh_dga;
	}
	else {
#ifdef ENABLE_VOSF
		if (use_vosf)
			video_refresh = video_refresh_window_vosf;
		else
#endif
			video_refresh = video_refresh_window_static;
	}
}

inline void SDLDisplayInstance::do_video_refresh(void)
{
	// Handle SDL events
	handle_events();

	// Update display
	video_refresh(&drv()->monitor);

	// Set new palette if it was changed
	handle_palette_changes();
}

// This function is called on non-threaded platforms from a timer interrupt
void VideoRefresh(void)
{
	for (vector<monitor_desc *>::iterator i = VideoMonitors.begin(); i != VideoMonitors.end(); ++i) {
		SDL_monitor_desc * sdm = static_cast<SDL_monitor_desc *>(*i);
		if (sdm && sdm->sdl_display.drv() && sdm->sdl_display.drv()->init_ok)
			sdm->sdl_display.VideoRefresh();
	}

}

void SDLDisplayInstance::VideoRefresh(void)
{
	// We need to check redraw_thread_active to inhibit refreshed during
	// mode changes on non-threaded platforms
	if (!redraw_thread_active)
		return;

	// Process pending events and update display
	do_video_refresh();
}

const int VIDEO_REFRESH_HZ = 60;
const int VIDEO_REFRESH_DELAY = 1000000 / VIDEO_REFRESH_HZ;

#ifndef USE_CPU_EMUL_SERVICES
static int redraw_func(void *arg)
{
	SDLDisplayInstance * sdi = (SDLDisplayInstance *)arg;
	return sdi->instance_redraw_func();
}

int SDLDisplayInstance::instance_redraw_func()
{
	uint64 start = GetTicks_usec();
	int64 ticks = 0;
	uint64 next = start + VIDEO_REFRESH_DELAY;

	while (!redraw_thread_cancel) {

		// Wait
		next += VIDEO_REFRESH_DELAY;
		int32 delay = int32(next - GetTicks_usec());
		if (delay > 0)
			Delay_usec(delay);
		else if (delay < -VIDEO_REFRESH_DELAY)
			next = GetTicks_usec();
		ticks++;

		// Pause if requested (during video mode switches)
		if (thread_stop_req) {
			thread_stop_ack = true;
			continue;
		}

		// Process pending events and update display
		do_video_refresh();
	}

#if DEBUG
	uint64 end = GetTicks_usec();
	D(bug("%" PRId64 " refreshes in %" PRId64 " usec = %f refreshes/sec\n", ticks, end - start, ticks * 1000000.0 / (end - start)));
#endif
	return 0;
}
#endif


/*
 *  Record dirty area from NQD
 */

#ifdef SHEEPSHAVER
void video_set_dirty_area(int x, int y, int w, int h)
{
#ifdef ENABLE_VOSF
	const VIDEO_MODE &mode = drv->mode;
	const unsigned screen_width = VIDEO_MODE_X;
	const unsigned screen_height = VIDEO_MODE_Y;
	const unsigned bytes_per_row = VIDEO_MODE_ROW_BYTES;

	if (use_vosf) {
		vosf_set_dirty_area(x, y, w, h, screen_width, screen_height, bytes_per_row);
		return;
	}
#endif

	// XXX handle dirty bounding boxes for non-VOSF modes
}
#endif

#endif	// ends: SDL version check
