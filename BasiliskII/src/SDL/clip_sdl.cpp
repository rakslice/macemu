/*
 *  clip_sdl.cpp - Clipboard handling, sdl implementation
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

#include "sysdeps.h"
#include "main.h"
#include "emul_op.h"

#include "clip.h"
#include "macos_util.h"
#include "pict.h"

#define DEBUG 0
#include "debug.h"

#include <SDL.h>
#include <algorithm>
#include <inttypes.h>

// Prototypes
static char * fourcc_str(uint32 type);
static void byte_sum(const void * data, size_t data_len);

// Strategy notes
//
// In general:
//
// This implementation follows the strategy of the old clip_unix and similar.
//
// When an application on the emulated mac puts something on the guest clipboard
// we immediately update the host clipboard.
//
// We don't do anything right away when the host clipboard changes.
//
// Instead the guest mac clipboard keeps its existing prior contents
// independently, and right when an application on the emulated mac is requesting the
// clipboard, we check the host clipboard and if we want to use it instead
// we replace the guest clipboard with it so that's what the guest application sees.
//
// The detail specific to this SDL implementation is:
//
// There is no way here to 100% tell if the host or guest clipboard content is newer.
// In X11 and similar we don't get good SDL events for monitoring the host clipboard
// due to the constant flow of irrelevant clipboard update events
// about for primary selection changes.
//
// And this kind of event-based change monitoring is somewhat unwise to begin with
// if it's possible for events to not make it 100% of the time.
//
// What I'm going with is that we cache the value we last updated the host
// clipboard with on behalf of a mac clipboard update (which we are guaranteed
// to not miss,) and if what we have on the host clipboard is still the same as that,
// we assume that it's from the guest and leave the guest clipboard intact
// so the better formats there get used.
//
// It seems the case where this is as an incorrect assumption, would not
// happen much in normal use, and if it does happen the consequences
// of just using these alternatives seem not too wrong.

class DataCache {
public:
	DataCache() : prev_val(NULL), prev_val_len(0) { }

	bool different_from(const void * new_val, size_t new_val_len) {
		// D(bug("DIFFERENT FROM: %p %zu %p %zu\n", prev_val, prev_val_len, new_val, new_val_len));
		// D(bug(" Previous:\n"));
		// byte_sum(prev_val, prev_val_len);
		bool out;
		if (new_val != NULL) {
			if (prev_val != NULL) {
				if (prev_val_len != new_val_len)
					out = true;
				else {
					int result = memcmp(new_val, prev_val, prev_val_len);
					// D(bug("  memcmp result is %d\n", result));
					if (result != 0) {
						uint64 bytes_different = 0;
						for (size_t i = 0; i < prev_val_len; i++)
							if ( ((uint8 *)new_val)[i] != ((uint8 *)prev_val)[i] )
								bytes_different++;
						D(bug("  %" PRIu64 " bytes different\n", bytes_different));
					}
					out = (result != 0);
				}
			} else {
				out = true;
			}
		} else {
			if (prev_val) {
				out = true;
			} else {
				// both null
				out = false;
			}
		}
		return out;
	}

	// Set the val. The val will later be freed with SDL_free; use SDL_strdup if necessary.
	void update(void * new_val, size_t new_val_len) {
		// update last_primary_selection_text
		if (prev_val) {
			SDL_free(prev_val);
		}
		prev_val = new_val;
		prev_val_len = new_val_len;
	}

	void free() {
		update(NULL, 0);
	}

protected:
	void * prev_val;
	size_t prev_val_len;
};

class TextCache {
public:
	bool different_from(const char * new_val) { return d.different_from(new_val, size_of(new_val)); }
	void update(char * new_val) { d.update(new_val, size_of(new_val)); }
	void free() { d.free(); }
protected:
	static size_t size_of(const char * val) { return (val == NULL)? 0 : strlen(val); }
	DataCache d;
};


// Flag indicating the emulated mac PutScrap currently in progress is from our code in GetScrap(), and PutScrap() should
// not treat it as a possible update from an application on the emulated mac
static bool we_put_this_data = false;

static bool we_zeroed = false;

// Flag indicating the emulated mac GetScrap currently in progress is our own read and shouldn't be intercepted
static bool we_are_getting_data = false;

// When something is copied on the emulated mac, we don't want to round trip it through the
// host clipboard if it is pasted again in the emulated mac, incurring unnecessary lossy format conversions.

// We just assume that if the clipboard text content is the same we last got from the mac,
// we should let its native clipboard win

TextCache last_clipboard_from_mac;

// These others are for telling when we've already converted the host clipboard to the emulated mac clipboard
// and it's still in place so we don't need to do it again.

DataCache last_timestamp_from_host;

DataCache last_bmp_from_host;
DataCache last_rgba_from_host;
TextCache last_text_from_host;

// Just for checking the integrity of the clipboard
DataCache last_pict_put_in_clipboard;

// Clear host clipboard data caches used to shortcut updates of the emulated mac clipboard state
static void ClearHostClipboardChecks() {
	last_bmp_from_host.free();
	last_rgba_from_host.free();
	last_text_from_host.free();

	// We don't reset timestamps here; a previously seen timestamp definitely indicates the host
	// clipboard is stale, unlike other clipboard contents where you could have copied the same data again

	last_pict_put_in_clipboard.free();
}

SDL_iconv_t to, from;

#define MAC_ICONV_CHARSET "MACINTOSH"
#define HOST_ICONV_CHARSET "UTF-8"

// These return a char * that needs to be freed with SDL_free, or NULL if the conversion fails
// (e.g. because the charset is not supported)
#define CONVERT_HOST_TO_MAC_STRING(data, len)  SDL_iconv_string(MAC_ICONV_CHARSET, HOST_ICONV_CHARSET, data, len);
#define CONVERT_MAC_TO_HOST_STRING(data, len)  SDL_iconv_string(HOST_ICONV_CHARSET, MAC_ICONV_CHARSET, data, len);

/*
 *  Initialization
 */

void ClipInit(void)
{
	to = SDL_iconv_open(MAC_ICONV_CHARSET, HOST_ICONV_CHARSET);
	if (to == (SDL_iconv_t) SDL_ICONV_ERROR) {
		D(bug("iconv open to failed\n"));
	}
	from = SDL_iconv_open(HOST_ICONV_CHARSET, MAC_ICONV_CHARSET);
	if (from == (SDL_iconv_t) SDL_ICONV_ERROR) {
		D(bug("iconv open from failed\n"));
	}
}


/*
 *  Deinitialization
 */

void ClipExit(void)
{
	last_clipboard_from_mac.free();
	ClearHostClipboardChecks();
	last_timestamp_from_host.free();
	last_pict_put_in_clipboard.free();
	SDL_iconv_close(from);
	SDL_iconv_close(to);
}

extern "C" void hexdump(char * data, int len);

void hexdump(char * data, int len) {
	if (!DEBUG) return;
	const int line_len = 8;
	for (int line_start = 0; line_start < len; line_start += line_len) {
		const int hex_len = line_len * 3;
		const int literal_len = line_len;
		const int n = 256; // hex_len + 1 + literal_len + 1;
		char buf[n];
		int pos = 0;
		for (int i = line_start; i < std::min(len, line_start + line_len); i++) {
			pos += snprintf(buf + pos, n - pos, "%02x ", (unsigned char) data[i]);
		}
		pos += snprintf(buf + pos, n - pos, " ");
		for (int i = line_start; i < std::min(len, line_start + line_len); i++) {
			unsigned char c = (unsigned char) data[i];
			if (c < 32 || c > 126) c = 0x5f; // _
			pos += snprintf(buf + pos, n - pos, "%c", c);
		}

		D(bug("   %04x: %s\n", line_start, buf));
	}
}

static bool zero_scrap()
{
	D(bug("zero_scrap()\n"));
	// Zero the clipboard
	static uint8 proc[] = {
		0x59, 0x8f,					// subq.l	#4,sp
		0xa9, 0xfc,					// ZeroScrap()
		0x58, 0x8f,					// addq.l	#4,sp
		M68K_RTS >> 8, M68K_RTS & 0xff
	};
	M68kRegisters r;
	r.d[0] = sizeof(proc);
	Execute68kTrap(0xa71e, &r);		// NewPtrSysClear()
	uint32 proc_area = r.a[0];

	if (proc_area) {
		D(bug("Clearing the mac clipboard\n"));

		// The procedure is run-time generated because it must lays in
		// Mac address space. This is mandatory for "33-bit" address
		// space optimization on 64-bit platforms because the static
		// proc[] array is not remapped
		Host2Mac_memcpy(proc_area, proc, sizeof(proc));
		we_zeroed = true;
		Execute68k(proc_area, &r);

		// We are done with scratch memory
		r.a[0] = proc_area;
		Execute68kTrap(0xa01f, &r);		// DisposePtr

		return true;
	}
	return false;
}

static void * read_scrap(uint32 type, size_t * out_length) {
	void * out = NULL;

	D(bug("read_scrap() type 0x%08x (%s)\n", type, fourcc_str(type)));
	// Allocate space for the offset storage in MacOS side
	M68kRegisters r;
	r.d[0] = 4;
	Execute68kTrap(0xa71e, &r);			// NewPtrSysClear()
	uint32 offset_storage_area = r.a[0];

	if (offset_storage_area) {
		// Routine for calling GetScrap
		static uint8 proc[] = {
			0x59, 0x8f,					// subq.l	#4,sp
			0x2f, 0x3c, 0, 0, 0, 0,		// move.l	#hDest,-(sp)
			0x2f, 0x3c, 0, 0, 0, 0,		// move.l	#type,-(sp)
			0x2f, 0x3c, 0, 0, 0, 0,		// move.l	#&offset,-(sp)
			0xa9, 0xfd,					// GetScrap()
			0x20, 0x1f,					// move.l	(sp)+,d0
			M68K_RTS >> 8, M68K_RTS & 0xff
		};
		r.d[0] = sizeof(proc);
		Execute68kTrap(0xa71e, &r);		// NewPtrSysClear()
		uint32 proc_area = r.a[0];

		if (proc_area) {
			Host2Mac_memcpy(proc_area, proc, sizeof(proc));

			D(bug("Getting size of clipboard data\n"));

			WriteMacInt32(proc_area +  4, 0); // hDest = NULL
			WriteMacInt32(proc_area + 10, type);
			WriteMacInt32(proc_area + 16, offset_storage_area);
			we_are_getting_data = true;
			Execute68k(proc_area, &r);
			uint32 data_size = r.d[0];

			if (data_size > 0) {
				// there is data available
				r.d[0] = data_size;
				Execute68kTrap(0xa722, &r);		// NewHandleSysClear()
				uint32 clipboard_data_handle = r.a[0];

				if (clipboard_data_handle) {
					// allocate a buffer for it
					out = SDL_malloc(data_size);
					if (out) {

						if (out_length) {
							*out_length = data_size;
						}

						D(bug("Getting clipboard data\n"));
						WriteMacInt32(proc_area +  4, clipboard_data_handle); // hDest
						WriteMacInt32(proc_area + 10, type);
						WriteMacInt32(proc_area + 16, offset_storage_area);
						we_are_getting_data = true;
						Execute68k(proc_area, &r);
						data_size = r.d[0];

						uint32 clipboard_data_ptr = ReadMacInt32(clipboard_data_handle);

						Mac2Host_memcpy(out, clipboard_data_ptr, data_size);

						
					} else {
						D(bug("couldn't allocate sdl buffer\n"));
					}

					// done with the handle
					r.a[0] = clipboard_data_handle;
					Execute68kTrap(0xa023, &r);		// DisposeHandle()

				} else {
					D(bug("couldn't allocate clipboard data handle\n"));
				}


			} else {
				D(bug("No clipboard data for type available\n"));

			}

			// We are done with scratch memory
			r.a[0] = proc_area;
			Execute68kTrap(0xa01f, &r);		// DisposePtr
		}
		r.a[0] = offset_storage_area;
		Execute68kTrap(0xa01f, &r);		// DisposePtr

		D(bug("done read_scrap()\n"));
	}

	return out;
}

static void write_scrap(uint32 type, void * data, size_t data_len)
{
	D(bug("write_scrap() type 0x%08x (%s), data_len %zu bytes\n", type, fourcc_str(type), data_len));
	// Allocate space for new scrap in MacOS side
	M68kRegisters r;
	r.d[0] = data_len;
	Execute68kTrap(0xa71e, &r);			// NewPtrSysClear()
	uint32 scrap_area = r.a[0];

	if (scrap_area) {
		Host2Mac_memcpy(scrap_area, data, data_len);

		// Add new data to clipboard
		static uint8 proc[] = {
			0x59, 0x8f,					// subq.l	#4,sp
			0x2f, 0x3c, 0, 0, 0, 0,		// move.l	#length,-(sp)
			0x2f, 0x3c, 0, 0, 0, 0,		// move.l	#type,-(sp)
			0x2f, 0x3c, 0, 0, 0, 0,		// move.l	#outbuf,-(sp)
			0xa9, 0xfe,					// PutScrap()
			0x58, 0x8f,					// addq.l	#4,sp
			M68K_RTS >> 8, M68K_RTS & 0xff
		};
		r.d[0] = sizeof(proc);
		//Execute68kTrap(0xa31e, &r);		// NewPtrClear()
		Execute68kTrap(0xa71e, &r);		// NewPtrSysClear()
		uint32 proc_area = r.a[0];

		if (proc_area) {
			D(bug("Putting clipboard data\n"));

			// The procedure is run-time generated because it must lays in
			// Mac address space. This is mandatory for "33-bit" address
			// space optimization on 64-bit platforms because the static
			// proc[] array is not remapped
			Host2Mac_memcpy(proc_area, proc, sizeof(proc));
			WriteMacInt32(proc_area +  4, data_len);
			WriteMacInt32(proc_area + 10, type);
			WriteMacInt32(proc_area + 16, scrap_area);
			we_put_this_data = true;
			Execute68k(proc_area, &r);

			

			// We are done with scratch memory
			r.a[0] = proc_area;
			Execute68kTrap(0xa01f, &r);		// DisposePtr
		}
		r.a[0] = scrap_area;
		Execute68kTrap(0xa01f, &r);		// DisposePtr
		D(bug("done write_scrap()\n"));
	}
}

static char * fourcc_str(uint32 type) {
	static char buf[7];
	buf[0] = '\'';
	buf[1] = type >> 24;
	buf[2] = (type >> 16) & 0xff;
	buf[3] = (type >> 8) & 0xff;
	buf[4] = type & 0xff;
	buf[5] = '\'';
	buf[6] = '\0';
	return buf;
}


static void byte_sum(const void * data, size_t data_len) {
	uint64 sum = 0;
	for (size_t i = 0; i < data_len; i++)
		sum += (uint64) ((uint8 *)data)[i];
	D(bug("byte_sum 0x%" PRIx64 "\n", sum));
}

const char * pict_loadable_types[] = {
	"image/bmp",
	NULL
};

void verify_same_pict() {
	// get mac clipboard
	void * mac_clipboard_pict;
	size_t mac_clipboard_pict_len;

	D(bug("Data corruption check: Verifying that the pict currently on the mac clipboard still matches what we put in\n"));

	mac_clipboard_pict = read_scrap(FOURCC('P','I','C','T'), &mac_clipboard_pict_len);
	if (mac_clipboard_pict) {
		// verify that it matches
		if (last_pict_put_in_clipboard.different_from(mac_clipboard_pict, mac_clipboard_pict_len)) {
			D(bug(" -> DATA IS DIFFERENT\n"));
		} else {
			D(bug(" -> the data is still ok (%zu bytes)\n", mac_clipboard_pict_len));
		}
		SDL_free(mac_clipboard_pict);
	} else {
		D(bug(" -> There was no PICT data on the clipboard!\n"));
	}
}

static char * get_host_clipboard_as_pict(size_t & data_len) {
	char * data = NULL;
	data_len = 0;
#if SDL_VERSION_ATLEAST(3, 2, 0)
	size_t num_mime_types;
	char ** mime_types = SDL_GetClipboardMimeTypes(&num_mime_types);

	if (!mime_types) return NULL;

	const char * load_mime_type = NULL;

	D(bug("Host clipboard mime types (%zu):\n", num_mime_types));
	for (int i = 0; i < num_mime_types; i++) {
		//D(bug(" %s\n", mime_types[i]));
		for (const char ** cur_type = pict_loadable_types; *cur_type != NULL; cur_type++) {
			if (strcmp(mime_types[i], *cur_type) == 0) {
				load_mime_type = *cur_type;
				break;
			}
		}
		if (load_mime_type != NULL) break;
	}

	if (load_mime_type == NULL) {
		D(bug("No mime type we can load as an image found\n"));
		return NULL;
	}

	D(bug("Loading %s data from clipboard\n", load_mime_type));

	size_t clipboard_data_size;
	void * clipboard_data = SDL_GetClipboardData(load_mime_type, &clipboard_data_size);
	// byte_sum(clipboard_data, clipboard_data_size);

	// let's leak a whole copy to see if it's still changing
	// void * new_clipboard = SDL_malloc(clipboard_data_size);
	// assert(new_clipboard);
	// memcpy(new_clipboard, clipboard_data, clipboard_data_size);
	// clipboard_data = new_clipboard;

	// D(bug("Loaded %s %zu bytes\n", load_mime_type, clipboard_data_size));

	if (clipboard_data == NULL) {
		D(bug("Couldn't load %s data from clipboard\n", load_mime_type));
		return NULL;
	}

	SDL_free(mime_types);

	// We check if the BMP is new here, but it seems that BMP data provided
	// as the result of a clipboard conversion changes from
	// one request to the next, sometimes but not all the time
	// So we also check RGBA below.
	// (Does the BMP data contain some kind of metadata timestamp?)

	if (!last_bmp_from_host.different_from(clipboard_data, clipboard_data_size)) {
		D(bug("The BMP data is already converted to the current mac clipboard contents, no need to do it again.\n"));
		verify_same_pict();
		SDL_free(clipboard_data);
		return NULL;
	}

	SDL_IOStream *src = SDL_IOFromConstMem(clipboard_data, clipboard_data_size);
	if (!src) {
		D(bug("Couldn't create SDL_IOStream: %s\n", SDL_GetError()));
		SDL_free(clipboard_data);
		return NULL;
	}
	SDL_Surface * s = SDL_LoadBMP_IO(src, false);

	SDL_CloseIO(src);

	// No free of clipboard_data
	last_bmp_from_host.update(clipboard_data, clipboard_data_size); // this now owns the allocated clipboard_data

	if (!s) {
		D(bug("Error loading BMP data: %s\n", SDL_GetError()));
		return NULL;
	}

	D(bug("BMP loaded surface pixel format is %s\n", SDL_GetPixelFormatName(s->format)));
	D(bug("w %d h %d\n", s->w, s->h));

	SDL_Surface * rgba_surface = SDL_ConvertSurface(s, SDL_PIXELFORMAT_ABGR8888);

	SDL_DestroySurface(s);

	if (!rgba_surface) {
		D(bug("Error converting SDL surface to RGBA: %s\n", SDL_GetError()));
		return NULL;
	}

	if (rgba_surface->w >= 65536 || rgba_surface->h >= 65536) {
		D(bug("Image is too big %dx%d for out PICT converter\n", rgba_surface->w, rgba_surface->h));
		SDL_DestroySurface(rgba_surface);
		return NULL;
	}

	uint16_t width, height;
	width = rgba_surface->w;
	height = rgba_surface->h;

	D(bug("loaded image is %hux%hu\n", width, height));

	size_t buf_size_align = 1-1;
	uint16_t width_align = 1-1;
	uint16_t height_align = 1-1;

	// let's round the image width up to even
	width = (width+width_align) & ~width_align;
	uint16_t buf_height = (height+height_align)&~height_align;
	D(bug("Using %dx%d\n", width, buf_height));

	size_t line_bytes = (size_t)width * 4;

	
	uint8_t * rgba_data = NULL;
	bool free_rgba_data = false;
	size_t rgba_data_size = line_bytes * buf_height;
	//size_t rgba_data_size = rgba_surface->pitch * buf_height;

	if (line_bytes != rgba_surface->pitch) {
		D(bug("RGBA surface pitch is %d and packed line bytes is %zu\n", rgba_surface->pitch, line_bytes));
		D(bug("Compacting buffer\n"));
		rgba_data = (uint8_t *) SDL_malloc(rgba_data_size);
		SDL_memset(rgba_data, 0xFF, rgba_data_size);
		if (rgba_data) {
			free_rgba_data = true;
			SDL_LockSurface(rgba_surface);
			size_t bytes_to_copy = std::min(line_bytes, (size_t)rgba_surface->pitch);
			for (uint16_t y = 0; y < height; y++) {
				SDL_memcpy(rgba_data + y * line_bytes, (uint8_t *)rgba_surface->pixels + y * rgba_surface->pitch, bytes_to_copy);
			}
			SDL_UnlockSurface(rgba_surface);
		} else {
			D(bug("Couldn't allocate separate rgba_data buffer\n"));
		}
	} else {
		SDL_LockSurface(rgba_surface);
		rgba_data = (uint8_t *) rgba_surface->pixels;
	}

	if (rgba_data) {
		// D(bug("rgba_data\n"));

		for (uint16_t y = 0; y < height; y++) {
			for (uint16_t x = 0; x < width; x++) {
				rgba_data[y * line_bytes + x * 4 + 3] = 0xff;
			}
		}

		// byte_sum(rgba_data, rgba_data_size);
		if (!last_rgba_from_host.different_from(rgba_data, rgba_data_size)) {
			D(bug("The RGBA is already converted to the current mac clipboard contents, no need to do it again.\n"));
			verify_same_pict();
		} else {

			void * rgba_data_copy = SDL_malloc(rgba_data_size);
			if (rgba_data_copy) {
				memcpy(rgba_data_copy, rgba_data, rgba_data_size);
				last_rgba_from_host.update(rgba_data_copy, rgba_data_size);
			}

			D(bug("Converting RGBA to PICT; 1: get size\n"));
			long bufSize = ConvertRGBAToPICT(NULL, 0, rgba_data, width, buf_height);

			if (bufSize > 0) {
				bufSize = (bufSize + buf_size_align) & ~buf_size_align;
				D(bug("Need %ld bytes\n", bufSize));
				uint8_t *buf = (uint8_t *)SDL_malloc(bufSize);

				if (buf == NULL) {
					D(bug("Couldn't create PICT buffer\n"));
				} else {
					SDL_memset(buf, 0, bufSize);
					D(bug("2: Loading PICT\n"));
					D(bug("\n"));
					long pictSize = ConvertRGBAToPICT(buf, bufSize, rgba_data, width, buf_height);
					D(bug("\n"));
					D(bug("pict size %ld\n", pictSize));
					data = (char *)buf;
					data_len = pictSize;
					//data_len = bufSize;
					data_len = (data_len + buf_size_align) & ~buf_size_align;
					D(bug("Loaded.\n"));

					void * pict_copy = SDL_malloc(data_len);
					if (pict_copy) {
						SDL_memcpy(pict_copy, data, data_len);
						last_pict_put_in_clipboard.update(pict_copy, data_len);

					} else {
						D(bug("Couldn't allocate storage for data corruption check pict\n"));
						last_pict_put_in_clipboard.free();
					}

				}
			} else {
				// Negative return value indicates error
				D(bug("Something went wrong with PICT conversion\n"));
			}
		}

		if (free_rgba_data) {
			SDL_free(rgba_data);
		} else {
			SDL_UnlockSurface(rgba_surface);
		}
	}

	if (rgba_surface) {
		SDL_DestroySurface(rgba_surface);
		rgba_surface = NULL;
	}

#endif
	return data;
}

void update_clipboard_from_host();

/*
 *  Mac application reads clipboard
 */

void GetScrap(void **handle, uint32 type, int32 offset)
{
	D(bug("GetScrap handle %p, type %08x (%s), offset %d\n", handle, type, fourcc_str(type), offset));

	if (we_are_getting_data) {
		we_are_getting_data = false;
		return;
	}

	if (offset == 0)
		update_clipboard_from_host();

	D(bug("done GetScrap()\n"));
	D(bug("\n"));

}

void update_clipboard_from_host() { 

	bool process_host_clipboard_as_text = true; // without specialized support all we can do is text

#if SDL_VERSION_ATLEAST(3, 2, 0)
	if (SDL_HasClipboardData("TIMESTAMP")) {
		size_t timestamp_data_size;
		void * timestamp_data = SDL_GetClipboardData("TIMESTAMP", &timestamp_data_size);
		if (timestamp_data && !last_timestamp_from_host.different_from(timestamp_data, timestamp_data_size)) {
			// We have already processed this host clipboard
			SDL_free(timestamp_data);
			return;
		}

		if (timestamp_data)
			last_timestamp_from_host.update(timestamp_data, timestamp_data_size); // now this owns the timestamp data
	} else {
		D(bug( " no timestamp data\n"));
	}

	process_host_clipboard_as_text = false;
	size_t num_mime_types;
	char ** mime_types = SDL_GetClipboardMimeTypes(&num_mime_types);
	if (mime_types) {
		D(bug("Host clipboard mime types (%zu)\n", num_mime_types));
		for (size_t i = 0; i < num_mime_types; i++) {
			//D(bug(" %s\n", mime_types[i]));
			if (strcmp(mime_types[i], "UTF8_STRING") == 0) {
				// UTF8_STRING seems to be the most widely used,
				// For instance with text from: xsel -b -i
				process_host_clipboard_as_text = true;
				break;
			}
		}
	}

	D(bug("  process_host_clipboard_as_text %d\n", process_host_clipboard_as_text));

	if (process_host_clipboard_as_text) {
		// any previous image is now invalid
		last_bmp_from_host.free();
		last_rgba_from_host.free();
	} else {
		// any previous text is now invalid
		last_text_from_host.free();
	}

	SDL_free(mime_types);
#endif	

	bool data_needs_sdl_free = false;
	char * data = NULL;
	size_t data_len = 0;
	bool convert_text = false;
	uint32 guest_type = 0;	

	if (process_host_clipboard_as_text) {
//	switch (guest_type) {
//		case FOURCC('T','E','X','T'):
//			if (!process_host_clipboard_as_text) break;
#if SDL_VERSION_ATLEAST(2, 0, 0)
			if (!SDL_HasClipboardText()) {
				last_text_from_host.free();
				return;
			}
			data = SDL_GetClipboardText();
			if (data == NULL) return;

			if (!last_clipboard_from_mac.different_from(data)) {
				// We want the emulated mac to get its own clipboard contents
				// to avoid format conversions

				// We're just bolted onto the side of the mac application reading
				// the mac clipboard here; all we need to do is get out of the way;
				D(bug("  This is clipboard from mac; leave as-is to use the mac's copy\n"));
				return;
			}

			if (!last_text_from_host.different_from(data)) {
				// We've already processed this data an it is on the current
				// mac clipboard
				SDL_free(data);
				return;
			}

			D(bug(" clipping TEXT\n"));

			last_text_from_host.update(data); // This now owns the allocation of data

			data_len = strlen(data);
			D(bug("Got text from clipboard: (len %zu)\n", data_len));
			hexdump(data, data_len);
			convert_text = true;
			guest_type = FOURCC('T','E','X','T');
#endif
	} else {
//			break;
//		case FOURCC('P','I','C','T'): {
			data = get_host_clipboard_as_pict(data_len);
			if (data) {
				guest_type = FOURCC('P','I','C','T');
				data_needs_sdl_free = true;
			}
			//break;
//		}
	}

	if (data == NULL) {
		D(bug(" no replacement data, leaving guest mac clipboard as is\n"));
		return;
	}

	char * converted = data;
	size_t converted_len = data_len;
	if (convert_text) {
		// Convert text from host charset to Mac charset
		char * try_converted = CONVERT_HOST_TO_MAC_STRING(data, data_len);
		if (try_converted) {
			converted = try_converted;
			converted_len = strlen(converted);
			D(bug("Converted text: (len %zu)\n", converted_len));
			hexdump(converted, converted_len);
		} else {
			D(bug("Conversion failed\n"));
		}
		for (char * p = converted; *p != '\0'; p++)
			if (*p == 10) *p = 13;
	}

	// Invalidate mac clipboard since we're replacing it
	last_clipboard_from_mac.free();
	if (guest_type != FOURCC('P','I','C','T')) {
		//guest_type = FOURCC('N', 'O', 'P', 'E');
		/*
		if (convert_text && converted != data) {
			SDL_free(converted);
			converted = (char *) SDL_malloc(converted_len);
		}
		*/
	}
	if (zero_scrap()) {
		write_scrap(guest_type, converted, converted_len);

		// // as a test let's read it back
		// size_t scrap_size;
		// void * out = read_scrap(guest_type, &scrap_size);
		// if (out) {
		// 	D(bug("The scrap was:\n"));
		// 	hexdump((char *) out, scrap_size);
		//  SDL_free(out);
		// } else {
		// 	D(bug("Couldn't read scrap we just wrote\n"));
		// }
	}

	if (convert_text && converted != data) {
		SDL_free(converted);
	}

	if (data_needs_sdl_free) {
		SDL_free(data);
	}

}

/*
 * ZeroScrap() is called before a Mac application writes to the clipboard; clears out the previous contents
 */

void ZeroScrap()
{
	// in B2 there is no patch for this and it never runs
#ifdef SHEEPSHAVER
	D(bug("ZeroScrap\n"));

	if (we_zeroed) {
		D(bug("  ZeroScrap for our update\n"));
		we_zeroed = false;
	}

	// setting the mac clipboard invalidates any related state
	ClearHostClipboardChecks();
	last_timestamp_from_host.free();
#else
	assert(false);
#endif
}

/*
 *  Mac application wrote to clipboard
 */

void PutScrap(uint32 type, void *scrap, int32 length)
{
	if (length <= 0) {
		D(bug("PutScrap empty length %d\n", length));
		return;
	}

	if (we_put_this_data) {
		D(bug("  PutScrap for our update\n"));
		we_put_this_data = false;
		return;
	}

	D(bug("PutScrap type %08" PRIx32 " (%s), data %p, length %" PRId32 "\n", type, fourcc_str(type), scrap, length));

	// setting the mac clipboard invalidates any related state
	ClearHostClipboardChecks();

	switch (type) {
	case FOURCC('T','E','X','T'): {
		D(bug(" clipping TEXT\n"));

		char data[length+1];
		memcpy(data, scrap, length);
		data[length] = '\0';

		D(bug("mac clipboard:\n"));
		hexdump(data, length);

		for (char * p = data; *p != '\0'; p++)
			if (*p == 13) *p = 10;

		// Convert text from Mac charset to host charset
		char * converted = CONVERT_MAC_TO_HOST_STRING(data, length); // returns allocated string if successful
		if (converted) {
			SDL_SetClipboardText(converted);
			D(bug("Setting host clipboard text: %s\n", converted));
			hexdump(converted, SDL_strlen(converted));
		} else {
			D(bug("Conversion failed\n"));
			D(bug("Setting host clipboard text: %s\n", data));
			SDL_SetClipboardText(data);
			converted = SDL_strdup(data); // make an allocated string copy
		}
		last_clipboard_from_mac.update(converted); // update tracking owns the allocated string now
		return;
		break;
	}
	}

}
