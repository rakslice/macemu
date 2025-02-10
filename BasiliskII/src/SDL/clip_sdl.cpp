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

#define DEBUG 0
#include "debug.h"

#include <SDL.h>

class TextCache {
public:
	TextCache() : prev_val(NULL) { }

	bool same_as(const char * new_val) {
		bool out;
		if (new_val != NULL) {
			if (prev_val != NULL) {
				out = strcmp(new_val, prev_val) != 0;
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
	void update(char * new_val) {
		// update last_primary_selection_text
		if (prev_val) {
			SDL_free(prev_val);
		}
		prev_val = new_val;
	}

	void free() {
		update(NULL);
	}

protected:
	char * prev_val;
};


// Flag indicating the emulated mac PutScrap currently in progress is from our code in GetScrap(), and PutScrap() should
// not treat it as a possible update from an application on the emulated mac
static bool we_put_this_data = false;

// When something is copied on the emulated mac, we don't want to round trip it through the
// host clipboard if it is pasted again in the emulated mac, incurring unnecessary lossy format conversions.

// We just assume that if the clipboard text content is the same we last got from the mac,
// we should let its native clipboard win

TextCache last_clipboard_from_mac;

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
	SDL_iconv_close(from);
	SDL_iconv_close(to);
}

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

static void write_scrap(uint32 type, void * data, size_t data_len)
{
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
			0xa9, 0xfc,					// ZeroScrap()
			0x2f, 0x3c, 0, 0, 0, 0,		// move.l	#length,-(sp)
			0x2f, 0x3c, 0, 0, 0, 0,		// move.l	#type,-(sp)
			0x2f, 0x3c, 0, 0, 0, 0,		// move.l	#outbuf,-(sp)
			0xa9, 0xfe,					// PutScrap()
			0x58, 0x8f,					// addq.l	#4,sp
			M68K_RTS >> 8, M68K_RTS & 0xff
		};
		r.d[0] = sizeof(proc);
		Execute68kTrap(0xa71e, &r);		// NewPtrSysClear()
		uint32 proc_area = r.a[0];

		D(bug("Putting clipboard data\n"));

		// The procedure is run-time generated because it must lays in
		// Mac address space. This is mandatory for "33-bit" address
		// space optimization on 64-bit platforms because the static
		// proc[] array is not remapped
		Host2Mac_memcpy(proc_area, proc, sizeof(proc));
		WriteMacInt32(proc_area +  6, data_len);
		WriteMacInt32(proc_area + 12, type);
		WriteMacInt32(proc_area + 18, scrap_area);
		we_put_this_data = true;
		Execute68k(proc_area, &r);

		// We are done with scratch memory
		r.a[0] = proc_area;
		Execute68kTrap(0xa01f, &r);		// DisposePtr
		r.a[0] = scrap_area;
		Execute68kTrap(0xa01f, &r);		// DisposePtr
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

/*
 *  Mac application reads clipboard
 */

void GetScrap(void **handle, uint32 type, int32 offset)
{
	D(bug("GetScrap handle %p, type %08x (%s), offset %d\n", handle, type, fourcc_str(type), offset));

	bool data_needs_sdl_free = false;
	char * data = NULL;
	size_t data_len = 0;
	bool convert_text = false;

	switch (type) {
		case FOURCC('T','E','X','T'):
			D(bug(" clipping TEXT\n"));
#if SDL_VERSION_ATLEAST(2, 0, 0)
				if (!SDL_HasClipboardText())
					return;
				data = SDL_GetClipboardText();
				if (data == NULL) return;

				if (!last_clipboard_from_mac.same_as(data)) {
					// We want the emulated mac to get its own clipboard contents
					// to avoid format conversions

					// We're just bolted onto the side here; all we need to do is
					// get out of the way;
					return;
				}

				data_len = strlen(data);
				D(bug("Got text from clipboard: (len %d)\n", data_len));
				hexdump(data, data_len);
				data_needs_sdl_free = true;
				convert_text = true;
#endif
			break;
	}

	if (data == NULL) return;

	char * converted = data;
	size_t converted_len = data_len;
	if (convert_text) {
		char * try_converted = CONVERT_HOST_TO_MAC_STRING(data, data_len);
		if (try_converted) {
			converted = try_converted;
			converted_len = strlen(converted);
			D(bug("Converted text: (len %d)\n", converted_len));
			hexdump(converted, converted_len);
		} else {
			D(bug("Conversion failed\n"));
		}
		for (char * p = converted; *p != '\0'; p++)
			if (*p == 10) *p = 13;
	}

	switch (type) {
	case FOURCC('T','E','X','T'):
		// Convert text from UTF-8 to Mac charset
		write_scrap(type, converted, converted_len);
		break;
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
	D(bug("ZeroScrap\n"));
	we_put_this_data = false;
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

	D(bug("PutScrap type %08lx (%s), data %08lx, length %ld\n", type, fourcc_str(type), scrap, length));

	switch (type) {
	case FOURCC('T','E','X','T'): {
		D(bug(" clipping TEXT\n"));

		char data[length+1];
		memcpy(data, scrap, length);
		data[length] = '\0';
		for (char * p = data; *p != '\0'; p++)
			if (*p == 13) *p = 10;

		// Convert text from Mac charset to ISO-Latin1
		char * converted = CONVERT_MAC_TO_HOST_STRING(data, length);
		if (converted) {
			SDL_SetClipboardText(converted);
			D(bug("Setting host clipboard text: %s\n", converted));
			// it will own the string
			last_clipboard_from_mac.update(converted);
		} else {
			D(bug("Conversion failed\n"));
			D(bug("Setting host clipboard text: %s\n", data));
			SDL_SetClipboardText(data);
			last_clipboard_from_mac.update(SDL_strdup(data));
		}
		break;
	}
	}

}
