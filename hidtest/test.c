/*******************************************************
 Windows HID simplification

 Alan Ott
 Signal 11 Software

 8/22/2009

 Copyright 2009

 This contents of this file may be used by anyone
 for any reason without any conditions and may be
 used as a starting point for your own applications
 which use HIDAPI.
********************************************************/

#include <stdio.h>
#include <wchar.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "hidapi.h"
#include "mtwister.h"

MTRand mtRand;
int mtRandInit;

static void initrand() {
  if (!mtRandInit) {
    mtRand = seedRand(time(0));
    mtRandInit = 1;
  }
}

static unsigned long irand(unsigned long hi) {
  initrand();
  return genRandLong(&mtRand) % hi;
}

static double urand() {
  initrand();
  return genRand(&mtRand);
}

// Headers needed for sleeping.
#ifdef _WIN32
	#include <windows.h>
#else
	#include <unistd.h>
#endif

typedef unsigned char u8;
typedef unsigned short u16;

static char hdata[512] = {
	0x10, 0x0f, 0x00, 0x58, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3f,
	0x01, 0xef, 0x00, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
	0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23,
	0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b,
	0x3c, 0x3d, 0x3e, 0x3f, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
	0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53,
	0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
	0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b,
	0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
	0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 0x80, 0x81, 0x82, 0x83,
	0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
	0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b,
	0x9c, 0x9d, 0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
	0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xb0, 0xb1, 0xb2, 0xb3,
	0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
	0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb,
	0xcc, 0xcd, 0xce, 0xcf, 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
	0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf, 0xe0, 0xe1, 0xe2, 0xe3,
	0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
	0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb,
	0xfc, 0xfd, 0xfe, 0xff, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
	0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13,
	0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b,
	0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40, 0x41, 0x42, 0x43,
	0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b,
	0x5c, 0x5d, 0x5e, 0x5f, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
	0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0x73,
	0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
	0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b,
	0x8c, 0x8d, 0x8e, 0x8f, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
	0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3,
	0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
	0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb,
	0xbc, 0xbd, 0xbe, 0xbf, 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
	0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 0xd0, 0xd1, 0xd2, 0xd3,
	0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
	0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb,
	0xec, 0xed, 0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
	0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
};

typedef float f32;

#define FPS 40
struct gfb_data {
  hid_device *handle;

  u8* fb_bitmap;
  u8* fb_vbitmap;
  f32* fb_grey[FPS];
  f32* fb_frame;

  int fb_grey_cursor;
  int fb_grey_frames;

  int fb_bitmap_size;
  int fb_vbitmap_size;

  int xres;
  int yres;
  int bpp;
  int line_length;
  f32 mouse_x;
  f32 mouse_y;
  int key0;
};

static void init_fb(struct gfb_data *data)
{
  int i;

  data->bpp = 1;
  data->yres = 43;
  data->xres = 160;
  data->line_length = 32; /* = xres*bpp/8 + 12 bytes padding */
  

  /*
   * The native monochrome format uses vertical bits. Therefore
   * the number of bytes needed to represent the first column is
   * 43/8 (rows/bits) rounded up.
   * Additionally, the format requires a padding of 32 bits in
   * front of theimage data.
   *
   * Therefore the vbitmap size must be:
   *   160 * ceil(43/8) + 32 = 160 * 6 + 32 = 992
   */
  data->fb_vbitmap_size = 992; /* = 32 + ceil(yres/8) * xres */
  data->fb_vbitmap = calloc(1, data->fb_vbitmap_size);

  data->fb_bitmap_size = data->line_length * data->yres;
  data->fb_bitmap = calloc(1, data->fb_bitmap_size);

  data->fb_grey_cursor = 0;
  for (i=0; i<FPS; i++) {
    data->fb_grey[i] = (f32*)calloc(sizeof(f32), data->yres * data->xres);
  }
  data->fb_frame = (f32*)calloc(sizeof(f32), data->yres * data->xres);
}

static struct gfb_data fb;

static int get_cursor(int i) {
  i += fb.fb_grey_cursor;
  while (i < 0) {
    i += FPS;
  }
  i %= FPS;
  return i;
}

double lerp(double a, double b, double t) {
  return (b - a) * t + a;
}

double between(double a, double b, double from, double upto, double pos) {
  double t = (pos - from) / (upto - from);
  return lerp(a, b, t);
}

double lerpx(double a, double b) {
  return between(a, b, -1, 1, fb.mouse_x);
}

double lerpy(double a, double b) {
  return between(a, b, -1, 1, fb.mouse_y);
}

static int get_grey(int x, int y) {
  struct gfb_data* data = &fb;
  int i;
  int pos = y*data->xres + x;
  f32 v = 0.0;
  f32 dice = urand();
  assert(x>=0 && x<data->xres);
  assert(y>=0 && y<data->yres);
  int n = 4;
  for (i=0; i<n; i++) {
    v += data->fb_grey[get_cursor(-i)][pos];
  }
  v /= n;
  f32 avg = (1 - v);
  //f32 chance = data->fb_frame[pos];
  f32 gamma = 1.0 / 2.2;
  if (fb.mouse_x >= 0) {
    gamma = lerp(1.0, 1.0 / 2.2, fb.mouse_x);
  } else {
    gamma = lerp(1.0, 2.2 / 1.0, -fb.mouse_x);
  }
  f32 p = data->fb_frame[pos];
  int on = (dice < 1.0 - pow(1.0 - p, gamma));
  //int on = avg > data->fb_frame[pos];
  data->fb_grey[data->fb_grey_cursor][pos] = on;
  return on;
}

static void set_pixel(int x, int y, double r, double g, double b) {
  struct gfb_data* data = &fb;
  int pos = y*data->xres + x;
  assert(x>=0 && x<data->xres);
  assert(y>=0 && y<data->yres);
  data->fb_frame[pos] = (r + g + b) / 3.0;
}

/* Update fb_vbitmap from the screen_base and send to the device */
static void gfb_fb_grey_update(struct gfb_data *data)
{
	int xres, yres;
	int col, row;
	u16 *src, *dst;

	/* Set the image message header */
	memcpy(data->fb_vbitmap, &hdata, sizeof(hdata));

	/* LCD is a portrait mode one so we have to rotate the framebuffer */

	src = (u16 *)data->fb_grey;
	dst = (u16 *)data->fb_bitmap;

	xres = data->xres;
	yres = data->yres;
	for (col = 0; col < xres; ++col)
		for (row = 0; row < yres; ++row)
			*dst++ = src[row * xres + col];
}

/* Update fb_vbitmap from the screen_base and send to the device */
static void gfb_fb_qvga_update(struct gfb_data *data)
{
	int xres, yres;
	int col, row;
	u16 *src, *dst;

	/* Set the image message header */
	memcpy(data->fb_vbitmap, &hdata, sizeof(hdata));

	/* LCD is a portrait mode one so we have to rotate the framebuffer */

	src = (u16 *)data->fb_bitmap;
	dst = (u16 *)(data->fb_vbitmap + sizeof(hdata));

	xres = data->xres;
	yres = data->yres;
	for (col = 0; col < xres; ++col)
		for (row = 0; row < yres; ++row)
			*dst++ = src[row * xres + col];
}

static void gfb_fb_mono_update(struct gfb_data *data)
{
	int xres, yres, ll;
	int band, bands, col, bit;
	u8 *dst, *src, *row_start;
	u8 mask;

	/* Clear the vbitmap (we only flip bits to 1 later on) */
	memset(data->fb_vbitmap, 0x00, data->fb_vbitmap_size);

	/* Set the magic number */
	data->fb_vbitmap[0] = 0x03;

	/*
	 * Translate the XBM format screen_base into the format needed by the
	 * G15. This format places the pixels in a vertical rather than
	 * horizontal format. Assuming a grid with 0,0 in the upper left corner
	 * and 159,42 in the lower right corner, the first byte contains the
	 * pixels 0,0 through 0,7 and the second byte contains the pixels 1,0
	 * through 1,7. Within the byte, bit 0 represents 0,0; bit 1 0,1; etc.
	 *
	 * The offset is adjusted by 32 within the image message.
	 */

	xres = data->xres;
	yres = data->yres;
	ll = data->line_length;

	dst = data->fb_vbitmap + 32;

	bands = (yres + 7) / 8; /* poor man's ceil(yres/8) */
	for (band = 0; band < bands ; ++band) {
		/* each band is 8 pixels vertically */
		row_start = data->fb_bitmap + band * 8 * ll;
		for (col = 0; col < xres; ++col) {
			src = row_start + col / 8;
			mask = 0x01 << (col % 8);
			for (bit = 0 ; bit < 8 ; ++bit) {
				if (*src & mask)
					*dst |= (0x01 << bit);
				src += ll;
			}
			++dst;
		}
	}
}

static int mapping(struct gfb_data* data, int x, int y) {
  static int map[43][160];
  static int init = 0;
  if (init == 0) {
    int row;
    int X;
    int Y;
    int off;
    int pos = 0;
    for (row=0; row<data->yres; row+=8) {
      for (X=0; X<data->xres; X++) {
        for (off=0; off<8; off++) {
          int Y = row + off;
          if (Y < data->yres && X < data->xres) {
            map[Y][X] = 8*pos + off;
          }
        }
        pos += 1;
      }
    }
    init = 1;
  }
  assert(x>=0 && x<160);
  assert(y>=0 && y<43);
  return map[y][x];
}

static int byte(struct gfb_data* data, int x, int y) {
  int r;
  assert(x>=0 && x<data->xres);
  assert(y>=0 && y<data->yres);
  //r = mapping(data, x, y);
  r = (8*y*data->line_length + x)/8;
  //printf("byt(x=%d, y=%d) == %d\n", x, y, r);
  return r;
}

static int bit(struct gfb_data* data, int x, int y) {
  int r;
  assert(x>=0 && x<data->xres);
  assert(y>=0 && y<data->yres);
  //r = mapping(data, x, y) % 8;
  r = (y*data->line_length + x) % 8;
  //printf("bit(x=%d, y=%d) == %d\n", x, y, r);
  return r;
}

static void lcd_pixel(int x, int y, int on) {
  struct gfb_data* data = &fb;
  u8* dst = data->fb_bitmap;
  if (on) {
    dst[byte(data,x,y)] |=  (1 << bit(data,x,y));
  } else {
    dst[byte(data,x,y)] &= ~(1 << bit(data,x,y));
  }
}

static void fb_write(struct gfb_data* data) {
  int res;
  int size;
  u8* buf;

  buf = data->fb_vbitmap;
  size = data->fb_vbitmap_size;

  res = hid_write(data->handle, buf, size);
  if (res < 0)
    printf("Unable to write() (2)\n");
  else if (res < size) {
    printf("Unable to write all the bytes (%d < %d)\n", res, size);
  }
  else {
    //printf("Wrote %d bytes\n", res);
  }
}

static void fb_draw(struct gfb_data* data) {
  u8* dst;
  int y;
  int x;
  int h;
  int w;
  int x0 = 5;
  int x1 = 15;
  int y0 = 20;
  int y1 = 40;
  u8 i;

  dst = data->fb_bitmap;
  w = data->line_length;
  h = data->yres;
  i = 0;

#if 0
  for (x=0; x<h; x++) {
    for (y=0; y<w; y++) {
      //dst[y*h+x] = i ? 255 : 0;
      dst[y*h+x] = irand(256) ^ (irand(256) >> 2);
      //i = (i + 1) % 2;
      //i = irand() % 2;
    }
  }
  for (y=y0; y<y1; y++) {
    for (x=x0; x<x1; x++) {
      dst[byte(data,x,y)] &= ~(1 << bit(data,x,y));
      //dst[byte(data,x,y)] = 0;
      //dst[byte(data,x,y)] = 0;
    }
  }
  for (y=y0; y<y1; y++) {
    for (x=x0; x<x1; x++) {
      if (irand(10) == 0) {
        dst[byte(data,x,y)] |=  (1 << bit(data,x,y));
      }
      //dst[byte(data,x,y)] = 1;
    }
  }
#endif
  f32 p;
  for (x=0; x<data->xres; x++) {
    for (y=0; y<data->yres; y++) {
      p = (f32)x / (f32)data->xres;
      //set_pixel(x, y, 0.0, 0.0, 0.0);
      set_pixel(x, y, p, p, p);
    }
  }

  for (x=0; x<data->xres; x++) {
    for (y=data->yres/2; y<data->yres; y++) {
      p = 1.0 - (f32)x / (f32)data->xres;
      //set_pixel(x, y, 0.0, 0.0, 0.0);
      set_pixel(x, y, p, p, p);
    }
  }

#if 0
  for (y=y0; y<y1; y++) {
    for (x=x0; x<x1; x++) {
      set_pixel(x, y, 0.5, 0.5, 0.5);
    }
  }

  x0 = 50+5; y0 = 20;
  x1 = 50+15; y1 = 40;
  p = 0.25;

  for (y=y0; y<y1; y++) {
    for (x=x0; x<x1; x++) {
      set_pixel(x, y, p, p, p);
    }
  }
#endif

  for (x=0; x<data->xres; x++) {
    for (y=0; y<data->yres; y++) {
      int on = get_grey(x,y);
      lcd_pixel(x, y, on);
    }
  }

  int xr = data->xres;
  int yr = data->yres;
  for (x=0; x<xr; x++) {
    for (y=0; y<yr; y++) {
      if (x < xr/2-10) continue;
      if (x > xr/2+10) continue;
      if (y < yr/2-10) continue;
      if (y > yr/2+10) continue;
      //lcd_pixel(x, y, urand() > 0.5);
      int on = (x + (y % 2)) % 2;
      if (fb.key0) {
        on = 1;
      }
      if (on) {
        lcd_pixel(x, y, on);
      }
      //dst[y*h+x] = i ? 255 : 0;
      //dst[y*h+x] = irand(256) ^ (irand(256) >> 2);
      //i = (i + 1) % 2;
      //i = irand() % 2;
    }
  }

  gfb_fb_mono_update(data);
  fb_write(data);
  data->fb_grey_cursor++;
  data->fb_grey_cursor %= FPS;
}

int main(int argc, char* argv[])
{
	(void)argc;
	(void)argv;

	int res;
	unsigned char buf[512];
	#define MAX_STR 255
	wchar_t wstr[MAX_STR];
	hid_device *handle;
	int j;
	int i;
	int any;

	struct hid_device_info *devs, *cur_dev;

        srand(time(0));
        init_fb(&fb);
        gfb_fb_mono_update(&fb);

	printf("hidapi test/example tool. Compiled with hidapi version %s, runtime version %s.\n", HID_API_VERSION_STR, hid_version_str());
	if (hid_version()->major == HID_API_VERSION_MAJOR && hid_version()->minor == HID_API_VERSION_MINOR && hid_version()->patch == HID_API_VERSION_PATCH) {
		printf("Compile-time version matches runtime version of hidapi.\n\n");
	}
	else {
		printf("Compile-time version is different than runtime version of hidapi.\n]n");
	}

	if (hid_init())
		return -1;

	devs = hid_enumerate(0x0, 0x0);
	cur_dev = devs;
	while (cur_dev) {
		printf("Device Found\n  type: %04hx %04hx\n  path: %s\n  serial_number: %ls", cur_dev->vendor_id, cur_dev->product_id, cur_dev->path, cur_dev->serial_number);
		printf("\n");
		printf("  Manufacturer: %ls\n", cur_dev->manufacturer_string);
		printf("  Product:      %ls\n", cur_dev->product_string);
		printf("  Release:      %hx\n", cur_dev->release_number);
		printf("  Interface:    %d\n",  cur_dev->interface_number);
		printf("  Usage (page): 0x%hx (0x%hx)\n", cur_dev->usage, cur_dev->usage_page);
		printf("\n");
		cur_dev = cur_dev->next;
	}
	hid_free_enumeration(devs);

	// Set up the command buffer.
	memset(buf,0x00,sizeof(buf));
	buf[0] = 0x01;
	buf[1] = 0x81;


	// Open the device using the VID, PID,
	// and optionally the Serial number.
	////handle = hid_open(0x4d8, 0x3f, L"12345");
	fb.handle = handle = hid_open(0x046d, 0xc21c, NULL);
	if (!handle) {
		printf("unable to open device\n");
 		return 1;
	}

	// Read the Manufacturer String
	wstr[0] = 0x0000;
	res = hid_get_manufacturer_string(handle, wstr, MAX_STR);
	if (res < 0)
		printf("Unable to read manufacturer string\n");
	printf("Manufacturer String: %ls\n", wstr);

	// Read the Product String
	wstr[0] = 0x0000;
	res = hid_get_product_string(handle, wstr, MAX_STR);
	if (res < 0)
		printf("Unable to read product string\n");
	printf("Product String: %ls\n", wstr);

	// Read the Serial Number String
	wstr[0] = 0x0000;
	res = hid_get_serial_number_string(handle, wstr, MAX_STR);
	if (res < 0)
		printf("Unable to read serial number string\n");
	printf("Serial Number String: (%d) %ls", wstr[0], wstr);
	printf("\n");

	// Read Indexed String 1
	wstr[0] = 0x0000;
	res = hid_get_indexed_string(handle, 1, wstr, MAX_STR);
	if (res < 0)
		printf("Unable to read indexed string 1\n");
	printf("Indexed String 1: %ls\n", wstr);

	// Set the hid_read() function to be non-blocking.
	hid_set_nonblocking(handle, 1);

	// Try to read from the device. There should be no
	// data here, but execution should not block.
	res = hid_read(handle, buf, 17);
        printf("Init data...\n");
        for (i = 0; i < res; i++)
                printf("%02hhx ", buf[i]);
        

#if 0
	// Send a Feature Report to the device
	buf[0] = 0x7;
	buf[1] = 0x6e;
	buf[2] = 0xff;
	buf[3] = 0x5a;
	buf[4] = 0x00;
	//res = hid_send_feature_report(handle, buf, 17);

	buf[0] = 0x2;
	buf[1] = 0x00;
	buf[2] = 0x00;
	buf[3] = 0x00;
	buf[4] = 0x00;
	res = hid_send_feature_report(handle, buf, 17);
	if (res < 0) {
		printf("Unable to send a feature report.\n");
	} else {
          printf("Sent %d bytes\n", res);
        }
#endif

#if 0
	buf[0] = 0x02;
	buf[1] = 0x80;
	buf[2] = 0xFF;
	buf[3] = 0xFF;
	buf[4] = 0xFF;
	res = hid_send_feature_report(handle, buf, 17);
	if (res < 0) {
		printf("Unable to send a feature report.\n");
	} else {
          printf("Sent %d bytes\n", res);
        }
#endif

#if 0
	// Request state (cmd 0x81). The first byte is the report number (0x1).
	buf[0] = 0x7;
	buf[1] = 0xff;
	buf[2] = 0xff;
	buf[3] = 0x0a;
	buf[4] = 0x02;
	res = hid_write(handle, buf, 17);
	if (res < 0)
		printf("Unable to write() (2)\n");
        else
		printf("Wrote %d bytes\n", res);
#endif
        fb_draw(&fb);

        any=1;
        while (any) {
          any=0;
          for (j=0; j<=7; j++) {
            memset(buf,0,sizeof(buf));
            // Read a Feature Report from the device
            buf[0] = j;
            res = hid_get_feature_report(handle, buf, sizeof(buf));
            if (res < 0) {
                    printf("Unable to get feature report %d.\n", j);
                    printf("%ls\n", hid_error(handle));
            }
            else {
                    // Print out the returned buffer.
                    printf("Feature Report (%d), %d bytes:\n   ", j, res);
                    for (i = 0; i < res; i++)
                            printf("%02hhx ", buf[i]);
                    printf("\n");
                    any=1;
            }
          }
          break;
        }

        any=1;
        while (any) {
          any=0;
          for (j=1; j<=7; j++) {
            memset(buf,0,sizeof(buf));
            // Read a Input Report from the device
            buf[0] = j;
            res = hid_get_input_report(handle, buf, sizeof(buf));
            if (res < 0) {
                    //printf("Unable to get input report %d.\n", j);
                    //printf("%ls\n", hid_error(handle));
            }
            else {
                    // Print out the returned buffer.
                    printf("Input Report (%d), %d bytes:\n   ", j, res);
                    for (i = 0; i < res; i++)
                            printf("%02hhx ", buf[i]);
                    printf("\n");
                    any=1;
            }
          }
          break;
        }

	memset(buf,0,sizeof(buf));

#if 0
	// Toggle LED (cmd 0x80). The first byte is the report number (0x1).
	buf[0] = 0x1;
	buf[1] = 0x80;
	res = hid_write(handle, buf, 17);
	if (res < 0) {
		printf("Unable to write()\n");
		printf("Error: %ls\n", hid_error(handle));
	}


	// Request state (cmd 0x81). The first byte is the report number (0x1).
	buf[0] = 0x1;
	buf[1] = 0x81;
	res = hid_write(handle, buf, 17);
	if (res < 0)
		printf("Unable to write() (2)\n");

#endif

	// Read requested state. hid_read() has been set to be
	// non-blocking by the call to hid_set_nonblocking() above.
	// This loop demonstrates the non-blocking nature of hid_read().
        {
          int fps;
          int now;

          fps = 0;
          now = time(0);
          memset(buf,0x00,sizeof(buf));
          while (1) {
            fps++;
            res = hid_read(handle, buf, sizeof(buf));
            while (res > 0) {
#if 0
              if (res != 8) {
                printf("Read %d bytes:\n   ", res);
              }
              // Print out the returned buffer.
              for (i = 0; i < res; i++)
                printf("%02hhx ", buf[i]);
              printf("\n");
#endif
              fb.mouse_x = (buf[1] / 255.0) * 2.0 - 1.0;
              fb.mouse_y = (buf[2] / 255.0) * 2.0 - 1.0;
              fb.mouse_y = - fb.mouse_y;
              res = hid_read(handle, buf, sizeof(buf));
            }
            if (res < 0)
              printf("Unable to read()\n");
            fb.key0 = (buf[7] & 2);
            fb_draw(&fb);
            //printf("waiting...\n");
#ifdef WIN32
            //Sleep(1);
#else
            //usleep(1*1000);
#endif
            if (time(0) != now) {
              printf("fps: %d mouse_x: %.2f mouse_y: %.2f\n", fps, fb.mouse_x, fb.mouse_y);
              fps = 0;
              now = time(0);
            }
          }
        }

	hid_close(handle);

	/* Free static HIDAPI objects. */
	hid_exit();

#ifdef WIN32
	system("pause");
#endif

	return 0;
}

#include "mtwister.c"
