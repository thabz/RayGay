
#include <cmath>
#include <memory.h>
#include <string>
#include <cstdio>

#include "image/imageio_hdri.h"
#include "exception.h"
#include "image/image.h"


typedef unsigned char RGBE[4];
#define R			0
#define G			1
#define B			2
#define E			3

#define  MINELEN	8	// minimum scanline length for encoding
#define  MAXELEN	0x7fff	// maximum scanline length for encoding

static void workOnRGBE(RGBE *scan, int len, float *cols);
static bool decrunch(RGBE *scanline, int len, FILE *file);
static bool oldDecrunch(RGBE *scanline, int len, FILE *file);

void HdriIO::save(const Image* const image, const std::string& filename) const {
    throw_exception("HDRI saving not implemented.");
}

Image* HdriIO::load(const std::string& fileName)
{
    int i;
    char str[200];
    FILE *file;

    file = fopen(fileName.c_str(), "rb");
    if (!file)
	throw_exception("HDRI file named " + fileName + " not found.");

    fread(str, 10, 1, file);
    if (memcmp(str, "#?RADIANCE", 10)) {
	fclose(file);
	throw_exception(fileName + " is not a HDRI file.");
    }

    fseek(file, 1, SEEK_CUR);

    char cmd[2000];
    i = 0;
    char c = 0, oldc;
    while(i < 2000) {
	oldc = c;
	c = fgetc(file);
	if (c == 0xa && oldc == 0xa)
	    break;
	cmd[i++] = c;
    }

    char reso[2000];
    i = 0;
    while(i < 2000) {
	c = fgetc(file);
	reso[i++] = c;
	if (c == 0xa)
	    break;
    }

    long w, h;
    if (!sscanf(reso, "-Y %ld +X %ld", &h, &w)) {
	fclose(file);
	throw_exception("Couldn't read resolution of HDRI file " + fileName);
    }

    Image* result = new Image(w,h);

    float *cols = new float[w * h * 3];

    RGBE *scanline = new RGBE[w];
    if (!scanline) {
	fclose(file);
	throw_exception("HDRI file " + fileName + " contains no data.");
    }

    // convert image 
    for (int y = h - 1; y >= 0; y--) {
	if (decrunch(scanline, w, file) == false)
	    break;
	workOnRGBE(scanline, w, cols);
	cols += w * 3;
    }

    delete [] scanline;
    fclose(file);

    // Copy cols to image
    for(int y = 0; y < h; y++) {
	for(int x = 0; x < w; x++) {
	    RGBA col = RGBA(cols[0+y*w+x], cols[1+y*w+x], cols[2+y*w+x], 1);
	    result->setRGBA(x,y,col);
	}
    }

    delete [] cols;
    return result;
}

float convertComponent(int expo, int val)
{
    float v = val / 256.0f;
    float d = (float) pow(2, expo);
    return v * d;
}

void workOnRGBE(RGBE *scan, int len, float *cols)
{
    while (len-- > 0) {
	int expo = scan[0][E] - 128;
	cols[0] = convertComponent(expo, scan[0][R]);
	cols[1] = convertComponent(expo, scan[0][G]);
	cols[2] = convertComponent(expo, scan[0][B]);
	cols += 3;
	scan++;
    }
}

bool decrunch(RGBE *scanline, int len, FILE *file)
{
    int  i, j;

    if (len < MINELEN || len > MAXELEN)
	return oldDecrunch(scanline, len, file);

    i = fgetc(file);
    if (i != 2) {
	fseek(file, -1, SEEK_CUR);
	return oldDecrunch(scanline, len, file);
    }

    scanline[0][G] = fgetc(file);
    scanline[0][B] = fgetc(file);
    i = fgetc(file);

    if (scanline[0][G] != 2 || scanline[0][B] & 128) {
	scanline[0][R] = 2;
	scanline[0][E] = i;
	return oldDecrunch(scanline + 1, len - 1, file);
    }

    // read each component
    for (i = 0; i < 4; i++) {
	for (j = 0; j < len; ) {
	    unsigned char code = fgetc(file);
	    if (code > 128) { // run
		code &= 127;
		unsigned char val = fgetc(file);
		while (code--)
		    scanline[j++][i] = val;
	    }
	    else  {	// non-run
		while(code--)
		    scanline[j++][i] = fgetc(file);
	    }
	}
    }

    return feof(file) ? false : true;
}

bool oldDecrunch(RGBE *scanline, int len, FILE *file)
{
    int i;
    int rshift = 0;

    while (len > 0) {
	scanline[0][R] = fgetc(file);
	scanline[0][G] = fgetc(file);
	scanline[0][B] = fgetc(file);
	scanline[0][E] = fgetc(file);
	if (feof(file))
	    return false;

	if (scanline[0][R] == 1 &&
		scanline[0][G] == 1 &&
		scanline[0][B] == 1) {
	    for (i = scanline[0][E] << rshift; i > 0; i--) {
		memcpy(&scanline[0][0], &scanline[-1][0], 4);
		scanline++;
		len--;
	    }
	    rshift += 8;
	}
	else {
	    scanline++;
	    len--;
	    rshift = 0;
	}
    }
    return true;
}


