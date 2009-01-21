
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_JPEGLIB_H

#include <cassert>
#include <iostream>
#include <string>
#include <cstdio>
extern "C" {
#include <memory.h>
#include <setjmp.h>
#include <jpeglib.h>
}

#include "image/imageio_jpeg.h"
#include "image/imageimpl.h"
#include "exception.h"

using namespace std;

void JpegIO::save(const Image* const image, FILE* fp) const {
    throw_exception("JPEG saving not implemented.");
}

struct my_error_mgr {
    struct jpeg_error_mgr pub;  /* "public" fields */
    jmp_buf setjmp_buffer;      /* for return to caller */
};
                                                                               
typedef struct my_error_mgr * my_error_ptr;

    METHODDEF(void)
my_error_exit (j_common_ptr cinfo)
{
    my_error_ptr myerr = (my_error_ptr) cinfo->err;
    (*cinfo->err->output_message) (cinfo);
    longjmp(myerr->setjmp_buffer, 1);
}

void readLine(unsigned char* buf, RGBA* line, int w, int bpp) {
    if (bpp==3) {
	for(int i = 0; i < w; i++) {
	    int r = buf[i*bpp+0];
	    int g = buf[i*bpp+1];
	    int b = buf[i*bpp+2];
	    RGBA col = RGBA(double(r),double(g),double(b),1) / 255.0;
	    line[i] = col;
	}
    } else if (bpp==1) {
	for(int i = 0; i < w; i++) {
	    int r = buf[i];
	    RGBA col = RGBA(double(r),double(r),double(r),1) / 255.0;
	    line[i] = col;
	}
    }
}

Image* JpegIO::load(const std::string& filename, Allocator::model_t model) {
    struct jpeg_decompress_struct cinfo;
    struct my_error_mgr jerr;
    FILE * infile;              /* source file */
    JSAMPARRAY buffer;          /* Output row buffer */

    if ((infile = ::fopen(filename.c_str(), "rb")) == NULL) {
	throw_exception("Unable to open " + filename);
    }
                                                                               
    cinfo.err = jpeg_std_error(&jerr.pub);
    jerr.pub.error_exit = my_error_exit;
    if (setjmp(jerr.setjmp_buffer)) {
        jpeg_destroy_decompress(&cinfo);
        ::fclose(infile);
        return NULL;
    }
                                                                               
    jpeg_create_decompress(&cinfo);
    jpeg_stdio_src(&cinfo, infile);
    jpeg_read_header(&cinfo, TRUE);
    jpeg_start_decompress(&cinfo);

    uint32_t width = cinfo.output_width;
    uint32_t height = cinfo.output_height;

    /* physical row width in output buffer */
    int row_stride = cinfo.output_width * cinfo.output_components;

    buffer = (*cinfo.mem->alloc_sarray)
        ((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);
                                                                               
    uint32_t bpp = cinfo.output_components;
    Image* image;
    if (bpp == 3) {
        image = new ImageImpl<uint8_t,3>(width,height,model);
    } else if (bpp == 1) {
        image = new ImageImpl<uint8_t,1>(width,height,model);
    } else {
	throw_exception("Error reading " + filename + ": Unsupported bytes per pixel");
    }

    RGBA* line = new RGBA[width];
    uint32_t y = 0;
    while (cinfo.output_scanline < height) {
        jpeg_read_scanlines(&cinfo, buffer, 1);
        readLine(buffer[0], line, width, bpp);
        for(uint32_t x = 0; x < width; x++) {
	    image->setRGBA(x,y,line[x]);
	}
	y++;
    }
    delete [] line;
                                                                               
    jpeg_finish_decompress(&cinfo);
    jpeg_destroy_decompress(&cinfo);
    ::fclose(infile);
    return image;

}

#endif /* HAVE_JPEGLIB_H */
