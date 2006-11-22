
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_PNG_H

#include <cassert>
#include <iostream>
#include <string>
#include <cstdio>
extern "C" {
#include <memory.h>
#include <png.h>
}

#include "image/imageimpl.h"
#include "image/imageio_png.h"
#include "exception.h"

using namespace std;

void PngIO::save(const Image* const image, const std::string& filename) const {

    FILE *fp;
    png_structp png_ptr;
    png_infop info_ptr;

    int width = image->getWidth();
    int height = image->getHeight();

    /* open the file */
    fp = ::fopen(filename.c_str(), "wb");
    if (fp == NULL)
	throw_exception("Error saving " + filename);

    /* Create and initialize the png_struct with the desired error handler
     * functions.  If you want to use the default stderr and longjump method,
     * you can supply NULL for the last three parameters.  We also check that
     * the library version is compatible with the one used at compile time,
     * in case we are using dynamically linked libraries.  REQUIRED.
     */
    png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
	    NULL, NULL, NULL);

    if (png_ptr == NULL)
    {
	::fclose(fp);
	throw_exception("Error saving " + filename);
    }

    /* Allocate/initialize the image information data.  REQUIRED */
    info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == NULL)
    {
	::fclose(fp);
	png_destroy_write_struct(&png_ptr,  png_infopp_NULL);
	throw_exception("Error saving " + filename);
    }

    /* Set error handling.  REQUIRED if you aren't supplying your own
     * error handling functions in the png_create_write_struct() call.
     */
    if (setjmp(png_jmpbuf(png_ptr)))
    {
	/* If we get here, we had a problem reading the file */
	::fclose(fp);
	png_destroy_write_struct(&png_ptr, &info_ptr);
	throw_exception("Error saving " + filename);
    }

    png_init_io(png_ptr, fp);

    png_set_IHDR(png_ptr, info_ptr, width, height, 8, 
	    PNG_COLOR_TYPE_RGB_ALPHA, PNG_INTERLACE_NONE, 
	    PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

    /*
       text_ptr[0].key = "Title";
       text_ptr[0].text = "RayGay Image";
       text_ptr[0].compression = PNG_TEXT_COMPRESSION_NONE;
       text_ptr[1].key = "Author";
       text_ptr[1].text = "RayGay";
       text_ptr[1].compression = PNG_TEXT_COMPRESSION_NONE;
       text_ptr[2].key = "Description";
       text_ptr[2].text = "An image rendered with RayGay";
       text_ptr[2].compression = PNG_TEXT_COMPRESSION_zTXt;
#ifdef PNG_iTXt_SUPPORTED
text_ptr[0].lang = NULL;
text_ptr[1].lang = NULL;
text_ptr[2].lang = NULL;
#endif

png_set_text(png_ptr, info_ptr, text_ptr, 3);
*/
    png_write_info(png_ptr, info_ptr);

    /* If you are only writing one row at a time, this works */
    png_byte row[width*4];
    png_bytep rowp = row;

    for (int y = 0; y < height; y++)
    {
	for(int x = 0; x < width; x++) {
	    RGBA pixel = image->getRGBA(x,y);
	    row[x*4 + 0] = int(pixel.r() * 255);
	    row[x*4 + 1] = int(pixel.g() * 255);
	    row[x*4 + 2] = int(pixel.b() * 255);
	    row[x*4 + 3] = int(pixel.a() * 255);
	}
	png_write_rows(png_ptr, &rowp, 1);
    }

    png_write_end(png_ptr, info_ptr);

    /* clean up after the write, and free any memory allocated */
    png_destroy_write_struct(&png_ptr, &info_ptr);

    /* close the file */
    ::fclose(fp);
}

Image* PngIO::load(const std::string& filename, Allocator::model_t model) {
    png_structp png_ptr;
    png_infop info_ptr;
    png_uint_32 width, height;
    int bit_depth, color_type, interlace_type;
    FILE *fp;
    if ((fp = ::fopen(filename.c_str(), "rb")) == NULL)
	throw_exception("Error opening " + filename);

    png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
	    (png_voidp) NULL, NULL, NULL);

    if (png_ptr == NULL)
    {
	::fclose(fp);
	throw_exception("Error opening " + filename);
    }

    /* Allocate/initialize the memory for image information.  REQUIRED. */
    info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == NULL)
    {
	::fclose(fp);
	png_destroy_read_struct(&png_ptr, png_infopp_NULL, png_infopp_NULL);
	throw_exception("Error opening " + filename);
    }

    if (setjmp(png_jmpbuf(png_ptr)))
    {
	/* Free all of the memory associated with the png_ptr and info_ptr */
	png_destroy_read_struct(&png_ptr, &info_ptr, png_infopp_NULL);
	::fclose(fp);
	/* If we get here, we had a problem reading the file */
	throw_exception("Error reading " + filename);
    }

    png_init_io(png_ptr, fp);

    png_read_info(png_ptr, info_ptr);

    png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
	    &interlace_type, int_p_NULL, int_p_NULL);

    /* Expand paletted colors into true RGB triplets */
    
    if (color_type == PNG_COLOR_TYPE_PALETTE)
	png_set_palette_to_rgb(png_ptr);

    /* Expand grayscale images to the full 8 bits from 1, 2, or 4 bits/pixel */
    if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
	png_set_gray_1_2_4_to_8(png_ptr);

    if (bit_depth < 8)
        png_set_packing(png_ptr);

    /* Expand paletted or RGB images with transparency to full alpha channels
     * so the data will be available as RGBA quartets.
     */
    if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
	png_set_tRNS_to_alpha(png_ptr);

    png_read_update_info(png_ptr, info_ptr);

    /* Allocate the memory to hold the image using the fields of info_ptr. */

    /* The easiest way to read the image: */
 //   png_bytep row = (png_bytep) png_malloc(png_ptr, png_get_rowbytes(png_ptr, info_ptr));
    png_byte row[png_get_rowbytes(png_ptr, info_ptr)];
    png_bytep rowp = row;
    int bpp = png_get_rowbytes(png_ptr,info_ptr) / width;
 //   cout << "Bytes per pixel: " << bpp << endl;
    assert(bpp == 4 || bpp == 3 || bpp == 1);

    Image* result;
    if (bpp == 4) {
	result = new ImageImpl<uint8_t,4>(width,height,model);
    } else if (bpp == 3) {
	result = new ImageImpl<uint8_t,3>(width,height,model);
    } else if (bpp == 1) {
	result = new ImageImpl<uint8_t,1>(width,height,model);
    } else {
	throw_exception("Error reading " + filename + ": Unsupported bytes per pixel");
    }

    for (uint32_t y = 0; y < height; y++)
    {
	png_read_rows(png_ptr, &rowp, png_bytepp_NULL, 1);
	for(uint32_t x = 0; x < width; x++) {
	    RGBA col;
	    if (bpp == 4) {
		col = RGBA(rowp[4*x + 0] / 255.0,
			   rowp[4*x + 1] / 255.0,
			   rowp[4*x + 2] / 255.0,
			   rowp[4*x + 3] / 255.0);
	    } else if (bpp == 3) {
		col = RGBA(rowp[3*x + 0] / 255.0,
			   rowp[3*x + 1] / 255.0,
			   rowp[3*x + 2] / 255.0,
			   1);
	    } else if (bpp == 1) {
		col = RGBA(rowp[x] / 255.0,
			   rowp[x] / 255.0,
			   rowp[x] / 255.0,
			   1);
	    }
	    result->setRGBA(x,y,col);
	}
    }

    png_read_end(png_ptr, info_ptr);

    /* clean up after the read, and free any memory allocated - REQUIRED */
    png_destroy_read_struct(&png_ptr, &info_ptr, png_infopp_NULL);

    /* close the file */
    ::fclose(fp);

    /* that's it */
    return result;
}

#endif

