#include "image/image.h"
#include "image/pngio.h"
#include <cassert>
#include <iostream>
#include <string>
#include <cstdio>
#include <memory.h>
 //#include <setjmp.h>
#include <png.h>

using namespace std;

void PngIO::save(const Image* const image, const std::string& filename) const {
    cout << "PNG saving not implemented " << __FILE__ << __LINE__ << endl;
    return;
}

Image* PngIO::load(const std::string& filename) {
   png_structp png_ptr;
   png_infop info_ptr;
   png_uint_32 width, height;
   int bit_depth, color_type, interlace_type;
   FILE *fp;
   if ((fp = fopen(filename.c_str(), "rb")) == NULL)
       return NULL;

   png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
	   (png_voidp) NULL, NULL, NULL);

   if (png_ptr == NULL)
   {
       fclose(fp);
       return NULL;
   }

   /* Allocate/initialize the memory for image information.  REQUIRED. */
   info_ptr = png_create_info_struct(png_ptr);
   if (info_ptr == NULL)
   {
      fclose(fp);
      png_destroy_read_struct(&png_ptr, png_infopp_NULL, png_infopp_NULL);
      return NULL;
   }

   if (setjmp(png_jmpbuf(png_ptr)))
   {
      /* Free all of the memory associated with the png_ptr and info_ptr */
      png_destroy_read_struct(&png_ptr, &info_ptr, png_infopp_NULL);
      fclose(fp);
      /* If we get here, we had a problem reading the file */
      return (NULL);
   }

   png_init_io(png_ptr, fp);

   png_read_info(png_ptr, info_ptr);

   png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
       &interlace_type, int_p_NULL, int_p_NULL);

   /* Expand paletted colors into true RGB triplets */
   /*
   if (color_type == PNG_COLOR_TYPE_PALETTE)
      png_set_palette_rgb(png_ptr);
*/
   /* Expand grayscale images to the full 8 bits from 1, 2, or 4 bits/pixel */
   if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
      png_set_gray_1_2_4_to_8(png_ptr);

   /* Expand paletted or RGB images with transparency to full alpha channels
    * so the data will be available as RGBA quartets.
    */
   if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
      png_set_tRNS_to_alpha(png_ptr);

   png_read_update_info(png_ptr, info_ptr);

   /* Allocate the memory to hold the image using the fields of info_ptr. */

   /* The easiest way to read the image: */
   png_bytep row = (png_bytep) png_malloc(png_ptr, png_get_rowbytes(png_ptr, info_ptr));

   for (unsigned int y = 0; y < height; y++)
   {
       png_read_rows(png_ptr, &row, png_bytepp_NULL, 1);
   }

   png_read_end(png_ptr, info_ptr);

   /* clean up after the read, and free any memory allocated - REQUIRED */
   png_destroy_read_struct(&png_ptr, &info_ptr, png_infopp_NULL);

   /* close the file */
   fclose(fp);

   /* that's it */
   return NULL;

}

