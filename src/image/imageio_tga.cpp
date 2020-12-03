
#include "image/imageio_tga.h"
#include "exception.h"
#include "image/imageimpl.h"
#include <cassert>
#include <cmath>
#include <cstring>
#include <iostream>
#include <string>

using namespace std;

/**
 * Writes the image into a  24 bit uncompressed tga-file
 */
void TgaIO::save(const Image *const image, FILE *outfile) const {
  int height = image->getHeight();
  int width = image->getWidth();

  uint8_t *bytes = new uint8_t[height * width * 4];
  uint8_t header[18];
  ::memset(&header, 0, 18);

  header[0] = 0;
  header[1] = 0;
  header[2] = 2;
  header[3] = 0;
  header[4] = 0;
  header[5] = 0;
  header[6] = 0;
  header[7] = 0;
  header[8] = 0;
  header[9] = 0;
  header[10] = 0;
  header[11] = 0;
  header[12] = width;
  header[13] = ((long)width >> 8);
  header[14] = height;
  header[15] = ((long)height >> 8);
  header[16] = 32;
  header[17] = 0;

  RGBA color;

  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      color = image->getRGBA(x, (height - 1) - y);
      bytes[4 * (x + y * width) + 0] = (uint8_t)(floor(color.b() * 255 + 0.5));
      bytes[4 * (x + y * width) + 1] = (uint8_t)(floor(color.g() * 255 + 0.5));
      bytes[4 * (x + y * width) + 2] = (uint8_t)(floor(color.r() * 255 + 0.5));
      bytes[4 * (x + y * width) + 3] = (uint8_t)(floor(color.a() * 255 + 0.5));
    }
  }

  //::fseek(outfile,0,0);
  ::fwrite(header, sizeof(uint8_t), 18, outfile);
  //::fseek(outfile,18,0);
  int bytes_saved =
      ::fwrite(bytes, sizeof(uint8_t), width * height * 4, outfile);

  // std::cout << bytes_saved << " bytes written" << std::endl;
  if (bytes_saved < width * height * 4) {
    throw_exception("Error saving file");
  }
  delete[] bytes;
}

RGBA readpixel(FILE *handle, int bpp) {
  uint8_t data[4];
  size_t bytes_read = ::fread(data, 1, bpp, handle);
  assert(bytes_read == bpp);

  RGBA result;
  switch (bpp) {
  case 3:
    result = RGBA(data[2], data[1], data[0], 255);
    break;
  case 4:
    result = RGBA(data[2], data[1], data[0], data[3]);
    break;
  case 2:
    // For 16 bits each colour component is stored as
    // 5 bits and the remaining bit is a binary alpha value.
    // uint16_t d = data[1] << 8 + data[0];
    throw_exception("Two bpp is not implemented");
    break;
  case 1:
    result = RGBA(data[0], data[0], data[0], 255);
    break;
  }
  result *= double(1.0 / 255.0);
  return result;
}

/**
 * @param handle The open file handle
 * @param dest an array of width length for storing RGBA values
 * @param width pixels per line
 * @param rle whether the data is compressed
 * @param bpp bytes per pixel (3 or 4)
 */
void readscanline(FILE *handle, RGBA *dest, int width, int bpp, bool rle) {
  if (rle) {
    int pixels_read = 0;
    uint8_t repcount;
    do {
      size_t bytes_read = ::fread(&repcount, 1, 1, handle);
      assert(bytes_read == 1);
      int count = (int(repcount) & 127) + 1;
      if (int(repcount) > 127) {
        // Process run-length packet
        RGBA color = readpixel(handle, bpp);
        for (int i = 0; i < count; i++) {
          if (pixels_read + i >= width)
            continue; // Safety
          dest[pixels_read + i] = color;
        }
      } else {
        // Process raw packet
        for (int i = 0; i < count; i++) {
          if (pixels_read + i >= width)
            continue; // Safety
          dest[pixels_read + i] = readpixel(handle, bpp);
        }
      }
      pixels_read += count;
    } while (pixels_read < width);
  } else {
    // Read an uncompressed line
    for (int x = 0; x < width; x++) {
      dest[x] = readpixel(handle, bpp);
    }
  }
}

/**
 * Loads the image from a tga 24 og 32 bit uncompressed tga-file.
 */
Image *TgaIO::load(const std::string &filename, Allocator::model_t model) {

  FILE *Handle;
  uint8_t Header[18];
  Handle = ::fopen(filename.c_str(), "rb");
  if (Handle == NULL) {
    throw_exception("Error opening " + filename);
  }

  ::fseek(Handle, 0, SEEK_SET);
  size_t bytes_read = ::fread(Header, 1, 18, Handle);
  assert(bytes_read == 18);

  int bpp = int(Header[16]) / 8; // Bytes per pixel

  // Says whether the data is rle encoded
  bool rle = int(Header[2]) >> 3 == 1;
  int datatype = int(Header[2]);
  //   cout << "bpp: " << bpp << (rle ? " (rle)" : "") << endl;
  //   cout << "header[2]: " << datatype << "." << endl;

  if (Header[1] != 0 || datatype == 0 || datatype == 1 || datatype == 9 ||
      datatype == 32 || datatype == 33 || bpp == 2) {
    throw_exception("Error opening " + filename +
                    ": Only 8, 24 or 32 bit truecolor RGB is supported");
  }

  long width = ((long)Header[13] << 8) + Header[12];
  long height = ((long)Header[15] << 8) + Header[14];
  assert(width > 0);
  assert(height > 0);

  // Skip id field which is right after the header.
  // Length of id field is header[0]
  uint8_t skip_id_field = Header[0];
  ::fseek(Handle, skip_id_field, SEEK_CUR);

  Image *image;
  if (bpp == 4) {
    image = new ImageImpl<uint8_t, 4>(width, height, model);
  } else if (bpp == 3) {
    image = new ImageImpl<uint8_t, 3>(width, height, model);
  } else if (bpp == 1) {
    image = new ImageImpl<uint8_t, 1>(width, height, model);
  } else {
    throw_exception("Error opening " + filename +
                    ": Unsupported bytes per pixel.");
  }

  RGBA *line = new RGBA[width];
  for (int y = 0; y < height; y++) {
    readscanline(Handle, line, width, bpp, rle);
    for (int x = 0; x < width; x++) {
      image->setRGBA(x, height - 1 - y, line[x]);
    }
  }
  delete[] line;
  ::fclose(Handle);
  return image;
}
