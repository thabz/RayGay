
#include "http/imageresponse.h"
#include "exception.h"
#include "image/image.h"
#include <cstdio>
#include <cstdlib>
#include <string>
#include <unistd.h>

ImageResponse::ImageResponse(Image *image) : HTTPResponse(200, "image/png") {
  char templ[] = "/tmp/raygay-image-XXXXXX";
  int fd = ::mkstemp(templ);
  if (fd == -1) {
    throw_exception("Could not create temporary image file");
  }
  ::close(fd);
  ::remove(templ);

  string filename = string(templ) + ".png";
  image->save(filename);
  FILE *f = ::fopen(filename.c_str(), "rb");
  ::remove(filename.c_str());
  if (f == NULL) {
    throw_exception("Could not open temporary image file");
  }
  this->setBody(f);
};

ImageResponse::~ImageResponse() {
  // TODO: Delete the temporary image file
}
