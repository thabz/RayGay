
#include "http/httpcommon.h"

class Image;

class ImageResponse : public HTTPResponse {
  ImageResponse(Image *image);
  ~ImageResponse();
};
