
#include <cstdio>
#include "http/imageresponse.h"
#include "image/image.h"

ImageResponse::ImageResponse(Image* image) : HTTPResponse (200, "image/png") {
    char filename[L_tmpnam+1];
    char* ptr;
    ptr = tmpnam(filename);
    image->save(string(ptr)+".png");
    FILE* f = ::fopen(ptr,"r");
    this->setBody(f);
};

ImageResponse::~ImageResponse() {
    // TODO: Delete the temporary image file
}

