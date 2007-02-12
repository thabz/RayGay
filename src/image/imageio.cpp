#include "image/imageio.h"
#include "exception.h"

/// Save an image
void ImageIO::save(const Image* const image, const std::string& filename) const
{
    FILE* fp = ::fopen(filename.c_str(), "wb");
    if (fp == NULL)
	throw_exception("Error saving " + filename);
    save(image,fp);	
    ::fclose(fp);
        
}
