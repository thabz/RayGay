
#ifndef IMAGE_DARWIN_IO_H
#define IMAGE_DARWIN_IO_H

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#ifdef OS_DARWIN

#include "image/imageio.h"
#include <ApplicationServices/ApplicationServices.h>

/**
 * A loader and saver for all the image formats supported by Mac OS X's Quartz.
 */
class DarwinIO : public ImageIO {
    public:
	void save(const Image* const image, const std::string& filename) const;
	Image* load(const std::string& filename, Allocator::model_t = Allocator::AUTO);
    private:
        CFStringRef filenameToUTI(const std::string& filename) const;
};

#endif
#endif
