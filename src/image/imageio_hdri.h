
#ifndef IMAGE_IO_HDRI_H
#define IMAGE_IO_HDRI_H

class Image;
#include "imageio.h"

/**
 * A loader for HDRI image files.
 *
 * Based on code by Igor Kravtchenko (www.obrazstudio.com)
 * posted on www.flipcode.com.
 *
 * @see http://www.flipcode.com/cgi-bin/msg.cgi?showThread=COTD-HDRImageReader&forum=cotd&id=-1
 * 
 */
class HdriIO : public ImageIO {
    public:
	/// Save an image
	void save(const Image* const image, const std::string& filename) const;
	/// Load an image
	Image* load(const std::string& filename);
};



#endif
