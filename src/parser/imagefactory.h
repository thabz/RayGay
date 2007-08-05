
#ifndef PARSER_IMAGE_FACTORY_H
#define PARSER_IMAGE_FACTORY_H

#include "scheme/scheme.h"

class Image;

/**
 * Factory for image-related Scheme-procedures.
 */
class ImageFactory {
    public:
        static SchemeObject* make_image(SchemeObject* width, SchemeObject* height, SchemeObject* bg_color);
        static SchemeObject* load_image(SchemeObject* s_filename);
        static SchemeObject* save_image(SchemeObject* s_image, SchemeObject* s_filename);
	static SchemeObject* set_pixel(SchemeObject* s_image, SchemeObject* s_x, SchemeObject* s_y, SchemeObject* color);
	static SchemeObject* get_pixel(SchemeObject* s_image, SchemeObject* s_x, SchemeObject* s_y);

	static void register_procs(Scheme* scheme);
};



#endif
