
#ifndef PARSER_TEXTURE_FACTORY_H
#define PARSER_TEXTURE_FACTORY_H

#include "scheme/scheme.h"
#include <map>

class Image;

/**
 * Factory for texture-related Scheme-procedures.
 */
class TextureFactory {
    public:
	static SchemeObject* make_texture(SchemeObject* s_filename, SchemeObject* s_repeat_x, SchemeObject* s_repeat_y, SchemeObject* s_interpolation_type);
	static SchemeObject* get_pixel(SchemeObject* s_texture, SchemeObject* s_x, SchemeObject* s_y);

	static void register_procs(Scheme* scheme);

	static std::map<std::string,Image*> image_cache;

};



#endif
