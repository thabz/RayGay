
#ifndef PARSER_TEXTURE_FACTORY_H
#define PARSER_TEXTURE_FACTORY_H

#include <libguile.h>
#include <map>

class Image;

/**
 * Factory for texture-related Scheme-procedures.
 */
class TextureFactory {
    public:
	static SCM make_texture(SCM s_filename, SCM s_repeat_x, SCM s_repeat_y, SCM s_interpolation_type);
	static SCM get_pixel(SCM s_texture, SCM s_x, SCM s_y);

	static void register_procs(Scheme* scheme);

	static std::map<std::string,Image*> image_cache;

};



#endif
