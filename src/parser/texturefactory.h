
#ifndef PARSER_TEXTURE_FACTORY_H
#define PARSER_TEXTURE_FACTORY_H

#include <libguile.h>

/**
 * Factory for texture-related Scheme-procedures.
 */
class TextureFactory {
    public:
	static SCM make_texture(SCM s_filename, SCM s_repeat_x, SCM s_repeat_y, SCM s_interpolation_type);

	static void register_procs();
};



#endif
