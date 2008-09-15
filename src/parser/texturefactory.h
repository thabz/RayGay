
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
        static SchemeObject* make_texture(Scheme* scheme, SchemeObject* s_filename, SchemeObject* s_repeat_x, SchemeObject* s_repeat_y, SchemeObject* s_interpolation_type);
        static SchemeObject* make_multi_texture(Scheme* scheme, SchemeObject* s_filenames, SchemeObject* s_tiles_per_row, SchemeObject* s_memory_cached, SchemeObject* s_repeat_x, SchemeObject* s_repeat_y, SchemeObject* s_interpolation_type);

	    static SchemeObject* get_pixel(Scheme* scheme, SchemeObject* s_texture, SchemeObject* s_x, SchemeObject* s_y);

	    static void register_procs(Scheme* scheme);

	    static std::map<std::wstring,Image*> image_cache;

};

SchemeObject* s_texture_p(Scheme* scheme, SchemeObject* object_smob);


#endif
