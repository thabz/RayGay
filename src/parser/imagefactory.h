
#ifndef PARSER_IMAGE_FACTORY_H
#define PARSER_IMAGE_FACTORY_H

#include "scheme/scheme.h"
#include "image/imagedrawing.h"

class Image;

/**
 * Factory for image-related Scheme-procedures.
 */
class ImageFactory {
    public:
        static SchemeObject* make_image(Scheme* scheme, SchemeObject* width, SchemeObject* height, SchemeObject* bg_color);
        static SchemeObject* image_copy(Scheme* scheme, SchemeObject* s_image);
        static SchemeObject* load_image(Scheme* scheme, SchemeObject* s_filename);
        static SchemeObject* save_image(Scheme* scheme, SchemeObject* s_image, SchemeObject* s_filename);
        static SchemeObject* image_width(Scheme* scheme, SchemeObject* s_image);
        static SchemeObject* image_height(Scheme* scheme, SchemeObject* s_image);
        static SchemeObject* set_alpha_combine_mode(Scheme* scheme, SchemeObject* s_mode);
	    static SchemeObject* set_pixel(Scheme* scheme, SchemeObject* s_image, SchemeObject* s_x, SchemeObject* s_y, SchemeObject* color);
	    static SchemeObject* get_pixel(Scheme* scheme, SchemeObject* s_image, SchemeObject* s_x, SchemeObject* s_y);
	    static SchemeObject* draw_line(Scheme* scheme, SchemeObject* s_image, SchemeObject* s_from, SchemeObject* s_to, SchemeObject* color);
	    static SchemeObject* draw_filled_box(Scheme* scheme, SchemeObject* s_image, SchemeObject* s_pos, SchemeObject* s_size, SchemeObject* color);
	    static SchemeObject* draw_circle(Scheme* scheme, SchemeObject* s_image, SchemeObject* s_center, SchemeObject* s_radius, SchemeObject* color);
        static SchemeObject* draw_string(Scheme* scheme, SchemeObject* s_image, SchemeObject* s_center, SchemeObject* s_text, SchemeObject* s_size, SchemeObject* s_ttf_file, SchemeObject* s_color);
        static SchemeObject* apply_gaussian_blur(Scheme* scheme, SchemeObject* s_image, SchemeObject* s_radius);
	    static void register_procs(Scheme* scheme);

        static ImageDrawing::AlphaCombineMode alpha_combine_mode;
        static SchemeObject* modulate_mode_symbol;
        static SchemeObject* add_mode_symbol;
        static SchemeObject* replace_mode_symbol;
        static SchemeObject* decal_mode_symbol;


};

SchemeObject* s_image_p(Scheme* scheme, SchemeObject* object);

#endif

