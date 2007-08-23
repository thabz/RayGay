
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
        static SchemeObject* make_image(SchemeObject* width, SchemeObject* height, SchemeObject* bg_color);
        static SchemeObject* load_image(SchemeObject* s_filename);
        static SchemeObject* save_image(SchemeObject* s_image, SchemeObject* s_filename);
        static SchemeObject* image_width(SchemeObject* s_image);
        static SchemeObject* image_height(SchemeObject* s_image);
        static SchemeObject* set_alpha_combine_mode(SchemeObject* s_mode);
	static SchemeObject* set_pixel(SchemeObject* s_image, SchemeObject* s_x, SchemeObject* s_y, SchemeObject* color);
	static SchemeObject* get_pixel(SchemeObject* s_image, SchemeObject* s_x, SchemeObject* s_y);
	static SchemeObject* draw_line(SchemeObject* s_image, SchemeObject* s_from, SchemeObject* s_to, SchemeObject* color);
	static SchemeObject* draw_circle(SchemeObject* s_image, SchemeObject* s_center, SchemeObject* s_radius, SchemeObject* color);
        static SchemeObject* draw_string(SchemeObject* s_image, SchemeObject* s_center, SchemeObject* s_text, SchemeObject* s_size, SchemeObject* s_ttf_file, SchemeObject* s_color);
        static SchemeObject* apply_gaussian_blur(SchemeObject* s_image, SchemeObject* s_radius);
	static void register_procs(Scheme* scheme);

        static ImageDrawing::AlphaCombineMode alpha_combine_mode;
        static SchemeObject* modulate_mode_symbol;
        static SchemeObject* add_mode_symbol;
        static SchemeObject* replace_mode_symbol;
        static SchemeObject* decal_mode_symbol;


};



#endif
