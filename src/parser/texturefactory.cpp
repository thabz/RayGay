
#include "parser/texturefactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "image/image.h"
#include "image/texture.h"
#include "exception.h"

using namespace std;

SCM TextureFactory::make_texture(SCM s_filename, SCM s_repeat_x, SCM s_repeat_y, SCM s_interpolation_type) {

    string filename = scm2string(s_filename);
    double rep_x = scm_num2double(s_repeat_x,0,"");
    double rep_y = scm_num2double(s_repeat_y,0,"");
    string type_string = scm2string(s_interpolation_type);
    Texture::InterpolationType type = Texture::INTERPOLATION_NONE;

    if (type_string == "none") {
	type = Texture::INTERPOLATION_NONE;
    } else if (type_string == "bilinear") {
	type = Texture::INTERPOLATION_BILINEAR;
    } else if (type_string == "bicubic") {
	type = Texture::INTERPOLATION_BICUBIC;
    } else {
	scm_error(NULL, "make-texture", ("Unknown interpolationtype: " + type_string).c_str(), SCM_UNSPECIFIED, NULL);
    }

    Image* image = NULL;
    try {
    	image = new Image(filename);
    } catch (Exception e) {
	scm_error(NULL, "make-texture", e.getMessage().c_str(), SCM_UNSPECIFIED, NULL);
    }
    Texture* texture = new Texture(image, Vector2(rep_x,rep_y), type);
    return texture2scm(texture);
}

void TextureFactory::register_procs() {
    scm_c_define_gsubr("make-texture",4,0,0,(SCM (*)()) TextureFactory::make_texture);
}