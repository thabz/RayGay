
#include "parser/texturefactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "image/image.h"
#include "image/texture.h"
#include "exception.h"
#include "renderersettings.h"

using namespace std;

std::map<std::string,Image*> TextureFactory::image_cache;

SCM TextureFactory::make_texture(SCM s_filename, SCM s_repeat_x, SCM s_repeat_y, SCM s_interpolation_type) {

    RendererSettings* renderer_settings = RendererSettings::uniqueInstance();

    char* proc = "make-texture";

    string filename = scm2string(s_filename);
    double rep_x = scm_num2double(s_repeat_x,2,proc);
    double rep_y = scm_num2double(s_repeat_y,3,proc);

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

    Image* image;
    try {
	if (image_cache.find(filename) != image_cache.end()) {
	    image = image_cache.find(filename)->second;
	} else {
    	    image = Image::load(filename, renderer_settings->image_alloc_model);
	    image_cache.insert(make_pair(filename,image));
	}
    } catch (Exception e) {
	scm_error(NULL, "make-texture", e.getMessage().c_str(), SCM_UNSPECIFIED, NULL);
    }
    Texture* texture = new Texture(image, Vector2(rep_x,rep_y), type);
    return texture2scm(texture);
}

SCM TextureFactory::get_pixel(SCM s_texture, SCM s_x, SCM s_y) 
{
    char* proc = "get-pixel";
    Texture* texture = scm2texture(s_texture, proc, 1);
    double x = scm_num2double(s_x, 2, proc);
    double y = scm_num2double(s_y, 2, proc);
    RGB pixel = texture->getTexel(x,y);
    return rgb2scm(pixel);
}
    

void TextureFactory::register_procs() {
    scm_c_define_gsubr("make-texture",4,0,0,(SCM (*)()) TextureFactory::make_texture);
    scm_c_define_gsubr("get-pixel",3,0,0,(SCM (*)()) TextureFactory::get_pixel);
}
