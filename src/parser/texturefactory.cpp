
#include "scheme/filenames.h"

#include "parser/texturefactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "image/image.h"
#include "image/texture.h"
#include "image/simpletexture.h"
#include "exception.h"
#include "renderersettings.h"

using namespace std;

std::map<std::wstring,Image*> TextureFactory::image_cache;

SchemeObject* s_texture_p(Scheme* scheme, SchemeObject* object) {
    return isWrappedObjectType(object, TEXTURE);        
}

SchemeObject* TextureFactory::make_texture(Scheme* scheme, SchemeObject* s_filename, SchemeObject* s_repeat_x, SchemeObject* s_repeat_y, SchemeObject* s_interpolation_type) {

    RendererSettings* renderer_settings = RendererSettings::uniqueInstance();

    wchar_t* proc = L"make-texture";

    double rep_x = safe_scm2double(s_repeat_x,2,proc);
    double rep_y = safe_scm2double(s_repeat_y,3,proc);

    assert_arg_symbol_type(proc, 4, s_interpolation_type);

    wstring type_string = s_interpolation_type->toString();
    Texture::InterpolationType type = Texture::INTERPOLATION_NONE;
    if (type_string == L"none" || RendererSettings::uniqueInstance()->fast_preview) {
	    type = Texture::INTERPOLATION_NONE;
    } else if (type_string == L"bilinear") {
	    type = Texture::INTERPOLATION_BILINEAR;
    } else if (type_string == L"bicubic") {
	    type = Texture::INTERPOLATION_BICUBIC;
    } else {
	    throw scheme_exception(L"make-texture", L"Unknown interpolationtype: " + type_string);
    }

    Image* image;
    try {
        if (s_string_p(scheme, s_filename) == S_TRUE) {    
            wstring filename = scm2string(s_filename);
	        if (image_cache.find(filename) != image_cache.end()) {
	            image = image_cache.find(filename)->second;
	        } else {
	            image = Image::load(SchemeFilenames::toFilename(filename), renderer_settings->image_alloc_model);
	            image_cache.insert(make_pair(filename,image));
	        }
        } else {
            image = scm2image(s_filename, proc, 1);
        }
    } catch (Exception e) {
        cout << e.getMessage() << endl;    
	    throw scheme_exception(L"make-texture");
    }
    Texture* texture = new SimpleTexture(image, Vector2(rep_x,rep_y), type);
    return texture2scm(texture);
}

SchemeObject* TextureFactory::get_pixel(Scheme* scheme, SchemeObject* s_texture, SchemeObject* s_x, SchemeObject* s_y) 
{
    wchar_t* proc = L"texture-get-pixel";
    Texture* texture = scm2texture(s_texture, proc, 1);
    double x = safe_scm2double(s_x, 2, proc);
    double y = safe_scm2double(s_y, 3, proc);
    RGB pixel = texture->getTexel(x,y);
    return rgb2scm(pixel);
}

void TextureFactory::register_procs(Scheme* scheme) {
    scheme->assign(L"texture?",1,0,0,(SchemeObject* (*)()) s_texture_p);
    scheme->assign(L"make-texture",4,0,0,(SchemeObject* (*)()) TextureFactory::make_texture);
    scheme->assign(L"texture-get-pixel",3,0,0,(SchemeObject* (*)()) TextureFactory::get_pixel);
}
