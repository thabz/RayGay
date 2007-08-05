
#include "parser/imagefactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "image/image.h"
#include "image/imageimpl.h"

SchemeObject* ImageFactory::make_image(SchemeObject* s_width, SchemeObject* s_height, SchemeObject* s_background_color) {
    char* proc = "make-image";        
    int w = safe_scm2int(s_width, 1, proc);
    int h = safe_scm2int(s_height, 2, proc);
    Image* image = new ImageImpl<uint8_t,4>(w, h);
    
    if (s_background_color != S_UNSPECIFIED) {
        RGBA color = scm2rgba(s_background_color, proc, 3);
        image->clear(color);
    }
    
    return image2scm(image);
}

SchemeObject* ImageFactory::load_image(SchemeObject* s_filename) {
    string filename = scm2string(s_filename);        
    Image* image = Image::load(filename);
    return image2scm(image);
}

SchemeObject* ImageFactory::save_image(SchemeObject* s_image, SchemeObject* s_filename) {
    char* proc = "save-image";
    Image* image = scm2image(s_image, proc, 1);
    string filename = scm2string(s_filename);        
    image->save(filename);
    return S_UNSPECIFIED;
}

SchemeObject* ImageFactory::set_pixel(SchemeObject* s_image, SchemeObject* s_x, SchemeObject* s_y, SchemeObject* s_color) {
    char* proc = "set-pixel";
    Image* image = scm2image(s_image, proc, 1);
    double x = safe_scm2double(s_x, 2, proc);
    double y = safe_scm2double(s_y, 3, proc);
    RGBA color = scm2rgba(s_color, proc, 4);
    image->setRGBA(int(x),int(y), color);
    return S_UNSPECIFIED;
}

SchemeObject* ImageFactory::get_pixel(SchemeObject* s_image, SchemeObject* s_x, SchemeObject* s_y) {
    char* proc = "get-pixel";
    Image* image = scm2image(s_image, proc, 1);
    double x = safe_scm2double(s_x, 2, proc);
    double y = safe_scm2double(s_y, 3, proc);
    RGB pixel = image->getRGBA(int(x),int(y));
    return rgb2scm(pixel);
}

void ImageFactory::register_procs(Scheme* scheme) {
    scheme->assign("make-image",2,1,0,(SchemeObject* (*)()) ImageFactory::make_image);
    scheme->assign("load-image",1,0,0,(SchemeObject* (*)()) ImageFactory::load_image);
    scheme->assign("save-image",2,0,0,(SchemeObject* (*)()) ImageFactory::save_image);
    scheme->assign("set-pixel",4,0,0,(SchemeObject* (*)()) ImageFactory::set_pixel);
    scheme->assign("get-pixel",3,0,0,(SchemeObject* (*)()) ImageFactory::get_pixel);
}
