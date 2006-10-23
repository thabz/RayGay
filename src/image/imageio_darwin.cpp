
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef OS_DARWIN

#include <ApplicationServices/ApplicationServices.h>

#include "image/imageio_darwin.h"
#include "exception.h"
#include "image/image.h"

using namespace std;

/**
 * TODO: Gå ikke omkring uint8_t men brug floats eller doubles.
 */
void DarwinIO::save(const Image* const image, const std::string& filename) const
{
    uint32_t w = image->getWidth();
    uint32_t h = image->getHeight();
    uint8_t* data = (uint8_t*)malloc(w * h * 4);

    for(uint32_t y = 0; y < h; y++ ) {
            for( unsigned int x = 0; x < w; x++ ) {
                    RGBA c = image->getRGBA(x,y);
                    c.clip();
                    data[4*(x + y*w) + 0] = uint8_t(c.r()*c.a()*255.0);
                    data[4*(x + y*w) + 1] = uint8_t(c.g()*c.a()*255.0);
                    data[4*(x + y*w) + 2] = uint8_t(c.b()*c.a()*255.0);
                    data[4*(x + y*w) + 3] = uint8_t(c.a()*255.0);
            }
    }

    CGColorSpaceRef colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
    CGContextRef contextRef = CGBitmapContextCreate(data, w, h, 8, 4*w, 
                                   colorSpace, kCGImageAlphaPremultipliedLast);

    CGImageRef imageRef = CGBitmapContextCreateImage(contextRef);  

    CFStringRef UTI = filenameToUTI(filename);
    CFStringRef path = CFStringCreateWithCString(NULL, filename.c_str(), kCFStringEncodingUTF8);
    CFURLRef url = CFURLCreateWithFileSystemPath (NULL, path, kCFURLPOSIXPathStyle, NULL);
    CGImageDestinationRef imageDest =  CGImageDestinationCreateWithURL(url, UTI, 1, NULL);

    CGImageDestinationAddImage(imageDest, imageRef, NULL);
    CGImageDestinationFinalize(imageDest);
    CFRelease(path);
    CFRelease(url);
}

Image* DarwinIO::load(const std::string& filename) 
{
#define TYPE uint8_t
        
    CFStringRef path = CFStringCreateWithCString(NULL, filename.c_str(), kCFStringEncodingUTF8);
    CFURLRef url = CFURLCreateWithFileSystemPath (NULL, path, kCFURLPOSIXPathStyle, NULL);
    CGImageSourceRef source = CGImageSourceCreateWithURL(url, NULL);
    CGImageRef imageRef = CGImageSourceCreateImageAtIndex(source, 0, NULL);
    CFRelease(path);
    CFRelease(url);
    uint64_t w = CGImageGetWidth(imageRef);
    uint64_t h = CGImageGetHeight(imageRef);
    CGImageAlphaInfo alpha_info = CGImageGetAlphaInfo(imageRef);
    Image* result = new Image(w,h);
    
    TYPE* data = (TYPE*)malloc(w * h * 4 * sizeof(TYPE));
    for( uint32_t i = 0; i < w*h*4; i += 4 )
    {
            data[i+0] = 0.0;
            data[i+1] = 0.0;
            data[i+2] = 0.0;
            data[i+3] = 0.0;
    }
    CGBitmapInfo bitmapInfo = kCGImageAlphaPremultipliedLast;
    if (sizeof(TYPE) == 4)
        bitmapInfo = bitmapInfo | kCGBitmapFloatComponents;
    CGColorSpaceRef colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
    CGContextRef contextRef = CGBitmapContextCreate(data, w, h, sizeof(TYPE)*8, sizeof(TYPE)*4*w, 
                                   colorSpace, bitmapInfo);
    CGRect rect;
    rect.origin = CGPointZero;
    rect.size.width = w;
    rect.size.height = h;
    CGContextDrawImage(contextRef,rect,imageRef);
    alpha_info = kCGImageAlphaPremultipliedLast;
    for(uint32_t y = 0; y < h; y++ ) {
            for( uint32_t x = 0; x < w; x++ ) {
                    RGBA c;
                    if (alpha_info == kCGImageAlphaNone || alpha_info == kCGImageAlphaNoneSkipLast) {
                            c = RGBA(double(data[4*(x + y*w) + 0]) / 255.0, 
                                    double(data[4*(x + y*w) + 1]) / 255.0,
                                    double(data[4*(x + y*w) + 2]) / 255.0,
                                    1);
                    } else if (alpha_info == kCGImageAlphaNoneSkipFirst) {
                            c = RGBA(double(data[4*(x + y*w) + 1]) / 255.0, 
                                    double(data[4*(x + y*w) + 2]) / 255.0,
                                    double(data[4*(x + y*w) + 3]) / 255.0,
                                    1);
                    } else if (alpha_info == kCGImageAlphaFirst) {
                            c = RGBA(double(data[4*(x + y*w) + 1]) / 255.0, 
                                    double(data[4*(x + y*w) + 2]) / 255.0,
                                    double(data[4*(x + y*w) + 3]) / 255.0,
                                    double(data[4*(x + y*w) + 0]) / 255.0);
                    } else if (alpha_info == kCGImageAlphaLast) {
                            c = RGBA(double(data[4*(x + y*w) + 0]) / 255.0, 
                                    double(data[4*(x + y*w) + 1]) / 255.0,
                                    double(data[4*(x + y*w) + 2]) / 255.0,
                                    double(data[4*(x + y*w) + 3]) / 255.0);
                    } else if (alpha_info == kCGImageAlphaFirst) {
                            c = RGBA(double(data[4*(x + y*w) + 1]) / 255.0, 
                                    double(data[4*(x + y*w) + 2]) / 255.0,
                                    double(data[4*(x + y*w) + 3]) / 255.0,
                                    double(data[4*(x + y*w) + 0]) / 255.0);
                    } else if (alpha_info == kCGImageAlphaOnly) {
                            c = RGBA(double(data[4*(x + y*w) + 0]) / 255.0, 
                                    double(data[4*(x + y*w) + 0]) / 255.0,
                                    double(data[4*(x + y*w) + 0]) / 255.0,
                                    double(data[4*(x + y*w) + 0]) / 255.0);
                    } else if (alpha_info == kCGImageAlphaPremultipliedFirst) {
                            c = RGBA(double(data[4*(x + y*w) + 1]) / 255.0, 
                                    double(data[4*(x + y*w) + 2]) / 255.0,
                                    double(data[4*(x + y*w) + 3]) / 255.0,
                                    1);
                    } else if (alpha_info == kCGImageAlphaPremultipliedLast) {
                            double a = double(data[4*(x + y*w) + 3]) / 255.0;
                            c = RGBA(double(data[4*(x + y*w) + 0]) / 255.0, 
                                     double(data[4*(x + y*w) + 1]) / 255.0,
                                     double(data[4*(x + y*w) + 2]) / 255.0,
                                     a);
                    };
                    result->setRGBA(x,y,c);
            }
    }
    return result;
}

CFStringRef DarwinIO::filenameToUTI(const string& filename) const 
{
    if (filename.find(".png") != string::npos) {
        return kUTTypePNG;
    } 
    else if (filename.find(".jpg") != string::npos || filename.find(".jpeg") != string::npos) {
           return kUTTypeJPEG;
    }     
    else if (filename.find(".jp2") != string::npos) {
           return kUTTypeJPEG2000;
    }     
    else if (filename.find(".tif") != string::npos || filename.find(".tiff") != string::npos) {
           return kUTTypeTIFF;
    }
    else {
    	throw_exception(filename + " has unknown fileformat.");
    }
}

#endif
