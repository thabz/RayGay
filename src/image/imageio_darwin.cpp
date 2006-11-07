
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef OS_DARWIN

#include <ApplicationServices/ApplicationServices.h>

#include "image/imageio_darwin.h"
#include "exception.h"
#include "image/imageimpl.h"

using namespace std;

void DarwinIO::save(const Image* const image, const std::string& filename) const
{
    uint32_t w = image->getWidth();
    uint32_t h = image->getHeight();
    uint8_t* data = (uint8_t*)malloc(w * h * 4);

    // The only CGImageBitmapInfo supported in Mac OS 10.4 seems to be 
    // kCGImageAlphaPremultipliedLast so we got to multiply RGB with alpha below.
    for(uint32_t y = 0; y < h; y++ ) {
            for( uint32_t x = 0; x < w; x++ ) {
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

Image* DarwinIO::load(const std::string& filename, Allocator::model_t model) 
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
    
    // TODO: Use floats when imageRef requires it. Also only use alpha when needed.
    Image* result = new ImageImpl<uint8_t,4>(w,h,model);
    
    TYPE* data = (TYPE*)malloc(w * h * 4 * sizeof(TYPE));
    for( uint32_t i = 0; i < w*h*4; i += 4 ) {
            data[i+0] = 0;
            data[i+1] = 0;
            data[i+2] = 0;
            data[i+3] = 0;
    }
    CGBitmapInfo bitmapInfo = kCGImageAlphaPremultipliedLast;
    if (sizeof(TYPE) >= 4)
        bitmapInfo = bitmapInfo | kCGBitmapFloatComponents;
    CGColorSpaceRef colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
    CGContextRef contextRef = CGBitmapContextCreate(data, w, h, sizeof(TYPE)*8, sizeof(TYPE)*4*w, 
                                   colorSpace, bitmapInfo);
    CGContextSetShouldAntialias (contextRef, false);
    CGContextSetInterpolationQuality(contextRef, kCGInterpolationNone);

    CGRect rect;
    rect.origin = CGPointZero;
    rect.size.width = w;
    rect.size.height = h;
    CGContextDrawImage(contextRef,rect,imageRef);

    for(uint32_t y = 0; y < h; y++ ) {
            for( uint32_t x = 0; x < w; x++ ) {
                    RGBA c;
                    double a = double(data[4*(x + y*w) + 3]) / 255.0;
                    if (IS_ZERO(a)) {
                        c = RGBA(0,0,0,0);      
                    } else {
                        c = RGBA((double(data[4*(x + y*w) + 0]) / 255.0) / a, 
                                 (double(data[4*(x + y*w) + 1]) / 255.0) / a,
                                 (double(data[4*(x + y*w) + 2]) / 255.0) / a,
                                 a);
                    }             
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
