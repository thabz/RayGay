
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef OS_DARWIN

#include "image/imageio_darwin.h"
#include <ApplicationServices/ApplicationServices.h>
#include "exception.h"
#include "image/image.h"

using namespace std;

void DarwinIO::save(const Image* const image, const std::string& filename) const
{
    // TODO: Create CFImage from imagedata.
    // CGBitmapContextCreate...
    // ..Plot
    // CGImageRef CGBitmapContextCreateImage(CGContextRef)

    unsigned long w = image->getWidth();
    unsigned long h = image->getHeight();
    unsigned char* data = (unsigned char*)malloc(w * h * 4);

    for( unsigned int y = 0; y < w; y++ ) {
            for( unsigned int x = 0; x < w; x++ ) {
                    RGBA c = image->getRGBA(x,y);
                    c.clip();
                    c *= 255;
                    data[4*(x + y*w) + 0] = c.r();
                    data[4*(x + y*w) + 1] = c.g();
                    data[4*(x + y*w) + 2] = c.b();
                    data[4*(x + y*w) + 3] = c.a();
            }
    }

    CGColorSpaceRef colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
    CGContextRef contextRef = CGBitmapContextCreate(data, w, h, 4, 4*w, 
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
    CFStringRef path = CFStringCreateWithCString(NULL, filename.c_str(), kCFStringEncodingUTF8);
    CFURLRef url = CFURLCreateWithFileSystemPath (NULL, path, kCFURLPOSIXPathStyle, NULL);
    CGImageSourceRef source = CGImageSourceCreateWithURL(url, NULL);
    CGImageRef image = CGImageSourceCreateImageAtIndex(source, 0, NULL);
    CFRelease(path);
    CFRelease(url);
    long w = CGImageGetWidth(image);
    long h = CGImageGetHeight(image);
    Image* result = new Image(w,h);
    // TODO: Copy image data

    // CGContextRef context = CGBitmapContextCreate...
    // CGContextDrawImage(context,rect,image);
    // Kopier pixels fra contextens backing array

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
