
#include "images/imageio_darwin.h"

#ifdef OS_DARWIN

#include <ApplicationServices/ApplicationServices.h>

using namespace std;

void DarwinIO::save(const Image* const image, const std::string& filename) const
{
    CFImageRef imageRef;
    // TODO: Create CFImage from imagedata.
    CFStringRef UTI = filenameToUTI(filename);
    CFStringRef path = CFStringCreateWithCString(NULL, filename, kCFStringEncodingUTF8);
    CFURLRef url = CFURLCreateWithFileSystemPath (NULL, path, kCFURLPOSIXPathStyle, NULL);
    CGImageDestinationRef imageDest =  CGImageDestinationCreateWithURL(url, kUTTypePNG, 1, NULL);
    CGImageDestinationAddImage(imageDest, imageRef, NULL);
    CGImageDestinationFinalize(imageDest);
    CFRelease(path);
    CFRelease(url);
}

Image* DarwinIO::load(const std::string& filename) 
{
    CFStringRef path = CFStringCreateWithCString(NULL, filename, kCFStringEncodingUTF8);
    CFURLRef url = CFURLCreateWithFileSystemPath (NULL, path, kCFURLPOSIXPathStyle, NULL);
    CGImageSourceRef source = CGImageSourceCreateWithURL(url, NULL);
    CGImageRef image = CGImageSourceCreateImageAtIndex(source, 0, NULL);
    CFRelease(path);
    CFRelease(url);
    long w = CGImageGetWidth(image);
    long h = CGImageGetHeight(image);
    Image result = new Image(w,h);
    // TODO: Copy image data
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
}

#endif