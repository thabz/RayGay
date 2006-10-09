
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <ctime>
#include <cstdio>
#include <iostream>
#include <vector>
#include <unistd.h>
#include <time.h>

#include <ApplicationServices/ApplicationServices.h>
#include "image/image.h"
#include "exception.h"

using namespace std;


void saveImageAsPNG(CGImageRef imageRef, char *filename) 
{
        CFStringRef path = CFStringCreateWithCString(NULL, filename, kCFStringEncodingUTF8);
        CFURLRef url = CFURLCreateWithFileSystemPath (NULL, path, kCFURLPOSIXPathStyle, NULL);
        CGImageDestinationRef imageDest =  CGImageDestinationCreateWithURL(url, kUTTypePNG, 1, NULL);
        CGImageDestinationAddImage(imageDest, imageRef, NULL);
        CGImageDestinationFinalize(imageDest);
        CFRelease(path);
        CFRelease(url);
        cout << "Saved file " << filename << endl;
}

CGImageRef loadImage(const char* filename) 
{
        CFStringRef path = CFStringCreateWithCString(NULL, filename, kCFStringEncodingUTF8);
        CFURLRef url = CFURLCreateWithFileSystemPath (NULL, path, kCFURLPOSIXPathStyle, NULL);
        CGImageSourceRef source = CGImageSourceCreateWithURL(url, NULL);
        CGImageRef image = CGImageSourceCreateImageAtIndex(source, 0, NULL);
        CFRelease(path);
        CFRelease(url);
        return image;
}

void saveSomeData() 
{
        int w = 640;
        int h = 480;
        float* data = (float*)malloc(w * h * 4 * sizeof(float));

        for( unsigned int i = 0; i < 100; i += 1 ) {
                data[4*(i + i*w)] = 255;
        }

        CGColorSpaceRef colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);

        CGContextRef contextRef = CGBitmapContextCreate(data, w, h, 8*sizeof(float), 4*w*sizeof(float), 
                                       colorSpace, kCGImageAlphaPremultipliedLast|kCGBitmapFloatComponents);
        CGColorSpaceRelease(colorSpace);        
        CGImageRef image = CGBitmapContextCreateImage(contextRef);
        saveImageAsPNG(image, "data.png");
}

void createImage() {
        Image* image = new Image(640,480);
        for( unsigned int x = 0; x < 100; x += 1 )
        {
                for( unsigned int y = 0; y < 100; y += 1 )
                {
                        image->setRGBA(x+100,y+100,RGBA(x / 100.0, y / 100.0,0,0.5));
                }
                
        }
        image->save("created.png");
}

void saveload() {
        Image* image = Image::load("test.png");
        image->save("test.jp2");
}
int main(int argc, char *argv[]) 
{
        
    try {    
        cout << "Creating an image and saving it" << endl;
        saveload();
        cout << "It worked?" << endl;
    } catch (Exception e) {
	cout << "Exception: " << e.getMessage() 
	    << " at " << e.getSourceFile() << ":" << e.getSourceLine() << endl;
	return EXIT_FAILURE;
    }
    
/*
    cout << "Trying to read an imagefile using Quartz 2D" << endl;

    const char* filename = "test.jpg";
    CGImageRef image = loadImage(filename);
    
    cout << "Read " << filename << endl;
    cout << "Size: " << CGImageGetWidth(image) << "x" << CGImageGetHeight(image) << endl;

    cout << "Trying to write a imagefile using Quartz 2D" << endl;

    saveImageAsPNG(image,"test-out.png");

    saveSomeData();
  */  
    return EXIT_SUCCESS;
}
