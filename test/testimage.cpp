
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <cstdio>
#include <iostream>

#include "image/rgba.h"
#include "image/image.h"
#include "testing.h"

using namespace std;

class test_rgba : public Test {
    public:
	void run() {
	    RGBA c;

	    // test += 
	    c = RGBA(1.0,2.0,3.0,4.0);
	    c += RGBA(1.0,2.0,3.0,4.0);
	    assertTrue(c == RGBA(2.0,4.0,6.0,8.0));
	    assertTrue(c.a() == 8.0);

	    c = RGBA(1.0,2.0,3.0,0.0);
	    c += RGBA(1.0,2.0,3.0,1.0);
	    assertTrue(c == RGBA(2.0,4.0,6.0,1.0));
	    assertTrue(c.a() == 1.0);

	    // test / and *
	    c = RGBA(1.0,2.0,3.0,4.0);
	    c = c * 2.0;
	    assertTrue(c == RGBA(2.0,4.0,6.0,8.0));
	    c = c / 2.0;
	    assertTrue(c == RGBA(1.0,2.0,3.0,4.0));
	}
};

class test_png : public Test {
    public:
	void run() {
	    // Test basic save and load
	    RGB color = RGB(1.0,0.0,1.0);

	    Image* img = new Image(10,20);
	    img->setRGBA(5,15,color);
	    img->save(getLoadPrefix() + "/test.png");
	    Image* img2 = Image::load(getLoadPrefix() + "/test.png");
	    assertTrue(img2->getWidth() == 10);
	    assertTrue(img2->getHeight() == 20);
	    //cout << RGB(img2->getRGBA(5,15)) << endl;
	    assertTrue(RGB(img2->getRGBA(5,15)) == color);
	    remove((getLoadPrefix() + "/test.png").c_str());
	    delete img;
	    delete img2;

	    // Load 24 bit png 
	    img = new Image(getLoadPrefix() + "/gfx/rgb.png");
	    assertTrue(img->getWidth() == 10);
	    assertTrue(img->getHeight() == 10);
	    cout << "Color: " << img->getRGBA(0,0) << endl;
	    assertTrue(img->getRGBA(0,0) == RGBA(1.0,0,0,1.0));
	    delete img;

	    // Test load of png with palette
	    img = new Image(getLoadPrefix() + "/gfx/withpalette.png");
	    assertTrue(img->getWidth() == 10);
	    assertTrue(img->getHeight() == 10);
	    assertTrue(img->getRGBA(0,0) == RGBA(1.0,0,0,1.0));
	    delete img;
	}
};

class test_tga : public Test {
    public: 
	void run() {
	    RGB color = RGB(1.0,0.0,1.0);

	    Image* img = new Image(10,20);
	    img->setRGBA(5,15,color);
	    img->save(getLoadPrefix() + "/test.tga");
	    Image* img2 = Image::load(getLoadPrefix() + "/test.tga");
	    assertTrue(img2->getWidth() == 10);
	    assertTrue(img2->getHeight() == 20);
	    assertTrue(RGB(img2->getRGBA(5,15)) == color);
	    remove((getLoadPrefix() + "/test.tga").c_str());
	    delete img;
	    delete img2;
	}
};

class test_jpg : public Test {
    public:
	void run() {
	    Image* img = new Image(getLoadPrefix() + "/gfx/simple.jpg");
	    assertTrue(img->getWidth() == 10);
	    assertTrue(img->getHeight() == 10);
	    delete img;
	}
};


int main(int argc, char *argv[]) {
    TestSuite suite;
    suite.add("RGBA",new test_rgba());
    suite.add("TGA",new test_tga());
#ifdef HAVE_PNG_H
    suite.add("PNG",new test_png());
#endif    
#ifdef HAVE_JPEGLIB_H
    suite.add("JPEG",new test_jpg());
#endif    
    suite.run();
    suite.printStatus();

    if (suite.hasFailures()) {
	return EXIT_FAILURE;
    } else {
	return EXIT_SUCCESS;
    }
}
