
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <cstdio>
#include <iostream>

#include "image/imageimpl.h"
#include "image/texture.h"
#include "image/simpletexture.h"
#include "testing.h"

using namespace std;

#define IS_SORTA_ZERO(p) (fabs(p) < 0.1)
#define IS_SORTA_EQUAL(a,b) (IS_SORTA_ZERO((a) - (b)))

#if 0
#define assertEqualColor(n,m) assertTrue((n - m).brightness() < 0.1)
#else
#define assertEqualColor(x,y) assertTrue(IS_SORTA_EQUAL((x).r(),(y).r()) && IS_SORTA_EQUAL((x).g(),(y).g()) && IS_SORTA_EQUAL((x).b(),(y).b()))
#endif

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

	    c = RGB(1.0,2.0,3.0);
	    assertTrue(c.a() == 1);

	    assertTrue(RGB(1.0,2.0,3.0) == RGBA(1.0,2.0,3.0,1.0));
	    assertTrue(RGBA(1.0,2.0,3.0,1.0) == RGB(1.0,2.0,3.0));
	}
};

class test_image : public Test {
    public:
	void run() {
	    RGBA r = RGBA(1,0,0,0);
	    RGBA g = RGBA(0,1,0,0);
	    RGBA b = RGBA(0,0,1,0);
	    
	    Image* img = new ImageImpl<double,4>(128,256);
	    
	    img->setRGBA(10,10,r);
	    assertTrue(img->getRGBA(10,10) == r);

	    img->setRGBA(20,20,b);
	    assertTrue(img->getRGBA(20,20) == b);

	    img->setRGBA(127,128,g);
	    assertTrue(img->getRGBA(127,128) == g);

	    img->setRGBA(127,255,g);
	    assertTrue(img->getRGBA(127,255) == g);
	    
	    img->clear(r);
	    assertTrue(img->getRGBA(127,128) == r);
	    assertTrue(img->getRGBA(127,255) == r);
	    assertTrue(img->getRGBA(10,10) == r);
	    assertTrue(img->getRGBA(20,20) == r);
	    delete img;
	    
        }

};

class test_image_mmap : public Test {
    public:
	void run() {
	    RGBA r = RGBA(1,0,0,0);
	    RGBA g = RGBA(0,1,0,0);
	    RGBA b = RGBA(0,0,1,0);
	    
	    Image* img = new ImageImpl<double,4>(128,256,Allocator::MMAP_ONLY);
	    
	    img->setRGBA(10,10,r);
	    assertTrue(img->getRGBA(10,10) == r);

	    img->setRGBA(20,20,b);
	    assertTrue(img->getRGBA(20,20) == b);

	    img->setRGBA(127,128,g);
	    assertTrue(img->getRGBA(127,128) == g);

	    img->setRGBA(127,255,g);
	    assertTrue(img->getRGBA(127,255) == g);
	    
	    delete img;
        }

};

class test_texture : public Test {
    public:
	void run() {
	    RGBA col = RGBA(1,0,0,1);
    	    Image* img = new ImageImpl<double,4>(32,32);
    	    img->clear(col);
    	    Texture* tex = new SimpleTexture(img, Vector2(1,1), Texture::INTERPOLATION_BICUBIC);
    	    for(double x = 0; x < 1.5; x += 1.0 / 128) {
                for(double y = 0; y < 1.5; y += 1.0 / 128) {
                    RGBA c = tex->getTexel(x,y);        
                    assertTrue(c == col);
                }   	            
    	    }


	    // Test with texture backed by a 2x2 pixel image
	    RGBA blue = RGBA(0,0,0.5,1);
	    RGBA red = RGBA(1,0,0,1);
    	    img = new ImageImpl<double,4>(2,2);
    	    img->clear(blue);
	    img->setRGBA(1,1,red);
	    img->setRGBA(0,0,red);
    	    tex = new SimpleTexture(img, Vector2(1,1), Texture::INTERPOLATION_NONE);
	    assertEqualColor(tex->getTexel(0.1,0.1), red);
	    assertEqualColor(tex->getTexel(0.9,0.9), red);
	    assertEqualColor(tex->getTexel(0.1,0.9), blue);
	    assertEqualColor(tex->getTexel(0.9,0.1), blue);
        }
};

class test_png : public Test {
    public:
	void run() {
	    // Test basic save and load
	    RGBA color = RGBA(1.0,1.0,0.0,128.0/255.0);

	    Image* img = new ImageImpl<double,4>(10,20);
	    img->setRGBA(5,15,color);
	    img->setRGBA(0,0,color);
	    assertTrue(img->getRGBA(5,15) == color);
	    assertTrue(img->getRGBA(0,0) == color);
	    img->save(getLoadPrefix() + "/test.png");
	    Image* img2 = Image::load(getLoadPrefix() + "/test.png");
	    assertTrue(img2->getWidth() == 10);
	    assertTrue(img2->getHeight() == 20);
	    //cout << img2->getRGBA(5,15) << endl;
	    //cout << color << endl;
	    assertEqualColor(img2->getRGBA(5,15), color);
	    assertEqualColor(img2->getRGBA(0,0), color);
	    ::remove((getLoadPrefix() + "/test.png").c_str());
	    delete img;
	    delete img2;

	    // Load 24 bit png 
	    img = Image::load(getLoadPrefix() + "/gfx/rgb.png");
	    img->save(getLoadPrefix() + "/rgb-kaj.png");
	    assertTrue(img->getWidth() == 10);
	    assertTrue(img->getHeight() == 10);
	    //cout << endl <<  img->getRGBA(0,0) << endl;
	    assertEqualColor(img->getRGBA(0,0), RGB(1.0,0,0));
	    assertEqualColor(img->getRGBA(9,9), RGB(1.0,0,0));
	    delete img;
	    ::remove((getLoadPrefix() + "/rgb-kaj.png").c_str());

	    // Test load of png with palette
	    img = Image::load(getLoadPrefix() + "/gfx/withpalette.png");
	    assertTrue(img->getWidth() == 10);
	    assertTrue(img->getHeight() == 10);
	    assertEqualColor(img->getRGBA(1,1), RGB(1.0,1.0,1.0));
	    assertEqualColor(img->getRGBA(0,0), RGB(1.0,0.0,0.0));
	    delete img;
	    
	    img = Image::load(getLoadPrefix() + "/gfx/16x12x3.png");
	    assertTrue(img->getWidth() == 16);
	    assertTrue(img->getHeight() == 12);
	    assertTrue(img->cpp() == 3);
	    delete img;

	    img = Image::load(getLoadPrefix() + "/gfx/16x12x1.png");
	    assertTrue(img->getWidth() == 16);
	    assertTrue(img->getHeight() == 12);
	    assertTrue(img->cpp() == 1);
	    delete img;
	}
};

class test_tga : public Test {
    public: 
	void run() {
	    RGB color = RGB(1.0,0.0,1.0);

	    Image* img = new ImageImpl<double,4>(10,20);
	    img->setRGBA(5,15,color);
	    img->setRGBA(9,19,color);
	    img->save(getLoadPrefix() + "/test.tga");
	    Image* img2 = Image::load(getLoadPrefix() + "/test.tga");
	    assertTrue(img2 != NULL);
	    assertTrue(img2->getWidth() == 10);
	    assertTrue(img2->getHeight() == 20);
	    assertTrue(RGB(img2->getRGBA(5,15)) == color);
	    remove((getLoadPrefix() + "/test.tga").c_str());
	    delete img;
	    delete img2;

	    img = Image::load(getLoadPrefix() + "/gfx/16x12x3.tga");
	    assertTrue(img->getWidth() == 16);
	    assertTrue(img->getHeight() == 12);
	    assertTrue(img->cpp() == 3);
	    delete img;

	    img = Image::load(getLoadPrefix() + "/gfx/16x12x1.tga");
	    assertTrue(img->getWidth() == 16);
	    assertTrue(img->getHeight() == 12);
	    assertTrue(img->cpp() == 1);
	    delete img;
	}
};

class test_jpg : public Test {
    public:
	void run() {
	    Image* img = Image::load(getLoadPrefix() + "/gfx/simple.jpg");
	    assertTrue(img->getWidth() == 10);
	    assertTrue(img->getHeight() == 10);
	    delete img;

	    img = Image::load(getLoadPrefix() + "/gfx/16x12x3.jpg");
	    assertTrue(img->getWidth() == 16);
	    assertTrue(img->getHeight() == 12);
	    assertTrue(img->cpp() == 3);
	    delete img;

	    img = Image::load(getLoadPrefix() + "/gfx/16x12x1.jpg");
	    assertTrue(img->getWidth() == 16);
	    assertTrue(img->getHeight() == 12);
	    assertTrue(img->cpp() == 1);
	    delete img;
	}
};


int main(int argc, char *argv[]) {
    TestSuite suite;
    suite.add("RGBA",new test_rgba());
    suite.add("Image",new test_image());
    suite.add("Image mmap'ed",new test_image_mmap());
    suite.add("Texture",new test_texture());
    suite.add("TGA",new test_tga());
    if (Image::supportsFormat(".png")) {
        suite.add("PNG",new test_png());
    }
    if (Image::supportsFormat(".jpg")) {
        suite.add("JPEG",new test_jpg());
    }
    suite.run();
    suite.printStatus();

    if (suite.hasFailures()) {
	return EXIT_FAILURE;
    } else {
	return EXIT_SUCCESS;
    }
}
