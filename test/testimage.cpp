
#include <cassert>
#include <cstdlib>
#include <cstdio>
#include <iostream>

#include "image/rgba.h"
#include "image/image.h"

using namespace std;

void test_rgba() {
    RGBA c;

    // test += 
    c = RGBA(1.0,2.0,3.0,4.0);
    c += RGBA(1.0,2.0,3.0,4.0);
    assert(c == RGBA(2.0,4.0,6.0,8.0));
    assert(c.a() == 8.0);

    c = RGBA(1.0,2.0,3.0,0.0);
    c += RGBA(1.0,2.0,3.0,1.0);
    assert(c == RGBA(2.0,4.0,6.0,1.0));
    assert(c.a() == 1.0);

    // test / and *
    c = RGBA(1.0,2.0,3.0,4.0);
    c = c * 2.0;
    assert(c == RGBA(2.0,4.0,6.0,8.0));
    c = c / 2.0;
    assert(c == RGBA(1.0,2.0,3.0,4.0));
}

void test_png() {
    RGB color = RGB(1.0,0.0,1.0);

    Image* img = new Image(10,20);
    img->setRGBA(5,15,color);
    img->save("test.png");
    Image* img2 = Image::load("test.png");
    assert(img2->getWidth() == 10);
    assert(img2->getHeight() == 20);
    //cout << RGB(img2->getRGBA(5,15)) << endl;
    assert(RGB(img2->getRGBA(5,15)) == color);
    remove("test.png");
    delete img;
    delete img2;
}

void test_tga() {
    RGB color = RGB(1.0,0.0,1.0);

    Image* img = new Image(10,20);
    img->setRGBA(5,15,color);
    img->save("test.tga");
    Image* img2 = Image::load("test.tga");
    assert(img2->getWidth() == 10);
    assert(img2->getHeight() == 20);
    assert(RGB(img2->getRGBA(5,15)) == color);
    remove("test.tga");
    delete img;
    delete img2;
}

int main(int argc, char *argv[]) {

    test_rgba();
    test_tga();
    test_png();
    return EXIT_SUCCESS;
}
