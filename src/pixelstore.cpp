
#include "pixelstore.h"
#include "image/rgb.h"
#include "pixel.h"
#include <string.h>
#include <cassert>
#include <iostream>
#include <math.h>

/**
 * @param width Width
 * @param height Height
 * @param d Max depth of subdivision
 */
PixelStore::PixelStore(int width, int height, int depth) {
    this->depth = depth;
    this->stride = 1 << depth;
    this->width = width;
    store = new RGB[width * stride * stride * 3];
    this->topRow = 0.0;
}

PixelStore::~PixelStore() {
    delete [] store;
}

void PixelStore::switchRow(double y) {
    assert(y == topRow + 1);
    int size = width * stride * stride;
    for(int i = 0; i < size; i++) {
	store[i] = store[size+i];
    }
    topRow = y;
}

int PixelStore::getIndex(double x, double y) const {
    int xi = int(x * stride);
    int yi = int((y - topRow) * stride * stride * width);
    return xi + yi;
}

void PixelStore::setColor(double x, double y, const RGB color) {
    store[getIndex(x,y)] = color;
}

RGB PixelStore::getColor(double x, double y) const {
    return store[getIndex(x,y)];
}

Pixel PixelStore::getPixel(double x, double y, int depth) const {
    assert(depth > 0);
    double off = 1.0 / double(1 << depth);
    return Pixel(getColor(x - off, y - off),
                 getColor(x + off, y - off),
                 getColor(x - off, y + off),
                 getColor(x + off, y + off));
}

void PixelStore::test() {
    PixelStore p = PixelStore(1000,1000,4);

    // Test set- and get-Color
    RGB color = RGB(0.1,0.2,0.3);
    p.setColor(0.5,0.5,color);
    assert(p.getColor(0.5,0.5) == color);
    p.setColor(5.25,1.5,color);
    assert(p.getColor(5.25,1.5) == color);
    
    // Test at depth 1
    p.setColor(0.0,0.0,RGB(0.1,0.1,0.1));
    p.setColor(0.0,1.0,RGB(0.2,0.2,0.2));
    p.setColor(1.0,0.0,RGB(0.3,0.3,0.3));
    p.setColor(1.0,1.0,RGB(0.4,0.4,0.4));
    assert(p.getPixel(0.5,0.5,1).getAverage() == RGB(0.25,0.25,0.25));

    // Test at depth 2 with an offset
    p.setColor(0.5,0.0,RGB(0.1,0.1,0.1));
    p.setColor(1.0,0.0,RGB(0.2,0.2,0.2));
    p.setColor(0.5,0.5,RGB(0.3,0.3,0.3));
    p.setColor(1.0,0.5,RGB(0.4,0.4,0.4));
    assert(p.getPixel(0.75,0.25,2).getAverage() == RGB(0.25,0.25,0.25));

    // Test switchRow
    p.setColor(0.0,1.0,RGB(0.1,0.1,0.1));
    p.setColor(1.0,1.0,RGB(0.2,0.2,0.2));
    assert(p.getColor(0.0,1.0) == RGB(0.1,0.1,0.1));
    p.switchRow(1.0);
    assert(p.getColor(0.0,1.0) == RGB(0.1,0.1,0.1));
    p.setColor(0.0,2.0,RGB(0.3,0.3,0.3));
    p.setColor(1.0,2.0,RGB(0.4,0.4,0.4));
    assert(p.getPixel(0.5,1.5,1).getAverage() == RGB(0.25,0.25,0.25));
    p.switchRow(2.0);
    assert(p.getColor(1.0,2.0) == RGB(0.4,0.4,0.4));

    std::cout << "PixelStore::test() done." << std::endl;
}
