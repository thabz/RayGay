
#include "image/jpeg.h"
#include "image/image.h"
#include <cassert>
#include <iostream>
#include <string>
#include <cstdio>
#include <memory.h>
#include <setjmp.h>
#include <png.h>

using namespace std;

void PngIO::save(const Image* const image, const std::string& filename) const {
    cout << "PNG saving not implemented " << __FILE__ << __LINE__ << endl;
    return;
}

Image* PngIO::load(const std::string& filename) {
    cout << "PNG loading not implemented " << __FILE__ << __LINE__ << endl;
    return NULL;
}

