
#include <cassert>
#include <cstdlib>
#include <iostream>

#include "image/rgba.h"

void test_rgba() {
    RGBA c;

    // test += 
    c = RGBA(1.0,2.0,3.0,4.0);
    c += RGBA(1.0,2.0,3.0,4.0);
    assert(c == RGBA(2.0,4.0,6.0,8.0));

    c = RGBA(1.0,2.0,3.0,0.0);
    c += RGBA(1.0,2.0,3.0,1.0);
    assert(c == RGBA(2.0,4.0,6.0,1.0));

    // test / and *
    c = RGBA(1.0,2.0,3.0,4.0);
    c = c * 2.0;
    assert(c == RGBA(2.0,4.0,6.0,8.0));
    c = c / 2.0;
    assert(c == RGBA(1.0,2.0,3.0,4.0));
}

int main(int argc, char *argv[]) {

    test_rgba();
    return EXIT_SUCCESS;
}
