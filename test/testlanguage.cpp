/*
 * This is a scratchpad for testing different C++ features...
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <cassert>
#include <iostream>

void test_bool() {
    bool t1 = true;
    bool t2 = true;
    bool f1 = false;
    bool f2 = false;

    assert(t1 && t2 == true);
    assert(t1 && t2);
    assert(t1 || f1);
    assert(f1 || t1);
    assert(f1 || f2 == false);


}

int main(int argc, char *argv[]) {

    test_bool();
    return EXIT_SUCCESS;
}


