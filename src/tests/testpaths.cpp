#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>

#include "paths/linesegment.h"
#include "paths/spiral.h"
#include "paths/circle.h"

int main(int argc, char *argv[]) {

    Linesegment::test();
    Circle::test();
    Spiral::test();
    return EXIT_SUCCESS;
}

