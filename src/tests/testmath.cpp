
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>

#include "math/vector.h"
#include "math/matrix.h"

int main(int argc, char *argv[]) {
    Vector::test();
    Matrix::test();
    return EXIT_SUCCESS;
}
