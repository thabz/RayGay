
#include "image/rgba.h"
#include <iostream>

std::ostream & operator<<(std::ostream &os, const RGBA &x) {
    os << '(' << x[0] << ',';
    os << x[1] << ',';
    os << x[2] << ',';
    os << x[3] << ')';
    return os;
}

