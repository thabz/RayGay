
#include "scheme.h"
#include <iostream>
#include <sstream>

using namespace std;

int main(int argc, char *argv[]) {
    Scheme* scheme = new Scheme();
    SchemeObject result = scheme->eval("(+ 1 (* 2 3))");
}
