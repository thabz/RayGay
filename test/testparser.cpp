
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "parser/parserclass.h"
#include "parser/assignments.h"
#include <cstdlib>
#include <cassert>
#include <iostream>

using namespace std;

#define lookup_float(s) Assignments::getUniqueInstance()->getNamedFloat(s)
#define assert_equal(a,b) assert(IS_EQUAL((a),(b)))

void test_float_ops() {
    Parser p = Parser("scenes/float_ops.gay");
    p.parse();
    p.execute();
    assert_equal(lookup_float("a"),1);
    assert_equal(lookup_float("b"),2);
    cout << lookup_float("d") << endl;
    assert_equal(lookup_float("d"),2);
    cout << lookup_float("c") << endl;
    assert_equal(lookup_float("c"),3);
}

int main(int argc, char *argv[]) {
    test_float_ops();
    return EXIT_SUCCESS;
}
