#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <cassert>
#include <iostream>
#include "stats.h"

void test_stats() {
    Stats stats = Stats();
    // Test Stats::put and get
    stats.put("Key 1",10);
    stats.put("Key 2",20);
    stats.put("Key 3",30);
    assert(stats.get("Key 1") == 10);
    assert(stats.get("Key 2") == 20);
    assert(stats.get("Key 3") == 30);
    stats.inc("Key 1");
    assert(stats.get("Key 1") == 11);

    assert(stats.get("Unknown key") == 0);

    // Test Stats::inc
    stats.inc("Key 4");
    assert(stats.get("Key 4") == 1);
    stats.inc("Key 4");
    assert(stats.get("Key 4") == 2);
    stats.dump();
}

int main(int argc, char *argv[]) {
    test_stats();
    return EXIT_SUCCESS;
}



