#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "stats.h"
#include <cassert>
#include <iostream>
#include <stdlib.h>

void test_stats() {
  Stats *stats = Stats::getUniqueInstance();
  stats->clear();
  // Test Stats::put and get
  stats->put((StatsKey)1, 10);
  stats->put((StatsKey)2, 20);
  stats->put((StatsKey)3, 30);
  assert(stats->get((StatsKey)1) == 10);
  assert(stats->get((StatsKey)2) == 20);
  assert(stats->get((StatsKey)3) == 30);
  stats->inc((StatsKey)1);
  assert(stats->get((StatsKey)1) == 11);

  assert(stats->get((StatsKey)4) == 0);

  // Test Stats::inc
  stats->inc((StatsKey)4);
  assert(stats->get((StatsKey)4) == 1);
  stats->inc((StatsKey)4);
  assert(stats->get((StatsKey)4) == 2);
  // stats->dump();
}

void test_stats2() {
  // Test singleton pattern
  Stats *stats = Stats::getUniqueInstance();
  assert(stats->get((StatsKey)4) == 2);

  // Test clear()
  stats->clear();
  assert(stats->get((StatsKey)4) == 0);
}

int main(int argc, char *argv[]) {
  test_stats();
  test_stats2();
  return EXIT_SUCCESS;
}
