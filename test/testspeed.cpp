#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <iostream>

#define NUM_MULTS 1000000000
#define NUM_DIVS 1000000000

using namespace std;

void float_test() {
  float v = 1.0;
  for (long i = 0; i < NUM_MULTS; i++) {
    v *= 1.0000001;
  }
  cout << v << endl;
  v = 1.0;
  for (long i = 0; i < NUM_DIVS; i++) {
    v /= 3.14;
  }
  cout << v << endl;
}

void double_test() {
  double v = 1.0;
  for (long i = 0; i < NUM_MULTS; i++) {
    v *= 1.0000001;
  }
  cout << v << endl;
  v = 1.0;
  for (long i = 0; i < NUM_DIVS; i++) {
    v /= 3.14;
  }
  cout << v << endl;
}

int main(int argc, char *argv[]) {
  time_t beginTime;

  beginTime = time(NULL);
  float_test();
  printf("Float test took %ld seconds.\n", time(NULL) - beginTime);
  beginTime = time(NULL);
  double_test();
  printf("Double test took %ld seconds.\n", time(NULL) - beginTime);
  return EXIT_SUCCESS;
}
