/*
 * This is a test of threads
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

extern "C" {
#include <pthread.h>
}
#include <cstdio>
#include <cstdlib>
#include <iostream>

using namespace std;

struct threadargs {
  int a;
  int b;
};

void *threadDo(void *input) {
  threadargs *myarg = static_cast<threadargs *>(input);
  int a = myarg->a;
  int b = myarg->b;
  if (a == b) {
    return NULL;
  } else {
    return NULL;
  }
}

void test_threads() {
  int threads_num = 10;

  pthread_t threads[threads_num];
  threadargs myargs[threads_num]; // arguments for threadDo

  for (int i = 0; i < threads_num; i++) {
    myargs[i].a = i;
    myargs[i].b = i * 2;

    // Spawn thread that calls threadDo with myargs[i]
    pthread_create(&threads[i], NULL, threadDo, (void *)&myargs[i]);
  }

  // wait for threads to finish
  for (int i = 0; i < threads_num; i++)
    pthread_join(threads[i], NULL);
}

int main(int argc, char *argv[]) {
  test_threads();
  return EXIT_SUCCESS;
}
