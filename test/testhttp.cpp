#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "http/md5.h"
#include <cstdio>
#include <cstring>
#include <cstdlib>

bool test_md5_stream_hex() {
  FILE *input = tmpfile();
  if (input == NULL) {
    return false;
  }

  char md5_hex[34];
  memset(md5_hex, 'X', sizeof(md5_hex));
  md5_stream_hex(input, md5_hex);
  fclose(input);

  return strcmp(md5_hex, "d41d8cd98f00b204e9800998ecf8427e") == 0 &&
         strlen(md5_hex) == 32 && md5_hex[32] == '\0' && md5_hex[33] == 'X';
}

int main(int argc, char *argv[]) {
  return test_md5_stream_hex() ? EXIT_SUCCESS : EXIT_FAILURE;
}
