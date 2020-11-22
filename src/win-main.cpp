
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <time.h>
#include <unistd.h>
#include <vector>

#include "exception.h"
#include "image/image.h"
#include <ApplicationServices/ApplicationServices.h>
#include <Carbon/Carbon.h>

using namespace std;

int main(int argc, char *argv[]) {
  /* Carbon deprecated in Mac OS X 10.6 */
  /*
 CFStringRef title = CFSTR("RayGay");

  WindowRef window;
  WindowGroupRef windowGroup;
  MenuRef menu;
  Rect rect = {40, 40, 400, 400};
  CreateNewWindow(kDocumentWindowClass,
 kWindowStandardDocumentAttributes|kWindowInWindowMenuAttribute, &rect,
 &window); SetWindowAlternateTitle(window, title);
  SetWindowTitleWithCFString(window, title);

  CreateWindowGroup(0, &windowGroup);
  SetWindowGroup(window, windowGroup);

  MenuRef root_menu = AcquireRootMenu();
  ClearMenuBar();
  CreateStandardWindowMenu(kWindowMenuIncludeRotate,&menu);
  InsertMenu(menu,0);
  DrawMenuBar();

  ShowWindow(window);
  sleep(100);
  return EXIT_SUCCESS;
  */
}
