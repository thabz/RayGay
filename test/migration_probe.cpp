// Read-only scene inspection for migration integration tests.
#include "parser/sceneparser.h"
#include "scheme/filenames.h"
#include "scene.h"
#include "renderersettings.h"
#include "cameras/camera.h"
#include "objects/object.h"
#include "materials/material.h"
#include "aabox.h"
#include <iomanip>
#include <cstdlib>
#include <iostream>
#include <unistd.h>
int main(int argc, char **argv) {
  if (argc != 2) return 2;
  srand(1);
  Scene scene;
  SceneParser parser(&scene);
  std::string path(argv[1]);
  auto slash = path.find_last_of('/');
  if (slash != std::string::npos) {
    if (chdir(path.substr(0, slash).c_str())) return 2;
    path = path.substr(slash + 1);
  }
  parser.parse_file(SchemeFilenames::toString(path));
  auto settings = RendererSettings::uniqueInstance();
  parser.populate(&scene, settings);
  std::cout << std::setprecision(15);
  std::cout << "MIGRATION {\"objects\":" << scene.getObjects().size()
            << ",\"lights\":" << scene.getLightsources().size()
            << ",\"width\":" << settings->image_width
            << ",\"height\":" << settings->image_height;
  if (scene.getCamera()) {
    Vector p = scene.getCamera()->getPosition();
    std::cout << ",\"camera\":[" << p[0] << ',' << p[1] << ',' << p[2] << ']';
  }
  std::cout << ",\"bounds\":[";
  bool first = true;
  int emitted = 0;
  for (auto entry : scene.getObjects()) {
    auto object = dynamic_cast<Object *>(entry);
    if (!object || emitted++ >= 16) continue;
    if (!first) std::cout << ',';
    first = false;
    AABox box = object->getBoundingBox();
    std::cout << '[';
    for (int i = 0; i < 3; ++i) std::cout << (i ? "," : "") << box.minimum(i);
    for (int i = 0; i < 3; ++i) std::cout << ',' << box.maximum(i);
    std::cout << ']';
  }
  std::cout << "]";
  if (!scene.getObjects().empty()) {
    auto object = dynamic_cast<Object *>(scene.getObjects()[0]);
    if (object && object->getMaterial()) {
      const auto m = object->getMaterial();
      std::cout << ",\"first_material\":[" << m->getKd() << ',' << m->getKs()
                << ',' << m->getSc() << ',' << m->getKt() << ',' << m->getEta() << ']';
    }
  }
  std::cout << "}" << std::endl;
}
