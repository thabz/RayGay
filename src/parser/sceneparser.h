
#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include <string>

#include "scheme/filenames.h"
#include "scheme/scheme.h"

class Scene;
class RendererSettings;

class SceneParser {
public:
  SceneParser(Scene *scene);
  void assignVariable(std::wstring var_name, double value);
  SchemeObject *lookup(std::wstring var_name);
  void populate(Scene *scene, RendererSettings *renderersettings);
  void parse_file(std::wstring filename);
  void parse_expr(std::wstring expr);
  static SchemeObject *set_settings(Scheme *, SchemeObject *s_settings);
  static SchemeObject *add_to_scene(Scheme *, SchemeObject *s_object);
  std::string version();

private:
  Scheme *scheme;
  static Scene *scene;
};

#endif
