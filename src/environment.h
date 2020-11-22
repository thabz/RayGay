
#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "scene.h"

class ObjectCollector;
class PreviewWindow;
class FilterStack;

class Environment {

public:
  static Environment *getUniqueInstance();

  void setScene(Scene *scene) { this->scene = scene; };
  Scene *getScene() const { return this->scene; };

  void setFilterStack(FilterStack *f) { this->filter_stack = f; };
  FilterStack *getFilterStack() const { return this->filter_stack; };

  bool hasPreviewWindow() { return this->has_preview_window; }
  void hasPreviewWindow(bool state) { this->has_preview_window = state; }
  PreviewWindow *getPreviewWindow() const { return this->preview_window; };
  void setPreviewWindow(PreviewWindow *w) { preview_window = w; };

  bool isVerbose() const { return verbose; };
  void isVerbose(bool v) { this->verbose = v; };

  bool isProfilingEnabled() const { return profiling_enabled; };
  void isProfilingEnabled(bool v) { this->profiling_enabled = v; };

private:
  Scene *scene;
  FilterStack *filter_stack;
  PreviewWindow *preview_window;
  bool has_preview_window;
  bool verbose;
  bool profiling_enabled;
  Environment();
  static Environment *unique_instance;
};

inline Environment::Environment() {
  verbose = false;
  filter_stack = NULL;
  profiling_enabled = false;
}

inline Environment *Environment::getUniqueInstance() {
  if (unique_instance == NULL) {
    unique_instance = new Environment();
  }
  return unique_instance;
}

#endif
