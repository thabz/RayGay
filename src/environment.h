
#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "scene.h"
#include "parser/objectcollector.h"

class ObjectCollector;
class PreviewWindow;

class Environment {

    public:
        static Environment* getUniqueInstance();

	void setScene(Scene* scene) { this->scene = scene; };
	Scene* getScene() const { return this->scene; };

	ObjectCollector* getObjectCollector() { return this->object_collector; };

	bool hasPreviewWindow() { return this->has_preview_window; }
	void hasPreviewWindow(bool state) { this->has_preview_window = state; }
	PreviewWindow* getPreviewWindow() const { return this->preview_window; };
	void setPreviewWindow(PreviewWindow* w) { preview_window = w; };

    private:
	Scene* scene;
	ObjectCollector* object_collector;
	PreviewWindow* preview_window;
	bool has_preview_window;
	Environment();
	static Environment* unique_instance;
};

inline
Environment::Environment() {
    object_collector = new ObjectCollector();
}

inline
Environment* Environment::getUniqueInstance() {
    if (unique_instance == NULL) {
	unique_instance = new Environment();
    }
    return unique_instance;
}

#endif

