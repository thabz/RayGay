
#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "scene.h"
#include "parser/objectcollector.h"

class ObjectCollector;

class Environment {

    public:
        static Environment* getUniqueInstance();

	void setScene(Scene* scene) { this->scene = scene; };
	Scene* getScene() const { return this->scene; };

	ObjectCollector* getObjectCollector() { return this->object_collector; };

    private:
	Scene* scene;
	ObjectCollector* object_collector;
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

