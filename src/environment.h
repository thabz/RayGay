
#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "scene.h"

class Environment {

    public:
        static Environment* getUniqueInstance();

	void setScene(Scene* scene) { this->scene = scene; };
	Scene* getScene() const { return this->scene; };

    private:
	Scene* scene;
	Environment() {};
	static Environment* unique_instance;
};

#endif

