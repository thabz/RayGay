
#ifndef PARSER_INTERPRETER_ENV_H
#define PARSER_INTERPRETER_ENV_H

#include "scene.h"

class InterpreterEnv {

    public:
        static InterpreterEnv* getUniqueInstance();

	void setScene(Scene* scene) { this->scene = scene; };
	Scene* getScene() const { return this->scene; };

    private:
	Scene* scene;
	InterpreterEnv() {};
	static InterpreterEnv* unique_instance;
};

#endif
