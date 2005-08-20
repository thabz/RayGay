
#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include <string>
#include <libguile.h>

class Scene;
class RendererSettings;

class Parser {
    public:
	Parser(std::string filename);
	void assignVariable(std::string var_name, double value);
	void populate(Scene* scene, RendererSettings* renderersettings);
	void run();

    private:
	std::string filename;
	SCM lookup(std::string var_name);

};

#endif

