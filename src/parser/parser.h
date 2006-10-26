
#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include <string>
#include <libguile.h>

class Scene;
class RendererSettings;

class Parser {
    public:
	Parser();
	void assignVariable(std::string var_name, double value);
	void populate(Scene* scene, RendererSettings* renderersettings);
	void parse_file(std::string filename);
	void parse_expr(std::string expr);
	static SCM set_settings(SCM s_settings);

    private:
	SCM lookup(std::string var_name);

};

#endif

