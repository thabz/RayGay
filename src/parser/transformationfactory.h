
#ifndef PARSER_TRANSFORMATION_FACTORY_H
#define PARSER_TRANSFORMATION_FACTORY_H

#include <libguile.h>

class Matrix;

class TransformationFactory {

    public:
	static SCM rotate(SCM obj, SCM axis, SCM angle);
	static SCM translate(SCM obj, SCM translation);
	static SCM scale(SCM obj, SCM scale);

	static void register_procs();

    private:
	static SCM transform(SCM obj, const Matrix& m, char* subr);
};

#endif
